//
// Copyright (C) 2025-2026  Divya Ranjan Pattanaik
// Copyright (C) 2025-2026  Tushar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "render-core.h"
#include "elisp-helpers.h"
#include "emacs-module.h"
#include "mupdf-helpers.h"
#include "render-theme.h"
#include "render-threads.h"

int plugin_is_GPL_compatible;

pthread_mutex_t g_mupdf_mutex[FZ_LOCK_MAX];

/**
 * Load and cache the display list for a page.
 *
 * If the page is empty, sets its status to rendering, loads the page,
 * computes its bounding box, and generates a display list. Uses a
 * cloned MuPDF context for thread safety.
 *
 */

int
load_page_dl(DocState *state, DocPage *dp)
{
	if (!state || !dp)
		return EXIT_FAILURE;
	if (dp->ready)
		return EXIT_SUCCESS;

	pthread_mutex_lock(&dp->mutex);

	/* Another thread may have loaded it while we waited */
	if (dp->ready)
		{
			pthread_mutex_unlock(&dp->mutex);
			return EXIT_SUCCESS;
		}

	fz_context *ctx = fz_clone_context(state->ctx);
	if (!ctx)
		{
			pthread_mutex_unlock(&dp->mutex);
			return EXIT_FAILURE;
		}

	fz_page *loaded_page = NULL;
	bool success = false;

	fz_try(ctx)
		{
			loaded_page = fz_load_page(ctx, state->doc, dp->page_num);
			dp->bbox = fz_bound_page(ctx, loaded_page);
			dp->display_list =
				fz_new_display_list_from_page(ctx, loaded_page);
			dp->ready = true;
			success = true;
		}
	fz_always(ctx)
		{
			if (loaded_page)
				fz_drop_page(ctx, loaded_page);
		}
	fz_catch(ctx)
		{
			success = false;
		}

	fz_drop_context(ctx);
	pthread_mutex_unlock(&dp->mutex);

	return success ? EXIT_SUCCESS : EXIT_FAILURE;
}

/**
 * Thread function to render a page and encode it as a PNM image.
 *
 * Uses the page's display list to generate a pixmap, optionally inverts
 * and gamma-corrects it, then encodes it to PNM and stores it in
 * `cp->img_data'. Updates image dimensions and status, and signals
 * completion via a condition variable.
 *
 * @param arg  Pointer to DrawThreadArgs (contains DocState and CachedPage).
 * Return:     NULL (result is stored in CachedPage).
 */

void *
draw_page_thread(void *arg)
{
	DrawThreadArgs *args = (DrawThreadArgs *)arg;
	if (!args)
		return NULL;

	DocState *doc_state = args->doc_state;
	EmacsWinState *win_state = args->win_state;
	CachedPage *cp = args->cp;
	DocPage *dp = args->dp;

	if (!doc_state || !win_state || !cp || !dp)
		{
			free(args);
			return NULL;
		}

	 /* If the document is being reloaded/closed, or the window is closing, */
	 /* abort this job safely. */
	if (doc_state->is_reloading || win_state->closing)
		{
			mark_cp_error_if_rendering(cp);
			release_doc_page(doc_state, dp);

			atomic_fetch_sub(&doc_state->active_jobs, 1);
			atomic_fetch_sub(&win_state->active_jobs, 1);

			free(args);
			return NULL;
		}

	// If the page isn't ready then parse/load it
	if (!dp->ready)
		load_page_dl(doc_state, dp);
	if (!dp->ready || !dp->display_list)
		{
			mark_cp_error_if_rendering(cp);
			release_doc_page(doc_state, dp);
			atomic_fetch_sub(&doc_state->active_jobs, 1);
			atomic_fetch_sub(&win_state->active_jobs, 1);
			free(args);
			return NULL;
		}

	fz_context *ctx = fz_clone_context(doc_state->ctx);
	if (!ctx)
		{
			mark_cp_error_if_rendering(cp);
			release_doc_page(doc_state, dp);
			atomic_fetch_sub(&doc_state->active_jobs, 1);
			atomic_fetch_sub(&win_state->active_jobs, 1);
			free(args);
			return NULL;
		}

	fz_matrix ctm = fz_transform_page(dp->bbox,
					  win_state->resolution,
					  win_state->rotate);
	fz_pixmap *pixmap = NULL;
	fz_buffer *buf = NULL;
	fz_output *out = NULL;
	int local_imgw = 0;
	int local_imgh = 0;

	bool render_ok = false;
	float frame_scale = doc_state->frame_scale;

	if (frame_scale <= 0.0f)
		frame_scale = 1.0f;

	fz_try(ctx)
		{
			pixmap = fz_new_pixmap_from_display_list(ctx, dp->display_list,
								 ctm, fz_device_rgb(ctx), 0);

			if (doc_state->invert)
				{
					fz_invert_pixmap_luminance(ctx, pixmap);
					fz_gamma_pixmap(ctx, pixmap, 1 / 1.4f);
				}

			local_imgh = (int)(fz_pixmap_height(ctx, pixmap) / frame_scale);
			local_imgw = (int)(fz_pixmap_width(ctx, pixmap) / frame_scale);
			buf = fz_new_buffer(ctx, 1024);
			out = fz_new_output_with_buffer(ctx, buf);
			fz_write_pixmap_as_pnm(ctx, out, pixmap);
			render_ok = true;
		}
	fz_always(ctx)
		{
			if (pixmap)
				fz_drop_pixmap(ctx, pixmap);

			if (out)
				{
					fz_close_output(ctx, out);
					fz_drop_output(ctx, out);
				}
		}
	fz_catch(ctx)
		{
			render_ok = false;
		}

	if (!render_ok || !buf || local_imgw <= 0 || local_imgh <= 0)
		{
			if (buf)
				fz_drop_buffer(ctx, buf);

			fz_drop_context(ctx);
			mark_cp_error_if_rendering(cp);
			release_doc_page(doc_state, dp);
			atomic_fetch_sub(&doc_state->active_jobs, 1);
			atomic_fetch_sub(&win_state->active_jobs, 1);
			free(args);
			return NULL;
		}

	size_t local_img_size = buf->len;
	char *local_img_data = malloc(local_img_size);

	if (!local_img_data)
		{
			fz_drop_buffer(ctx, buf);
			fz_drop_context(ctx);
			mark_cp_error_if_rendering(cp);
			release_doc_page(doc_state, dp);
			atomic_fetch_sub(&doc_state->active_jobs, 1);
			atomic_fetch_sub(&win_state->active_jobs, 1);
			free(args);
			return NULL;
		}

	memcpy(local_img_data, buf->data, local_img_size);
	fz_drop_buffer(ctx, buf);
	fz_drop_context(ctx);

	/* If the slot was invalidated while we were rendering, its status will */
	/* no longer be PAGE_STATUS_RENDERING. In that case, discard the result. */

	pthread_mutex_lock(&cp->mutex);
	if (cp->status == PAGE_STATUS_RENDERING)
		{
			free(cp->img_data);
			cp->img_data = local_img_data;
			cp->img_size = local_img_size;
			cp->imgw = local_imgw;
			cp->imgh = local_imgh;
			cp->status = PAGE_STATUS_READY;
			pthread_cond_signal(&cp->cond);
		}
	else
		free(local_img_data);
	pthread_mutex_unlock(&cp->mutex);

	release_doc_page(doc_state, dp);
	atomic_fetch_sub(&doc_state->active_jobs, 1);
	atomic_fetch_sub(&win_state->active_jobs, 1);

	free(args);
	return NULL;
}

/**
 * Build a sliding cache window around the given page.
 *
 * Determines a window of pages centered around page `n', clears any
 * previously rendered pages, loads display lists for empty ones, and
 * submits rendering jobs. Waits for the current page to be fully cached.
 *
 * @param state  Pointer to the current DocState.
 * @param n      Page number to center the cache window around.
 */

/**
 * Build a sliding cache window around the given page.
 *
 * This function assumes that there are no active render jobs for this
 * window. Callers should call wait_for_win_jobs() first.
 */

void
build_cache_window(DocState *doc_state, EmacsWinState *win_state, int n)
{
	if (!doc_state || !win_state)
		return;
	if (!doc_state->doc_pages || doc_state->pagecount <= 0)
		return;

	int pagecount = doc_state->pagecount;
	if (n < 0)
		n = 0;
	if (n >= pagecount)
		n = pagecount - 1;

	int start, end;
	if (n < MAX_CACHE_WINDOW_SIZE)
		{
			start = 0;
			end = n + MAX_CACHE_WINDOW_SIZE;
			if (end >= pagecount)
				end = pagecount - 1;
		}
	else if (n > (pagecount - 1) - MAX_CACHE_WINDOW_SIZE)
		{
			end = pagecount - 1;
			start = n - MAX_CACHE_WINDOW_SIZE;
			if (start < 0)
				start = 0;
		}
	else
		{
			start = n - MAX_CACHE_WINDOW_SIZE;
			end = n + MAX_CACHE_WINDOW_SIZE;
		}

	win_state->current_page_number = n;
	win_state->current_window_index = n - start;

	for (int i = 0; i < MAX_CACHE_SIZE; ++i)
		{
			int idx = start + i;
			CachedPage *cp = win_state->cache_window[i];
			if (!cp)
				continue;

			if (idx < pagecount && idx <= end)
				{
					/* Check if we can reuse the existing CachedPage */
					pthread_mutex_lock(&cp->mutex);
					int old_page_num = cp->page_num;
					bool reusable = (old_page_num == idx &&
							 cp->status == PAGE_STATUS_READY &&
							 cp->img_data != NULL);
					pthread_mutex_unlock(&cp->mutex);

					if (reusable)
						continue;

					if (old_page_num == idx)
						{
							/* Clear the slot, but keep DocPage reference. */
							clear_cp(cp);
							DocPage *dp = &doc_state->doc_pages[idx];
							if (!dp->ready)
								load_page_dl(doc_state, dp);
							if (!dp->ready)
								{
									mark_cp_error(cp);
									continue;
								}

							if (!submit_render_job(doc_state, win_state, cp, dp))
								mark_cp_error(cp);
						}
					else
						{
							/* Now we acquire a new DocPage and release the last one. */
							clear_cp_and_release(doc_state, cp);
							DocPage *dp = acquire_doc_page(doc_state, idx);
							if (!dp)
								{
									mark_cp_error(cp);
									continue;
								}

							pthread_mutex_lock(&cp->mutex);
							cp->page_num = idx;
							pthread_mutex_unlock(&cp->mutex);

							if (!submit_render_job(doc_state, win_state, cp, dp))
								mark_cp_error(cp);
						}
				}
			else
				{
					/* Outside new window. Clear and release. */
					clear_cp_and_release(doc_state, cp);
				}
		}

	win_state->current_cached_page =
		win_state->cache_window[win_state->current_window_index];
}

/**
 * Slide the cache window forward by one page.
 *
 * Advances to the next page, shifts the cache window if within bounds,
 * reuses or frees cached pages, and spawns rendering jobs as needed.
 * Falls back to full rebuild if near document edges. Waits until the
 * new current page is ready.
 *
 * @param state  Pointer to the current DocState.
 * Return:       true if sliding succeeded, false if at end of document.
 */

void
slide_cache_window_forward(DocState *doc_state, EmacsWinState *win_state)
{
    if (!doc_state || !win_state)
        return;

    if (doc_state->pagecount <= 0)
        return;

    wait_for_win_jobs(win_state);
    int n = win_state->current_page_number + 1;
    if (n < doc_state->pagecount)
        build_cache_window(doc_state, win_state, n);
}

/**
 * Slide the cache window backward by one page.
 *
 * Moves to the previous page, shifts the cache window if within bounds,
 * frees or reuses cached pages, and triggers rendering if needed.
 * Falls back to a full rebuild near document start. Waits until the
 * new current page is ready.
 *
 * @param state  Pointer to the current DocState.
 * Return:       true if sliding succeeded, false if at start of document.
 */

void
slide_cache_window_backward(DocState *doc_state, EmacsWinState *win_state)
{
    if (!doc_state || !win_state)
        return;

    if (doc_state->pagecount <= 0)
        return;

    wait_for_win_jobs(win_state);
    int n = win_state->current_page_number - 1;
    if (n >= 0)
        build_cache_window(doc_state, win_state, n);
}

/**
 * emacs_load_doc - Load a document from Emacs, initialize state, and render
 * first page.
 * @env:   The Emacs environment pointer.
 * @nargs: Number of arguments passed from Elisp (should be 1).
 * @args:  Array of Elisp argument values; args[0] is the file path string.
 * @data:  User-supplied callback data (ignored).
 *
 * Return: Elisp `t` on completion (`nil` otherwise).
 */

emacs_value
emacs_load_doc(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    (void)data;

    size_t str_length = 0;
    int start_page = 0;

    if (nargs >= 2)
    {
        start_page = env->extract_integer(env, args[1]);

        if (start_page < 0)
            start_page = 0;
    }

    DocState *old_state = get_doc_state_ptr(env);

    if (old_state)
    {
        old_state->is_reloading = true;

        clear_all_overlays(env);

        clear_doc_state_ptr(env);
        wait_for_doc_jobs(old_state);
        free_doc_state(old_state);
    }

    emacs_value curr_win = EMACS_CURR_WIN;
    emacs_value current_doc_overlay = EMACS_NIL;

    DocState *doc_state = NULL;
    EmacsWinState *win_state = NULL;

    current_doc_overlay = init_overlay(env, curr_win);

    if (env->eq(env, current_doc_overlay, EMACS_NIL))
    {
        emacs_message(env, "Failed to create reader overlay.");
        return EMACS_NIL;
    }

    doc_state = init_doc_state_ptr(env);

    if (!doc_state)
    {
        emacs_message(env, "Document cannot be loaded into memory.");
        goto cleanup;
    }

    reset_doc_state(doc_state);

    win_state = init_win_state_ptr(env, doc_state, curr_win);

    if (!win_state)
    {
        emacs_message(env, "Window state couldn't be initialized.");
        goto cleanup;
    }

    if (!elisp_2_c_str(env, args[0], &doc_state->path, &str_length))
    {
        emacs_message(env,
                      "Failed to convert Emacs string to C string.");
        goto cleanup;
    }

    if (init_main_ctx(doc_state) == EXIT_FAILURE)
    {
        emacs_message(env, "Failed to initialize MuPDF context.");
        goto cleanup;
    }

    if (load_mupdf_doc(doc_state) == EXIT_FAILURE)
    {
        emacs_message(env,
                      "The document could not be loaded by MuPDF.");
        goto cleanup;
    }

    if (doc_state->pagecount <= 0)
    {
        emacs_message(env, "The document has no pages.");
        goto cleanup;
    }

    outline2plist(env, doc_state, doc_state->outline);

    doc_state->frame_scale = get_frame_scale_factor(env);

    if (doc_state->frame_scale <= 0.0f)
        doc_state->frame_scale = 1.0f;

    /*
     * Allocate DocPage array.
     *
     * doc_state->pagecount is temporarily used as the number of
     * successfully initialized DocPages so cleanup can destroy only valid
     * mutexes.
     */
    int total_pages = doc_state->pagecount;

    doc_state->pagecount = 0;
    doc_state->doc_pages = calloc(total_pages, sizeof(DocPage));

    if (!doc_state->doc_pages)
    {
        emacs_message(env, "Failed to allocate DocPage array.");
        goto cleanup;
    }

    for (int i = 0; i < total_pages; ++i)
    {
        DocPage *dp = &doc_state->doc_pages[i];

        dp->page_num = i;
        dp->display_list = NULL;
        dp->ready = false;
        atomic_init(&dp->refcount, 0);

        if (pthread_mutex_init(&dp->mutex, NULL) != 0)
        {
            emacs_message(env, "Failed to initialize DocPage mutex.");
            goto cleanup;
        }

        doc_state->pagecount = i + 1;
    }

    if (start_page >= doc_state->pagecount)
        start_page = doc_state->pagecount - 1;

    win_state->current_page_number = start_page;

    build_cache_window(doc_state, win_state, start_page);

    set_current_pagecount(env, doc_state);
    set_current_page_number(env, win_state->current_page_number);

    CachedPage *cp = win_state->current_cached_page;

    if (!wait_for_cached_page(cp))
    {
        emacs_message(env, "Failed to render the current page.");
        goto cleanup;
    }

    if (!display_cached_page(env, cp, current_doc_overlay))
    {
        emacs_message(env, "Failed to display the rendered page.");
        goto cleanup;
    }

    set_current_render_status(env);

    return EMACS_T;

cleanup:
    if (!env->eq(env, current_doc_overlay, EMACS_NIL))
    {
        if (win_state)
        {
            clear_overlay_and_win_state(env, current_doc_overlay);
        }
        else
        {
            emacs_value clear_window_args[3] = {
                curr_win,
                env->intern(env, "overlay"),
                EMACS_NIL
            };

            env->funcall(env,
                         env->intern(env, "set-window-parameter"),
                         3,
                         clear_window_args);

            env->funcall(env,
                         env->intern(env, "delete-overlay"),
                         1,
                         &current_doc_overlay);
        }
    }
    else if (win_state)
    {
        win_state->closing = true;
        wait_for_win_jobs(win_state);
        free_cache_window(win_state);
        free(win_state);
    }

    if (doc_state)
    {
        wait_for_doc_jobs(doc_state);
        free_doc_state(doc_state);
    }

    clear_doc_state_ptr(env);

    return EMACS_NIL;
}

/**
 * Redisplay the current document page in Emacs.
 *
 * Resets visual state (no rotation/inversion), re-renders the current
 * cached page in a background thread, waits for completion, and updates
 * the image in the associated Emacs overlay.
 *
 * @param env    Emacs module environment.
 * @param nargs  Argument count (unused).
 * @param args   Argument values (unused).
 * @param data   Additional data (unused).
 * Return:       EMACS_T on success, EMACS_NIL if state is not available.
 */

emacs_value
emacs_redisplay_doc(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                    void *data)
{
	(void)nargs;
	(void)args;
	(void)data;

	DocState *doc_state = NULL;
	EmacsWinState *win_state = NULL;
	emacs_value overlay = EMACS_NIL;

	if (!get_reader_current_state(env, &doc_state, &win_state, &overlay))
		return EMACS_NIL;

	if (doc_state->pagecount <= 0)
		return EMACS_NIL;

	wait_for_win_jobs(win_state);
	nullify_window_cache(win_state);
	build_cache_window(doc_state,
			   win_state,
			   win_state->current_page_number);

	CachedPage *cp = win_state->current_cached_page;

	if (!wait_for_cached_page(cp))
		{
			emacs_message(env, "Failed to render the page.");
			return EMACS_NIL;
		}
	if (!display_cached_page(env, cp, overlay))
		{
			emacs_message(env, "Failed to display the rendered page.");
			return EMACS_NIL;
		}

	set_current_render_status(env);
	return EMACS_T;
}

/**
 * Close the currently loaded document and free all resources.
 *
 * Frees the cache window and cached pages pool, drops the outline,
 * document, and MuPDF context, resets the DocState, and frees it.
 *
 * @param env    Emacs module environment.
 * @param nargs  Argument count (unused).
 * @param args   Argument values (unused).
 * @param data   Additional data (unused).
 * Return:       EMACS_T after successful cleanup.
 */

emacs_value
emacs_close_doc(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                void *data)
{
	(void)nargs;
	(void)args;
	(void)data;

	DocState *state = get_doc_state_ptr(env);
	if (!state)
		{
			emacs_message(env, "No valid DocState user pointer found.");
			return EMACS_NIL;
		}

	state->is_reloading = true;

	clear_all_overlays(env);

	clear_doc_state_ptr(env);

	wait_for_doc_jobs(state);
	free_doc_state(state);

	return EMACS_T;
}

/**
 * emacs_next_page - Move to and display the next page in the overlay.
 * @env:   The Emacs environment pointer.
 * @nargs: (ignored).
 * @args:  (ignored).
 * @data:  (ignored).
 *
 * Return: Elisp `t` on completion, `nil` otherwise.
 */

emacs_value
emacs_next_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                void *data)
{
    (void)nargs;
    (void)args;
    (void)data;

    DocState *doc_state = NULL;
    EmacsWinState *win_state = NULL;
    emacs_value overlay = EMACS_NIL;

    if (!get_reader_current_state(env, &doc_state, &win_state, &overlay))
        return EMACS_NIL;

    if (win_state->current_page_number >= doc_state->pagecount - 1)
    {
        emacs_message(env, "Already last page!");
        return EMACS_NIL;
    }

    return render_and_display_page(env, doc_state, win_state, overlay, win_state->current_page_number + 1);
}

/**
 * emacs_prev_page - Move to and display the previous page in the overlay.
 * @env:   The Emacs environment pointer.
 * @nargs: (ignored).
 * @args:  (ignored).
 * @data:  (ignored).
 *
 * Return: Elisp `t` if page moved, `nil` if already at first page (and warn).
 */

emacs_value
emacs_prev_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                void *data)
{
	(void)nargs;
	(void)args;
	(void)data;

	DocState *doc_state = NULL;
	EmacsWinState *win_state = NULL;
	emacs_value overlay = EMACS_NIL;

	if (!get_reader_current_state(env, &doc_state, &win_state, &overlay))
		return EMACS_NIL;

	if (win_state->current_page_number <= 0)
		{
			emacs_message(env, "Already first page!");
			return EMACS_NIL;
		}

	return render_and_display_page(env,
			 doc_state,
			 win_state,
			 overlay,
			 win_state->current_page_number - 1);
}

/**
 * emacs_first_page - Jump to and display the first page of the document.
 * @env:   The Emacs environment pointer.
 * @nargs: (ignored).
 * @args:  (ignored).
 * @data:  (ignored).
 *
 * Return: Elisp `t` on success, `nil` if already at the first page (and warn).
 */

emacs_value
emacs_first_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                 void *data)
{
	(void)nargs;
	(void)args;
	(void)data;

	DocState *doc_state = NULL;
	EmacsWinState *win_state = NULL;
	emacs_value overlay = EMACS_NIL;

	if (!get_reader_current_state(env, &doc_state, &win_state, &overlay))
		return EMACS_NIL;

	if (win_state->current_page_number == 0)
		{
			emacs_message(env, "Already first page!");
			return EMACS_NIL;
		}

	return render_and_display_page(env, doc_state, win_state, overlay, 0);
}

/**
 * emacs_last_page - Jump to and display the last page of the document.
 * @env:   The Emacs environment pointer.
 * @nargs: (ignored).
 * @args:  (ignored).
 * @data:  (ignored).
 *
 * Return: Elisp `t` on success, `nil` if already at the last page (and warn).
 */

emacs_value
emacs_last_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                void *data)
{
    (void)nargs;
    (void)args;
    (void)data;

    DocState *doc_state = NULL;
    EmacsWinState *win_state = NULL;
    emacs_value overlay = EMACS_NIL;

    if (!get_reader_current_state(env, &doc_state, &win_state, &overlay))
        return EMACS_NIL;

    if (win_state->current_page_number >= doc_state->pagecount - 1)
    {
        emacs_message(env, "Already the last page!");
        return EMACS_NIL;
    }

    return render_and_display_page(env,
                     doc_state,
                     win_state,
                     overlay,
                     doc_state->pagecount - 1);
}

/**
 * emacs_goto_page - Jump to a specific page number and display it.
 * @env:   The Emacs environment pointer.
 * @nargs: Number of Elisp args (should be 1).
 * @args:  Array of Elisp argument values; args[0] is desired page index.
 * @data:  (ignored).
 *
 * Return: Elisp `t` on success or out-of-bounds (always `t`), otherwise emits a
 * bounds warning.
 */

emacs_value
emacs_goto_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                void *data)
{
	(void)nargs;
	(void)data;

	int page_number = env->extract_integer(env, args[0]);

	DocState *doc_state = NULL;
	EmacsWinState *win_state = NULL;
	emacs_value overlay = EMACS_NIL;

	if (!get_reader_current_state(env, &doc_state, &win_state, &overlay))
		return EMACS_NIL;

	return render_and_display_page(env, doc_state, win_state, overlay, page_number);
}

/**
 * emacs_doc_change_page_size - Scale the displayed image to a new size.
 * @env:   The Emacs environment pointer.
 * @nargs: Number of Elisp args (should be 1).
 * @args:  Array of Elisp argument values; args[0] is the float scale factor.
 * @data:  (ignored).
 *
 * Return: Elisp float of new clamped scale on completion, `nil` otherwise.
 */

emacs_value
emacs_doc_scale_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
		     void *data)
{
	(void)nargs;
	(void)data;
	DocState *doc_state = NULL;
	EmacsWinState *win_state = NULL;
	emacs_value overlay = EMACS_NIL;
	if (!get_reader_current_state(env, &doc_state, &win_state, &overlay))
		return EMACS_NIL;
	if (doc_state->pagecount <= 0)
		return EMACS_NIL;
	float scale_factor = env->extract_float(env, args[0]);
	double new_res = fz_clamp(scale_factor * 72.0, MINRES, MAXRES);

	// Calculate the clamped scale factor to return to Elisp
	double scale_clamped = new_res / 72.0;

	wait_for_win_jobs(win_state);
	if (win_state->resolution != new_res)
		{
			win_state->resolution = new_res;
			nullify_window_cache(win_state);
		}
	build_cache_window(doc_state,
			   win_state,
			   win_state->current_page_number);
	CachedPage *cp = win_state->current_cached_page;
	if (!wait_for_cached_page(cp))
		{
			emacs_message(env, "Failed to render the page.");
			return EMACS_NIL;
		}
	if (!display_cached_page(env, cp, overlay))
		{
			emacs_message(env, "Failed to display the rendered page.");
			return EMACS_NIL;
		}
	set_current_render_status(env);

	return env->make_float(env, scale_clamped);
}

/**
 * emacs_doc_rotate - rotate the displayed image by rotation angle.
 * @env:   The Emacs environment pointer.
 * @nargs: Number of Elisp args (should be 1).
 * @args:  Array of Elisp argument values, args[0] is the angle (in degrees).
 * @data:  (ignored).
 *
 * Return: Elisp `t` on completion, `nil` otherwise.
 */

emacs_value
emacs_doc_rotate(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                     void *data)
{
	(void)nargs;
	(void)data;

	DocState *doc_state = NULL;
	EmacsWinState *win_state = NULL;
	emacs_value overlay = EMACS_NIL;

	if (!get_reader_current_state(env, &doc_state, &win_state, &overlay))
		return EMACS_NIL;
	if (doc_state->pagecount <= 0)
		return EMACS_NIL;

	int rotation_deg = env->extract_integer(env, args[0]);
	wait_for_win_jobs(win_state);

	int new_rotate = (win_state->rotate + rotation_deg) % 360;
	if (new_rotate < 0)
		new_rotate += 360;

	if (win_state->rotate != new_rotate)
	{
		win_state->rotate = new_rotate;
		nullify_window_cache(win_state);
	}

	build_cache_window(doc_state, win_state, win_state->current_page_number);
	CachedPage *cp = win_state->current_cached_page;

	if (!wait_for_cached_page(cp))
		{
			emacs_message(env, "Failed to render the page.");
			return EMACS_NIL;
		}

	if (!display_cached_page(env, cp, overlay))
		{
			emacs_message(env, "Failed to display the rendered page.");
			return EMACS_NIL;
		}

	set_current_render_status(env);
	return EMACS_T;
}

emacs_value
emacs_doc_window_create(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                        void *data)
{
	(void)nargs;
	(void)data;

	emacs_value win = args[0];
	DocState *doc_state = get_doc_state_ptr(env);

	if (!doc_state)
		{
			emacs_message(env, "No valid DocState user pointer found.");
			return EMACS_NIL;
		}

	emacs_value overlay = init_overlay(env, win);
	if (env->eq(env, overlay, EMACS_NIL))
		{
			emacs_message(env, "Failed to create reader overlay.");
			return EMACS_NIL;
		}

	EmacsWinState *win_state = init_win_state_ptr(env, doc_state, win);
	if (!win_state)
		{
			emacs_message(env, "Window state couldn't be initialized.");
			emacs_value clear_window_args[3] = {
				win,
				env->intern(env, "overlay"),
				EMACS_NIL
			};
			env->funcall(env, env->intern(env, "set-window-parameter"), 3,
				     clear_window_args);

			env->funcall(env, env->intern(env, "delete-overlay"), 1,
				     &overlay);
			return EMACS_NIL;
		}

	return EMACS_T;
}

emacs_value
emacs_doc_window_close(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                       void *data)
{
	(void)nargs;
	(void)data;

	emacs_value overlay = args[0];

	clear_overlay_and_win_state(env, overlay);

	/* If `clear_overlay_and_win_state' did not delete the overlay because */
	/* the win-state property was already missing, delete it here as a */
	/* fallback. */
	emacs_value overlayp =
		env->funcall(env, env->intern(env, "overlayp"), 1, &overlay);

	if (env->eq(env, overlayp, EMACS_T))
		{
			emacs_value window_sym = env->intern(env, "window");
			emacs_value overlay_window_args[2] = {
				overlay,
				window_sym
			};
			emacs_value win =
				env->funcall(env, env->intern(env, "overlay-get"), 2,
					     overlay_window_args);
			if (!env->eq(env, win, EMACS_NIL))
				{
					emacs_value window_overlay_args[2] = {
						win,
						env->intern(env, "overlay")
					};
					emacs_value window_overlay =
						env->funcall(env, env->intern(env, "window-parameter"), 2,
							     window_overlay_args);

					if (env->eq(env, window_overlay, overlay))
						{
							emacs_value set_args[3] = {
								win,
								env->intern(env, "overlay"),
								EMACS_NIL
							};

							env->funcall(env, env->intern(env, "set-window-parameter"), 3,
								     set_args);
						}
				}

			env->funcall(env, env->intern(env, "delete-overlay"), 1,
				     &overlay);
		}

	return EMACS_T;
}

// Entrypoint for the dynamic module
int
emacs_module_init(struct emacs_runtime *runtime)
{
	emacs_env *env = runtime->get_environment(runtime);

	if (!env)
		{
			fprintf(stderr, "Failed to get Emacs environment.\n");
			return 1;
		}

	// Initialize global MuPDF mutexes
	for (int i = 0; i < FZ_LOCK_MAX; ++i)
		{
			pthread_mutex_init(&g_mupdf_mutex[i], NULL);
		}

	// Module function registrations
	register_module_func(env, emacs_load_doc,
			     "reader-dyn--load-doc", 1, 2,
			     "Loads a DOC to be rendered in Emacs.");

	register_module_func(env, emacs_redisplay_doc,
			     "reader-dyn--redisplay-doc", 0, 0,
			     "Redisplays the document at the current page and scale.");

	register_module_func(env, emacs_close_doc,
			     "reader-dyn--close-doc", 0, 0,
			     "Frees the DocState in memory and all other "
			     "artifacts related to the current document.");

	register_module_func(env, emacs_next_page,
			     "reader-dyn--next-page", 0, 0,
			     "Loads and renders the next page of the document.");

	register_module_func(env, emacs_prev_page,
			     "reader-dyn--prev-page", 0, 0,
			     "Loads and renders the previous page of the document.");

	register_module_func(env, emacs_first_page,
			     "reader-dyn--first-page", 0, 0,
			     "Loads and renders the first page of the document.");

	register_module_func(env, emacs_last_page,
			     "reader-dyn--last-page", 0, 0,
			     "Loads and renders the last page of the document.");

	register_module_func(env, emacs_goto_page,
			     "reader-dyn--goto-page", 1, 1,
			     "Loads and renders the N page number.");

	register_module_func(env, emacs_doc_scale_page,
			     "reader-dyn--scale-page", 1, 1,
			     "Scales the current page of the document by a given FACTOR.");

	register_module_func(env, emacs_doc_invert,
			     "reader-dyn--doc-invert", 1, 1,
			     "Sets the document-local invert state for dark mode.");

	register_module_func(env, emacs_doc_rotate,
			     "reader-dyn--rotate-page", 1, 1,
			     "Rotates the page by the given DEGREE.");

	register_module_func(env, emacs_doc_window_create,
			     "reader-dyn--window-create", 1, 1,
			     "Initializes window parameters for EmacsWinState.");

	register_module_func(env, emacs_doc_window_close,
			     "reader-dyn--window-close", 1, 1,
			     "Frees EmacsWinState.");

	// Register buffer-local variables.
	permanent_buffer_local_var(env, "reader-current-doc-pagecount");
	permanent_buffer_local_var(env, "reader-current-doc-render-status");
	permanent_buffer_local_var(env, "reader-current-doc-state-ptr");
	permanent_buffer_local_var(env, "reader-current-doc-outline");
	permanent_buffer_local_var(env, "reader--recent-pagenumber-fallback");

	// Provide the current dynamic module as a feature to Emacs
	provide(env, "render-core");

	// Initialize the global thread pool
	threadpool_init(&g_thread_pool);

	fprintf(stderr, "reader: %d threads have been initialized.\n",
		MAX_POOL_SIZE);
	fprintf(stderr,
		"reader: dynamic module has been loaded successfully.\n");
	return 0;
}
