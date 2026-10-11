// Helper functions that complement the MuPDF C API

//
// Copyright (C) 2025-2026  Divya Ranjan Pattanaik
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

#include "mupdf-helpers.h"
#include "render-core.h"
#include <pthread.h>

/**
 * reset_doc_state - Reset all fields of a DocState to their initial values.
 * @state: Pointer to the DocState to be reset.
 *
 */

void
reset_doc_state(DocState *state)
{
	// We need to preserve these across resets
	int current_active_jobs = state->active_jobs;
	bool current_is_reloading = state->is_reloading;

	*state = (DocState){
		.ctx = NULL,
		.locks = {0},
		.doc = NULL,
		.doc_pages = NULL,
		.path = NULL,
		.pagecount = 0,
		.frame_scale = 1,
		.outline = NULL,
		.invert = 0,
		.active_jobs = current_active_jobs,
		.is_reloading = current_is_reloading
	};
}

void
fail(const char *msg)
{
	fprintf(stderr, "%s\n", msg);
	abort();
}

void
lock_mutex(void *user, int lock)
{
	pthread_mutex_t *mutexes = (pthread_mutex_t *)user;
	if (pthread_mutex_lock(&mutexes[lock]) != 0)
		fail("pthread_mutex_lock()");
}

void
unlock_mutex(void *user, int lock)
{
	pthread_mutex_t *mutexes = (pthread_mutex_t *)user;
	if (pthread_mutex_unlock(&mutexes[lock]) != 0)
		fail("pthread_mutex_unlock()");
}

/**
 * Initialize the main MuPDF context with thread-safe locking.
 *
 * Sets up mutexes for MuPDF's internal locking, assigns them to the
 * document state's lock structure, and creates a new `fz_context'.
 * Also registers default document handlers.
 *
 * @param state  Pointer to the DocState to initialize.
 * Return:       EXIT_SUCCESS on success, EXIT_FAILURE on error.
 */

int
init_main_ctx(DocState *state)
{
	state->locks.user = g_mupdf_mutex;
	state->locks.lock = lock_mutex;
	state->locks.unlock = unlock_mutex;
	state->ctx = fz_new_context(NULL, &state->locks, FZ_STORE_DEFAULT);

	if (!state->ctx)
	{
		fprintf(stderr, "Cannot create MuPDF context\n");
		return EXIT_FAILURE;
	}

	fz_register_document_handlers(state->ctx);

	return EXIT_SUCCESS;
}

/**
 * Load fz_document using MuPDF.
 *
 * Opens the document at `state->path', loads its outline and page count,
 * and stores them in the DocState. Handles errors via MuPDF's exception
 * mechanism.
 *
 * @param state  Pointer to the DocState with initialized context and path.
 * Return:       EXIT_SUCCESS on success, EXIT_FAILURE on failure.
 */

int
load_mupdf_doc(DocState *state)
{

	fz_try(state->ctx)
	{
		state->doc = fz_open_document(state->ctx, state->path);
		state->pagecount = fz_count_pages(state->ctx, state->doc);
	}
	fz_catch(state->ctx)
	{
		fprintf(stderr, "Could not open document\n");
		fz_drop_context(state->ctx);
		state->ctx = NULL;
		return EXIT_FAILURE;
	}

	fz_try(state->ctx)
	{
		state->outline = fz_load_outline(state->ctx, state->doc);
	}
	fz_catch(state->ctx)
	{
		fprintf(stderr, "Could not load outline\n");
		fz_drop_outline(state->ctx, state->outline);
		state->outline = NULL;
	}

	return EXIT_SUCCESS;
}

void
drop_doc_page_display_list(DocState *state, DocPage *dp)
{
	if (!dp) return;

	pthread_mutex_lock(&dp->mutex);
	if (dp->display_list && state->ctx) {
		fz_drop_display_list(state->ctx, dp->display_list);
		dp->display_list = NULL;
	}
	dp->ready = false;
	pthread_mutex_unlock(&dp->mutex);
}

void
free_doc_pages(DocState *state)
{
    if (!state || !state->doc_pages) return;

    for (int i = 0; i < state->pagecount; ++i) {
        DocPage *dp = &state->doc_pages[i];
        pthread_mutex_destroy(&dp->mutex);
        if (dp->display_list && state->ctx) {
            fz_drop_display_list(state->ctx, dp->display_list);
            dp->display_list = NULL;
        }
    }
    free(state->doc_pages);
    state->doc_pages = NULL;
}

/* Decrement refcount when nothing references this DocPage
 and also free its expensive display list from memory */

void
release_doc_page(DocState *doc_state, DocPage *dp)
{
	if (!doc_state || !dp)
		return;

	int prev = atomic_fetch_sub(&dp->refcount, 1);

	if (prev == 1)
		drop_doc_page_display_list(doc_state, dp);
	else if (prev <= 0)
		atomic_store(&dp->refcount, 0);
}

/* A DocPage is being acquried (by a CachedPage) for rendering
   so we increase its refcount and if it isn't ready, we parse it */

DocPage *
acquire_doc_page(DocState *doc_state, int page_num)
{
	if (!doc_state || !doc_state->doc_pages)
		return NULL;

	if (page_num < 0 || page_num >= doc_state->pagecount)
		return NULL;

	DocPage *dp = &doc_state->doc_pages[page_num];

	atomic_fetch_add(&dp->refcount, 1);

	if (!dp->ready)
		{
			if (load_page_dl(doc_state, dp) != EXIT_SUCCESS)
				{
					release_doc_page(doc_state, dp);
					return NULL;
				}
		}

	return dp;
}

/**
 * Free all ready pages in the document's cache window.
 */

void
free_cache_window(EmacsWinState *win_state)
{
	if (!win_state)
		return;

	DocState *doc_state = win_state->doc_state;

	for (int i = 0; i < MAX_CACHE_SIZE; ++i)
		{
			CachedPage *cp = win_state->cache_window[i];
			if (!cp)
				continue;

			int old_page_num = -1;

			pthread_mutex_lock(&cp->mutex);
			old_page_num = cp->page_num;
			cp->page_num = -1;
			cp->status = PAGE_STATUS_EMPTY;

			free(cp->img_data);
			cp->img_data = NULL;
			cp->img_size = 0;
			cp->imgw = 0;
			cp->imgh = 0;
			pthread_mutex_unlock(&cp->mutex);

			/*
			 * Release this slot's reference to the shared DocPage.
			 * If this was the last window holding this page, the display
			 * list will be dropped automatically by release_doc_page().
			 */
			if (doc_state && doc_state->doc_pages &&
			    old_page_num >= 0 && old_page_num < doc_state->pagecount)
				{
					DocPage *dp = &doc_state->doc_pages[old_page_num];

					int prev = atomic_fetch_sub(&dp->refcount, 1);
					if (prev == 1)
						{
							drop_doc_page_display_list(doc_state, dp);
						}
					else if (prev <= 0)
						{
							atomic_store(&dp->refcount, 0);
						}
				}

			/* Destroy the slot's synchronization primitives and free it */
			pthread_mutex_destroy(&cp->mutex);
			pthread_cond_destroy(&cp->cond);
			free(cp);

			win_state->cache_window[i] = NULL;
		}

	win_state->current_cached_page = NULL;
}

void
free_doc_state(DocState *state)
{
	if (!state)
		return;

	// Drop MuPDF objects
	if (state->outline)
	{
		fz_drop_outline(state->ctx, state->outline);
		state->outline = NULL;
		}
	if (state->doc)
	{
		fz_drop_document(state->ctx, state->doc);
		state->doc = NULL;
	}
	if (state->ctx)
	{
		fz_drop_context(state->ctx);
		state->ctx = NULL;
	}

	// Free path
	if (state->path)
	{
		free(state->path);
		state->path = NULL;
	}

	// Free the state struct itself
	free(state);
}

// Cache window slot helpers

/**
 * Clear only the rasterize data from a slot (CachedPage), keeping
 * its DocPage reference. Used when the slot already points to the
 * correct page (e.g. during zoom/rotate).
 */

void
clear_cp(CachedPage *cp)
{
	if (!cp) return;
	pthread_mutex_lock(&cp->mutex);
	cp->status = PAGE_STATUS_EMPTY;
	free(cp->img_data);
	cp->img_data = NULL;
	cp->img_size = 0;
	cp->imgw = 0;
	cp->imgh = 0;
	pthread_mutex_unlock(&cp->mutex);
}
/**
 * Clear a slot (CachedPage) completely and release its reference to the shared DocPage.
 * Used when a slot is evicted from the cache window or reassigned to a new page.
 */

void
clear_cp_and_release(DocState *doc_state, CachedPage *cp)
{
    if (!cp) return;
    int old_page_num = -1;

    pthread_mutex_lock(&cp->mutex);
    old_page_num = cp->page_num;
    cp->page_num = -1;
    cp->status = PAGE_STATUS_EMPTY;
    free(cp->img_data);
    cp->img_data = NULL;
    cp->img_size = 0;
    cp->imgw = 0;
    cp->imgh = 0;
    pthread_mutex_unlock(&cp->mutex);

    if (doc_state && doc_state->doc_pages &&
        old_page_num >= 0 && old_page_num < doc_state->pagecount)
    {
        release_doc_page(doc_state, &doc_state->doc_pages[old_page_num]);
    }
}

void
mark_cp_error(CachedPage *cp)
{
	if (!cp)
		return;
	pthread_mutex_lock(&cp->mutex);
	cp->status = PAGE_STATUS_ERROR;
	pthread_cond_signal(&cp->cond);
	pthread_mutex_unlock(&cp->mutex);
}

void
mark_cp_error_if_rendering(CachedPage *cp)
{
	if (!cp)
		return;

	pthread_mutex_lock(&cp->mutex);
	if (cp->status == PAGE_STATUS_RENDERING)
		{
			cp->status = PAGE_STATUS_ERROR;
			pthread_cond_signal(&cp->cond);
		}
	pthread_mutex_unlock(&cp->mutex);
}
