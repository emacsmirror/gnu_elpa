// Helper functions that complement the MuPDF C API

//
// Copyright (C) 2025, 2026  Divya Ranjan Pattanaik
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
 * doc_page_width - Provide integer width of the current document
 * @state: Pointer to DocState
 * Return: Integer width
 */

int
doc_page_width(EmacsWinState *win_state)
{
	CachedPage *cp = win_state->current_cached_page;
	int width = cp->imgw;
	return width;
}

/**
 * doc_page_height - Provide integer height of the current document
 * @state: Pointer to DocState
 * Return: Integer height
 */

int
doc_page_height(EmacsWinState *win_state)
{
	CachedPage *cp = win_state->current_cached_page;
	int height = cp->imgh;
	return height;
}

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
                      .locks = NULL,
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
	fz_register_document_handlers(state->ctx);

	if (!state->ctx)
	{
		fprintf(stderr, "Cannot create MuPDF context\n");
		return EXIT_FAILURE;
	}

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

/**
 * Free resources associated with a CachedPage.
 *
 * Frees any associated image data if the page was marked
 * as ready. Marks the page as empty.
 *
 * @param cp     Pointer to the CachedPage to be freed.
 */

void
free_cached_page(DocState *state, CachedPage *cp)
{
	// Handle all those that aren''t ready and also allow cleaning up failed/error
	// ones so that they can be retried
	if (cp->status != PAGE_STATUS_READY && cp->status != PAGE_STATUS_ERROR)
		return;

	pthread_mutex_lock(&cp->mutex);
	cp->status = PAGE_STATUS_EMPTY;
	free(cp->img_data);
	cp->img_data = NULL;
	cp->img_size = 0;
	cp->page_num = -1;
	pthread_mutex_unlock(&cp->mutex);
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

/**
 * Free all ready pages in the document's cache window.
 *
 * Iterates through the cache window and frees pages marked as ready.
 * Resets the cache window pointer afterward.
 *
 * @param state  Pointer to the current DocState.
 */

void
free_cache_window(DocState *doc_state, EmacsWinState *win_state)
{
	fprintf(stderr, "Freeing the cache window for current document...\n");
	for (int i = 0; i < MAX_CACHE_SIZE; i++)
		{
			if (win_state->cache_window[i]
			    && (win_state->cache_window[i]->status == PAGE_STATUS_READY
				|| win_state->cache_window[i]->status == PAGE_STATUS_ERROR))
				{
					free_cached_page(doc_state, win_state->cache_window[i]);
				}
		}

	// Clear the whole array
	memset(win_state->cache_window, 0, sizeof(win_state->cache_window));
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
