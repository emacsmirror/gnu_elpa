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

#ifndef RENDER_CORE_H
#define RENDER_CORE_H

#include "emacs-module.h"
#include <mupdf/fitz.h>
#include <pthread.h>
#include <stdatomic.h>

#define MINRES 18
#define MAXRES 850
#define MAX_CACHE_SIZE 11
#define MAX_CACHE_WINDOW_SIZE (MAX_CACHE_SIZE / 2)

extern pthread_mutex_t g_mupdf_mutex[FZ_LOCK_MAX];

/* Represents what's the current status of the page */
/* in the entire rasterization and display process */

typedef enum
{
	PAGE_STATUS_EMPTY,
	PAGE_STATUS_RENDERING,
	PAGE_STATUS_READY,
	PAGE_STATUS_ERROR
} PageStatus;

/*************************************************************/
/* Representation of the document's page from MuPDF	     */
/* This contains only parsed page data without any	     */
/* knowledge of window, buffer or anything from the	     */
/* frontend. This is to be owned by a document's DocState    */
/* and be shared by all windows that are showing the same    */
/* document. Since it contains directly parsed data from     */
/* the document, it is expensive to be created and should be */
/* reused as much as possible				     */
/*************************************************************/

typedef struct
{
	int page_num;
	fz_display_list *display_list;
	fz_rect bbox;
	bool ready;
	pthread_mutex_t mutex;
} DocPage;


/**************************************************************/
/* This represents rasterized image of a page that has been   */
/* produced via load_page_dl and draw_page_thread. This image */
/* has specific properties such as invert, resolution, etc.   */
/* The image is to be owned by an EmacsWinState, i.e., an     */
/* Emacs Window.					      */
/**************************************************************/

typedef struct
{
	int page_num;
	int imgw, imgh;
	char *img_data;
	size_t img_size;
	PageStatus status;
	pthread_mutex_t mutex;
	pthread_cond_t cond;
} CachedPage;

/**********************************************************************/
/* Representation of the document's specific state in a given Emacs   */
/* Window. All window-specific operations, such as zooming, rotating, */
/* etc. must be done using this. For each window we keep a separate   */
/* cache_window and, of course, a distinct CachedPage to display.     */
/**********************************************************************/

typedef struct
{
	DocState *doc_state;

	float resolution;
	int rotate;

	int current_page_number;
	int current_window_index;

	CachedPage *cache_window[MAX_CACHE_SIZE];
	CachedPage *current_cached_page;

	atomic_int active_jobs;
	bool closing;
} EmacsWinState;

/************************************************************************/
/* The primary representation of the entire document. DocState holds    */
/* DocPages and each EmacsWinState links to some DocState. It shall     */
/* have the important MuPDF objects such as fz_context and fz_document, */
/* and for this reason, to follow MuPDF's multi-threading rules,        */
/* it must always be used by one thread at any given time.	        */
/************************************************************************/

typedef struct
{
	fz_context *ctx;
	fz_locks_context locks;
	fz_document *doc;

	DocPage *doc_pages;

	char *path;
	int pagecount;
	fz_outline *outline;
	float frame_scale; // Emacs' frame-scale-factor
	int invert;

	atomic_int active_jobs;
	bool is_reloading;
} DocState;

int
load_page_dl(DocState *state, CachedPage *cp);
void *
draw_page_thread(void *arg);
void
build_cache_window(DocState *doc_state, EmacsWinState *win_state, int n);
void
slide_cache_window_forward(DocState *state, EmacsWinState *win_state);
void
slide_cache_window_backward(DocState *state, EmacsWinState *win_state);

// Emacs module functions
emacs_value
emacs_load_doc(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value
emacs_doc_window_create(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
			void *data);
emacs_value
emacs_doc_window_close(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
		       void *data);
emacs_value
emacs_next_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value
emacs_prev_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value
emacs_first_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
		 void *data);
emacs_value
emacs_last_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value
emacs_goto_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value
emacs_doc_scale_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
		     void *data);
emacs_value
emacs_doc_rotate_doc(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
		     void *data);

#endif // RENDER_CORE_H
