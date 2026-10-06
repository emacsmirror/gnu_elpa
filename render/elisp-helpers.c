// Helper functions that complement the C API of the Emacs Module

//
// Copyright (C) 2025, 2026  Divya Ranjan Pattanaik
// Copyright (C) 2025  Tushar
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

#include "elisp-helpers.h"
#include "render-threads.h"
#include "emacs-module.h"
#include "render-core.h"

/**
 * elisp_2_c_str - Convert an Elisp string to a C string.
 * @env: Pointer (of type `emacs_env') to the environment of the Emacs runtime
 * @value: The value of the Elisp string that is to be converted.  It will only
 * return the value (in Elisp, (symbol-value str)) of the string, not the
 * symbol.
 * @buffer: The C character array that is going to store the converted string.
 * @size: Pointer to the length of the string after being converted, as
 * type size_t.
 *
 * Return: true on success, false on error
 */

bool
elisp_2_c_str(emacs_env *env, emacs_value value, char **buffer, size_t *size)
{
	ptrdiff_t buffer_size;
	if (!env->copy_string_contents(env, value, NULL, &buffer_size))
		return false;
	assert(env->non_local_exit_check(env) == emacs_funcall_exit_return);
	assert(buffer_size > 0);
	*buffer = malloc((size_t)buffer_size);
	if (*buffer == NULL)
	{
		env->non_local_exit_signal(env, env->intern(env, "memory-full"),
					   env->intern(env, "nil"));
		return false;
	}
	ptrdiff_t old_buffer_size = buffer_size;
	if (!env->copy_string_contents(env, value, *buffer, &buffer_size))
	{
		free(*buffer);
		*buffer = NULL;
		return false;
	}
	assert(env->non_local_exit_check(env) == emacs_funcall_exit_return);
	assert(buffer_size == old_buffer_size);
	*size = (size_t)(buffer_size - 1);
	return true;
}

/**
 * provide - Calls the Elisp (provide) function to provide a feature to the
 * module.
 * @env: Pointer (of type `emacs_env') to the environment of the Emacs runtime
 * @value: String of the symbol that is to be provid-ed.
 *
 * It doesn’t store the symbol or value of the feature anywhere, only calls the
 * provide function with the symbol from the string.
 *
 * It’s equivalent to doing the following in Elisp:
 * (provide 'feature)
 */

void
provide(emacs_env *env, const char *value)
{
	emacs_value Qvalue = env->intern(env, value);
	emacs_value Qprovide = env->intern(env, "provide");
	emacs_value args[] = { Qvalue };

	env->funcall(env, Qprovide, 1, args);
}

/**
 * register_module_func - Register a function from the dynamic module into the
 * Emacs environment.  It makes the function available to be directly usable
 * from Emacs as any other Elisp function.
 * @env: Pointer to the Emacs environment for the runtime (emacs_env).
 * @module_func: Pointer to the function (emacs_value) that needs to be
registered.
 * @symbol: The symbol for which the function is to be registered.
 * @min_args: The minimum number of arguments this function must be provided
 * with.
 * @max_args: The maximum number of arguments this function can take.
 * @docstring: The string which would act as the docstring for the module
 * function. Equivalent to the docstring in Elisp functions, and visible from
 * C-h f.
 *
 * The @module_func must be not only compile-able but also it must respect the
 * Elisp runtime of Emacs, any sort of undefined or unpredictable behavior would
 * not be found during compile-time, but rather would result in a segfault of
 * Emacs.
 *
 * Returns: nothing
 */

void
register_module_func(emacs_env *env,
		     emacs_value (*module_func)(emacs_env *env, ptrdiff_t nargs,
						emacs_value *args, void *data),
		     char *symbol, int min_args, int max_args, char *docstring)
{

	emacs_value elisp_func_symbol = env->intern(env, symbol);
	emacs_value elisp_func = env->make_function(
	    env, min_args, max_args, module_func, docstring, NULL);

	emacs_value elisp_func_args[2] = { elisp_func_symbol, elisp_func };
	env->funcall(env, env->intern(env, "fset"), 2, elisp_func_args);
}

/**
 * data2elisp_image - Create an Elisp image object from raw image data

 * One needs to be extremely about handling the image data, and make sure it
 * later gets dropped and freed accordingly.
 *
 * Returns: The Elisp image object, which will be a specific plist, and can be
 * rendered in Emacs with `insert-image' or other means.

 */

emacs_value
data2elisp_image(emacs_env *env, int width, int height, char *img_data,
		 size_t img_size)
{
	if (!img_data || img_size == 0 || width <= 0 || height <= 0)
		return EMACS_NIL;

	emacs_value image_width
	    = env->make_integer(env, width);
	emacs_value image_height
	    = env->make_integer(env, height);
	emacs_value img_data_string
	    = env->make_unibyte_string(env, img_data, img_size);
	emacs_value image_args[9]
	    = { img_data_string,
		env->intern(env, "pbm"),
		env->intern(env, "t"),
		env->intern(env, ":width"),
		image_width,
		env->intern(env, ":height"),
		image_height,
		env->intern(env,
			    ":scale"), // Never really used, only a placeholder
		env->make_integer(env, 1) };

	emacs_value image_data = env->funcall(
	    env, env->intern(env, "create-image"), 9, image_args);

	return image_data;
}

/**
 * init_doc_state_ptr - Initialize the DocState and expose it to Elisp.
 * @env:    The Emacs environment pointer.
 *
 * Return: A pointer to the current `docstate` (or null if the symbol is unbound
 * or not a valid user pointer.).
 */

DocState *
init_doc_state_ptr(emacs_env *env)
{
	DocState *state = malloc(sizeof(DocState));
	state->active_jobs = 0;
	state->is_reloading = false;

	// Create a user pointer and expose it to Emacs in a buffer-local
	emacs_value user_ptr = env->make_user_ptr(env, NULL, state);
	emacs_value doc_state_ptr_sym
	    = env->intern(env, "reader-current-doc-state-ptr");
	env->funcall(env, env->intern(env, "set"), 2,
		     (emacs_value[]){ doc_state_ptr_sym, user_ptr });
	return state;
}

/**
 * get_doc_state_ptr - Retrieve the DocState pointer stored in Elisp.
 * @env:    The Emacs environment pointer.
 *
 * Return: A pointer to the current `DocState` (or NULL if the symbol is
 * unbound or not a valid user pointer.).
 */

DocState *
get_doc_state_ptr(emacs_env *env)
{
	emacs_value ptr_sym = env->intern(env, "reader-current-doc-state-ptr");
	emacs_value ptr
	    = env->funcall(env, env->intern(env, "symbol-value"), 1, &ptr_sym);

	if (env->eq(env, ptr, EMACS_NIL))
		return NULL;

	DocState *state = env->get_user_ptr(env, ptr);
	return state;
}

void
clear_doc_state_ptr(emacs_env *env)
{
	emacs_value sym = env->intern(env, "reader-current-doc-state-ptr");
	env->funcall(env, env->intern(env, "set"), 2,
		     (emacs_value[]){ sym, EMACS_NIL });
}

EmacsWinState *
init_win_state_ptr(emacs_env *env, DocState *doc_state, emacs_value window)
{
	/* Use calloc to ensure the struct starts zeroed */
	EmacsWinState *win_state = calloc(1, sizeof(EmacsWinState));
	if (!win_state)
		return NULL;

	reset_win_state(win_state);
	win_state->doc_state = doc_state;

	/* Allocate the window's CachedPage array */
	for (int i = 0; i < MAX_CACHE_SIZE; ++i)
		{
			CachedPage *cp = malloc(sizeof(CachedPage));
			if (!cp)
				{
					/* Cleanup on allocation failure */
					for (int j = 0; j < i; ++j)
						{
							pthread_mutex_destroy(&win_state->cache_window[j]->mutex);
							pthread_cond_destroy(&win_state->cache_window[j]->cond);
							free(win_state->cache_window[j]);
						}
					free(win_state);
					return NULL;
				}

			cp->page_num = -1;
			cp->img_data = NULL;
			cp->img_size = 0;
			cp->imgw = 0;
			cp->imgh = 0;
			cp->status = PAGE_STATUS_EMPTY;

			pthread_mutex_init(&cp->mutex, NULL);
			pthread_cond_init(&cp->cond, NULL);

			win_state->cache_window[i] = cp;
		}

	win_state->current_cached_page = win_state->cache_window[0];

	/* Create a user pointer */
	emacs_value win_state_usr_ptr = env->make_user_ptr(env, NULL, win_state);

	/* Get the overlay window parameter */
	emacs_value overlay = env->funcall(
					   env, env->intern(env, "window-parameter"), 2,
					   (emacs_value[]){ window, env->intern(env, "overlay") });

	/* Set the overlay property for the EmacsWinState user pointer */
	env->funcall(env, env->intern(env, "overlay-put"), 3,
		     (emacs_value[]){ overlay, env->intern(env, "win-state"),
				      win_state_usr_ptr });
	return win_state;
}

EmacsWinState *
get_win_state_ptr(emacs_env *env, emacs_value overlay)
{
	if (env->eq(env, overlay, EMACS_NIL))
	{
		emacs_message(env,
			      "Selected window doesn't have a valid overlay.");
		return NULL;
	}

	emacs_value ptr = env->funcall(
	    env, env->intern(env, "overlay-get"), 2,
	    (emacs_value[]){ overlay, env->intern(env, "win-state") });

	if (env->eq(env, ptr, EMACS_NIL))
	{
		emacs_message(
		    env,
		    "Selected window doesn't have a pointer to EmacsWinState.");
		return NULL;
	}

	EmacsWinState *win_state = env->get_user_ptr(env, ptr);
	return win_state;
}

void
reset_win_state(EmacsWinState *win_state)
{
    if (!win_state) return;

    win_state->resolution = 72.0;
    win_state->rotate = 0;
    win_state->current_page_number = 0;
    win_state->current_window_index = 0;
    win_state->active_jobs = 0;
    win_state->closing = false;
}

void
set_current_page_number(emacs_env *env, int page)
{
	page += 1; // Exposed number shouldn't be 0-indexed
	emacs_value curr_win = EMACS_CURR_WIN;
	env->funcall(env, env->intern(env, "set-window-parameter"), 3,
		     (emacs_value[]){ curr_win, env->intern(env, "page"),
				      env->make_integer(env, page) });
	env->funcall(env, env->intern(env, "set"), 2,
		     (emacs_value[]){
			 env->intern(env, "reader--recent-pagenumber-fallback"),
			 env->make_integer(env, page) });
}

/**
 * set_current_render_status - Mark the buffer’s render status as true.
 * @env:    The Emacs environment pointer.
 *
 * Sets the Elisp variable `doc-render-status` to `t` in the current buffer,
 * indicating that the image rendering is up to date.
 */

void
set_current_render_status(emacs_env *env)
{
	emacs_value render_status_var
	    = env->intern(env, "reader-current-doc-render-status");
	env->funcall(
	    env, env->intern(env, "set"), 2,
	    (emacs_value[]){ render_status_var, env->intern(env, "t") });
}

/**
 * set_current_pagecount - Expose the document’s total page count to Elisp.
 * @env:    The Emacs environment pointer.
 * @state:  Pointer to the `DocState` containing the pagecount.
 *
 * Sets the Elisp variable `current-doc-pagecount` in the current buffer to the
 * number of pages the document has.
 */

void
set_current_pagecount(emacs_env *env, DocState *state)
{
	emacs_value pagecount_args[2]
	    = { env->intern(env, "reader-current-doc-pagecount"),
		env->make_integer(env, state->pagecount) };
	env->funcall(env, env->intern(env, "set"), 2, pagecount_args);
}

/**
 * init_overlay - Create and register an overlay covering the whole buffer.
 * @env:    The Emacs environment pointer.
 *
 * Makes an overlay spanning the entire buffer for the given window . Stores the
 * overlay object in the the window parameter.
 */

emacs_value
init_overlay(emacs_env *env, emacs_value window)
{
	emacs_value start
	    = env->funcall(env, env->intern(env, "point-min"), 0, NULL);
	emacs_value end
	    = env->funcall(env, env->intern(env, "point-max"), 0, NULL);

	// Check if there is an existing overlay for the window and deletes it
	emacs_value existing_overlay = env->funcall(
	    env, env->intern(env, "window-parameter"), 2,
	    (emacs_value[]){ window, env->intern(env, "overlay") });

	if (!env->eq(env, existing_overlay, EMACS_NIL))
		env->funcall(env, env->intern(env, "reader-dyn--window-close"),
			     1, &existing_overlay);

	emacs_value overlay
	    = env->funcall(env, env->intern(env, "make-overlay"), 2,
			   (emacs_value[]){ start, end });
	env->funcall(
	    env, env->intern(env, "overlay-put"), 3,
	    (emacs_value[]){ overlay, env->intern(env, "window"), window });
	env->funcall(
	    env, env->intern(env, "set-window-parameter"), 3,
	    (emacs_value[]){ window, env->intern(env, "overlay"), overlay });

	return overlay;
}

/*
 * Check whether the overlay is in the correct
 * buffer or not
 */
bool
check_current_overlay(emacs_env *env, emacs_value overlay)
{
	if (overlay == EMACS_NIL)
		return false;

	emacs_value overlay_buffer = env->funcall(
	    env, env->intern(env, "overlay-buffer"), 1, &overlay);
	emacs_value current_buffer
	    = env->funcall(env, env->intern(env, "current-buffer"), 0, NULL);

	return env->eq(env, overlay_buffer, current_buffer);
}

/**
 * get_current_doc_overlay - Retrieve the stored overlay object for the
 * document as a window parameter.
 * @env:    The Emacs environment pointer.
 *
 * Return: The Elisp overlay object, or an unbound value if not set.
 */

emacs_value
get_current_doc_overlay(emacs_env *env)
{
	emacs_value curr_win = EMACS_CURR_WIN;
	emacs_value current_overlay = env->funcall(
	    env, env->intern(env, "window-parameter"), 2,
	    (emacs_value[]){ curr_win, env->intern(env, "overlay") });

	// Check if overlay is valid for current buffer
	if (current_overlay != EMACS_NIL
	    && !check_current_overlay(env, current_overlay))
	{
		// Stale overlay, clear it
		env->funcall(env, env->intern(env, "set-window-parameter"), 3,
			     (emacs_value[]){ curr_win,
					      env->intern(env, "overlay"),
					      EMACS_NIL });
		return EMACS_NIL;
	}

	return current_overlay;
}

/**
 * emacs_message - Print a message in Emacs with the given string
 * @env:    The Emacs environment pointer.
 * @str:    Null-terminated C string containing the message.
 */

void
emacs_message(emacs_env *env, char *str)
{
	emacs_value el_string = env->make_string(env, str, strlen(str));
	env->funcall(env, env->intern(env, "message"), 1, &el_string);
}

/**
 * permanent_buffer_local_var: Sets a buffer local variable and makes it
 * permanently local so that it doesn’t get overridden
 * @env: The Emacs environment pointer.
 * @symbol: The symbol of the variable to be set
 */

void
permanent_buffer_local_var(emacs_env *env, char *symbol)
{
	emacs_value el_symbol = env->intern(env, symbol);
	env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1,
		     &el_symbol);
	env->funcall(env, env->intern(env, "set"), 2,
		     (emacs_value[]){ el_symbol, EMACS_NIL });

	env->funcall(env, env->intern(env, "put"), 3,
		     (emacs_value[]){ el_symbol,
				      env->intern(env, "permanent-local"),
				      EMACS_T });
}

/**
 * Display image data in an Emacs overlay.
 *
 * Converts raw image data into an Emacs image spec and sets it as the
 * `display` property of the given overlay. Flushes the image from cache
 *  after displaying.
 */

void
display_img_to_overlay(emacs_env *env, int width, int height, char *img_data,
		       size_t img_size, emacs_value buffer_overlay)
{
	if (!img_data || img_size == 0 || width <= 0 || height <= 0)
		return;

	emacs_value elisp_img
		= data2elisp_image(env, width, height, img_data, img_size);
	emacs_value overlay_put_args[3]
		= { buffer_overlay, env->intern(env, "display"), elisp_img };
	env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
	env->funcall(env, env->intern(env, "image-flush"), 2, (emacs_value[]){ elisp_img,
									       EMACS_T });
}

/**
 * Convert a fz_outline tree to an Emacs plist structure.
 *
 * Recursively traverses the outline linked list and builds a nested
 * plist representation for Emacs: each node becomes a plist with
 * `:title`, `:page`, and optionally `:children` keys.
 *
 * Stores the result in `reader-current-doc-outline` and returns it.
 *
 * @param env    Emacs module environment.
 * @param node   Root of the fz_outline tree (from MuPDF).
 * Return:       Emacs Lisp list of plists representing the outline.
 */

emacs_value
outline2plist(emacs_env *env, DocState *state, fz_outline *node)
{
	emacs_value outline_plist = EMACS_NIL;
	if (!node)
	{
		return outline_plist;
	}

	while (node)
	{
		if (!node->title)
		{
			node = node->next;
			continue;
		}

		/* Build the base plist: (:title TITLE :page PAGE) */
		emacs_value node_title
		    = env->make_string(env, node->title, strlen(node->title));
		fz_location page_location = fz_resolve_link(
		    state->ctx, state->doc, node->uri, &node->x, &node->y);
		int page_num = fz_page_number_from_location(
		    state->ctx, state->doc, page_location);
		emacs_value node_page = env->make_integer(env, page_num);
		emacs_value base_args[]
		    = { env->intern(env, ":title"), node_title,
			env->intern(env, ":page"), node_page };
		emacs_value node_plist
		    = env->funcall(env, env->intern(env, "list"), 4, base_args);

		/* If there are children, recurse and append as :children key */
		if (node->down)
		{
			emacs_value children
			    = outline2plist(env, state, node->down);
			emacs_value child_pair_args[]
			    = { env->intern(env, ":children"), children };
			emacs_value children_pair = env->funcall(
			    env, env->intern(env, "list"), 2, child_pair_args);
			/* Append the (:children CHILDREN) onto this node's
			 * plist */
			node_plist = env->funcall(
			    env, env->intern(env, "append"), 2,
			    (emacs_value[]){ node_plist, children_pair });
		}

		/* Cons this node_plist onto our accumulator */
		outline_plist = env->funcall(
		    env, env->intern(env, "cons"), 2,
		    (emacs_value[]){ node_plist, outline_plist });
		node = node->next;
	}

	/* Reverse to preserve original document order */
	outline_plist = env->funcall(env, env->intern(env, "nreverse"), 1,
				     &outline_plist);

	/* Store in reader-current-doc-outline and return */
	env->funcall(
	    env, env->intern(env, "set"), 2,
	    (emacs_value[]){ env->intern(env, "reader-current-doc-outline"),
			     outline_plist });
	return outline_plist;
}

float
get_frame_scale_factor(emacs_env *env)
{
	emacs_value factor = env->funcall(
	    env, env->intern(env, "frame-scale-factor"), 0, NULL);
	return env->extract_float(env, factor);
}

/* Fetch the current DocState, EmacsWinState and overlay */
/* To be used when operating on the current state, e.g. */
/* changing pages, zooming, etc. Return false if invalid.*/

bool
get_reader_current_state(emacs_env *env,
			 DocState **doc_state_out,
			 EmacsWinState **win_state_out,
			 emacs_value *overlay_out)
{
	DocState *doc_state = get_doc_state_ptr(env);

	if (!doc_state)
		{
			emacs_message(env, "No valid DocState user pointer found.");
			return false;
		}
	emacs_value overlay = get_current_doc_overlay(env);
	if (!check_current_overlay(env, overlay))
		{
			emacs_message(env, "Invalid overlay state: reinitializing");
			return false;
		}

	EmacsWinState *win_state = get_win_state_ptr(env, overlay);

	if (!win_state || win_state->closing)
		return false;
	if (doc_state_out)
		*doc_state_out = doc_state;
	if (win_state_out)
		*win_state_out = win_state;
	if (overlay_out)
		*overlay_out = overlay;

	return true;
}

/* Free all the CachedPages of an Emacs window's
 cache window. We only nullify them so that new data
can be rendered into those slots, we retain the links to
DocPages. */

void
nullify_window_cache(EmacsWinState *win_state)
{
	if (!win_state)
		return;

	for (int i = 0; i < MAX_CACHE_SIZE; ++i)
		{
			CachedPage *cp = win_state->cache_window[i];
			if (!cp)
				continue;

			pthread_mutex_lock(&cp->mutex);
			cp->status = PAGE_STATUS_EMPTY;
			free(cp->img_data);
			cp->img_data = NULL;
			cp->img_size = 0;
			cp->imgw = 0;
			cp->imgh = 0;
			pthread_mutex_unlock(&cp->mutex);
		}
}

/* Delete the overlay and the EmacsWinState associated  */

void
clear_overlay_and_win_state(emacs_env *env, emacs_value overlay)
{
	if (env->eq(env, overlay, EMACS_NIL))
		return;

	emacs_value overlayp =
		env->funcall(env, env->intern(env, "overlayp"), 1, &overlay);

	if (!env->eq(env, overlayp, EMACS_T))
		return;

	emacs_value win_state_sym = env->intern(env, "win-state");
	emacs_value get_args[2] = { overlay, win_state_sym };
	emacs_value ws_val =
		env->funcall(env, env->intern(env, "overlay-get"), 2, get_args);

	if (env->eq(env, ws_val, EMACS_NIL))
		return;

	// Clear the property first to prevent double close
	emacs_value clear_args[3] = { overlay, win_state_sym, EMACS_NIL };
	env->funcall(env, env->intern(env, "overlay-put"), 3, clear_args);
	EmacsWinState *win_state = env->get_user_ptr(env, ws_val);

	if (win_state)
		{
			win_state->closing = true;
			wait_for_win_jobs(win_state);
			free_cache_window(win_state);
			free(win_state);
		}

	// Clear the window's overlay parameter if it points to this overlay
	emacs_value window_sym = env->intern(env, "window");
	emacs_value overlay_window_args[2] = { overlay, window_sym };
	emacs_value win =
		env->funcall(env, env->intern(env, "overlay-get"), 2,
			     overlay_window_args);

	if (!env->eq(env, win, EMACS_NIL))
		{
			emacs_value window_overlay_args[2] = { win, env->intern(env, "overlay") };
			emacs_value window_overlay =
				env->funcall(env, env->intern(env, "window-parameter"), 2,
					     window_overlay_args);

			if (env->eq(env, window_overlay, overlay))
				{
					emacs_value set_args[3] = { win, env->intern(env, "overlay"), EMACS_NIL };
					env->funcall(env, env->intern(env, "set-window-parameter"), 3,
						     set_args);
				}
		}
	env->funcall(env, env->intern(env, "delete-overlay"), 1, &overlay);
}

/* Safely clear all overlays and their associated EmacsWinStates  */

void
clear_all_overlays(emacs_env *env)
{
	emacs_value start =
		env->funcall(env, env->intern(env, "point-min"), 0, NULL);
	emacs_value end =
		env->funcall(env, env->intern(env, "point-max"), 0, NULL);

	emacs_value overlays_in_args[2] = { start, end };
	emacs_value overlays =
		env->funcall(env, env->intern(env, "overlays-in"), 2,
			     overlays_in_args);

	while (!env->eq(env, overlays, EMACS_NIL))
		{
			emacs_value ov =
				env->funcall(env, env->intern(env, "car"), 1, &overlays);
			clear_overlay_and_win_state(env, ov);
			overlays =
				env->funcall(env, env->intern(env, "cdr"), 1, &overlays);
		}
}

/* Take an already rasterized CachedPage and display it in Emacs */

bool
display_cached_page(emacs_env *env, CachedPage *cp, emacs_value overlay)
{
	char *img_data;
	if (!cp)
		return false;

	// Get data of the page without race conditions
	pthread_mutex_lock(&cp->mutex);
	if (cp->status != PAGE_STATUS_READY || !cp->img_data)
		{
			pthread_mutex_unlock(&cp->mutex);
			return false;
		}
	int width    = cp->imgw;
	int height   = cp->imgh;
	img_data = cp->img_data;
	size_t img_size = cp->img_size;
	pthread_mutex_unlock(&cp->mutex);

	display_img_to_overlay(env, width, height, img_data, img_size, overlay);
	return true;
}

/* Properly render the page, build the cache window arond it, and once rendered display it */

emacs_value
render_and_display_page(emacs_env *env, DocState *doc_state,
			EmacsWinState *win_state,
			emacs_value overlay,
			int page_number)
{
	if (page_number < 0 || page_number >= doc_state->pagecount)
		{
			emacs_message(env, "Provided page number is out of bounds!");
			return EMACS_NIL;
		}

	wait_for_win_jobs(win_state);
	build_cache_window(doc_state, win_state, page_number);

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

	set_current_page_number(env, win_state->current_page_number);
	set_current_render_status(env);
	return EMACS_T;
}
