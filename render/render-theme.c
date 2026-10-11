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

#include "render-theme.h"
#include "elisp-helpers.h"
#include "render-core.h"
#include "render-threads.h"

/**
 * Toggle dark theme rendering for the current document page.
 *
 * Inverts the `invert` flag in the DocState, re-renders the current
 * page in a background thread, waits for it to finish, and updates
 * the Emacs overlay with the new image.
 *
 * @param env    Emacs module environment.
 * @param nargs  Argument count (unused).
 * @param args   Argument values (unused).
 * @param data   Additional data (unused).
 * Return:       EMACS_T on success, EMACS_NIL if no DocState is available.
 */

emacs_value
emacs_doc_invert(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                     void *data)
{
	(void)nargs;
	(void)data;

	DocState *doc_state = get_doc_state_ptr(env);

	if (!doc_state)
		{
			emacs_message(env, "No valid DocState user pointer found.");
			return EMACS_NIL;
		}

	int invert = env->extract_integer(env, args[0]);

	doc_state->invert = invert ? 1 : 0;

	return EMACS_T;
}
