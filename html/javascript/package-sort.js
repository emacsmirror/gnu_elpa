/**
 *
 * @licstart  The following is the entire license notice for the
 *  JavaScript code in this page.
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see
 * <https://www.gnu.org/licenses/>.
 *
 * @licend  The above is the entire license notice
 * for the JavaScript code in this page.
 */

// This script is based on a proposal by Javier Olaechea:
// https://lists.gnu.org/archive/html/emacs-devel/2026-05/msg00384.html

"use strict";

// Each index function gets the <td> node and is supposed to return a
// value we can order using the compare functions below.
const index = {
	"alpha": x => x.textContent,
	"number": x => parseInt(x.textContent),
	"date": x => new Date(x.children[0].dateTime).getTime(),
};

const compare = {
	"asc": (l, r) => l < r,
	"desc": (l, r) => l > r,
};

const comparator = rel => (l, r) => rel(l, r) ? -1 : rel(r, l) ? 1 : 0;

window.addEventListener("DOMContentLoaded", function (event) {
	const tbody = document.querySelector("#packages tbody");
	const rows = Array.from(tbody.children);
	
	const headers = document.querySelectorAll("#packages th");
	for (let i = 0; i < headers.length; i++) {
		const header = headers[i];

		// We support sorting if the column has been annotated by a
		// tag that indicates how to sort the data.
		const fn = index[header.dataset.sort];
		if (!fn) continue;

		header.addEventListener("click", function(event) {
			// We reset the order for each column except the one we
			// are sorting on.  By default, if no previous order is
			// set, we start by sorting in descending order, since the
			// most likely information the user is interested in is
			// finding the most popular packages by descending rank.
			for (let j = 0; j < headers.length; j++) {
				if (j != i) delete headers[j].dataset.order;
			}
			header.dataset.order = header.dataset.order === "desc" ?
				"asc" : "desc";

			// We could move lookup out of this function, but then we
			// would compute more at startup, which is wasteful
			// assuming that not every user will sort the table
			// multiple times over.
			const lookup = new Map(rows.map(e =>
				[e, fn(e.children[i])]))

			rows.sort(comparator((l, r) =>
				compare[header.dataset.order](lookup.get(l), lookup.get(r))));
			tbody.replaceChildren(...rows);
		});
	}
});

// Local Variables:
// indent-tabs-mode: t
// js-indent-level: 4
// tab-width: 4
// End:
