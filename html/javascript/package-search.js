/* This program is free software: you can redistribute it and/or
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
 * <https://www.gnu.org/licenses/>. */

"use strict";

window.addEventListener("load", function (event) {
	const table = document.getElementById("packages");

	const search = document.createElement("input");
	search.setAttribute("placeholder", "Search packages...");
	search.setAttribute("type", "search");
	search.addEventListener("input", function(event) {
		const query = new RegExp(event.target.value, "i");

		for (let i = 1; i < table.rows.length; i++) {
			const row = table.rows.item(i);
			row.classList.remove("invisible");

			const name = row.childNodes.item(0);
			const name_matches = name.innerText.match(query);
			name.classList.remove("alt");

			const desc = row.childNodes.item(2);
			const desc_matches = desc.innerText.match(query);
			desc.classList.remove("alt");

			if (query) { // avoid matching the empty string
				if (name_matches || desc_matches) {
					if (name_matches) { name.classList.add("alt"); }
					if (desc_matches) { desc.classList.add("alt"); }
				} else {
					row.classList.add("invisible");
				}
			}
		}
	});

	const main = document.querySelector("main");
	main.prepend(search);
});

// Local Variables:
// indent-tabs-mode: t
// js-indent-level: 4
// tab-width: 4
// End:
