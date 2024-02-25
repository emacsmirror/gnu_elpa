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

function parse_pattern(pattern) { // ala `apropos-parse-pattern'
	const words = pattern.split(/\s+/);
	const parts = words.map(s => `(${s.replace(/([^a-zA-Z0-9])/g, "\\$1")})`);
	return new RegExp(`((${parts.join("|")}).*)+`, "i");
}

window.addEventListener("load", function (event) {
	const table = document.getElementById("packages");

	const search = document.createElement("input");
	search.setAttribute("placeholder", "Search packages...");
	search.setAttribute("type", "search");

	let tid = false;			// timeout ID
	search.addEventListener("input", function(event) {
		if (tid) clearTimeout(tid);

		tid = setTimeout(function (query) {
			const pattern = parse_pattern(query);
			for (let i = 1; i < table.rows.length; i++) {
				const row = table.rows.item(i);

				const name = row.childNodes.item(0);
				name.classList.remove("alt");

				const desc = row.childNodes.item(2);
				desc.classList.remove("alt");

				if (query) {
					const name_matches = name.innerText.match(pattern);
					const desc_matches = desc.innerText.match(pattern);
					if (name_matches || desc_matches) {
						row.classList.remove("invisible");
						if (name_matches) { name.classList.add("alt"); }
						if (desc_matches) { desc.classList.add("alt"); }
					} else {
						row.classList.add("invisible");
					}
				} else {
					row.classList.remove("invisible");
				}
			}

			tid = false;
		}, 100, event.target.value.trim());
	});

	const main = document.querySelector("main");
	main.prepend(search);
});

// Local Variables:
// indent-tabs-mode: t
// js-indent-level: 4
// tab-width: 4
// End:
