EMACS ?= emacs

math2svg.js: math2svg/package.json math2svg/*.js
	cd math2svg && npm install && npm run build

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

clean:
	rm -rf math2svg.js* *.elc math2svg/node_modules
