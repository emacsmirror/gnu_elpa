emacs-lisp-intro-nl.texi:	emacs-lisp-intro-nl.po
	po4a-translate \
	 --format texinfo \
	 --master emacs-lisp-intro.texi \
	 --po emacs-lisp-intro-nl.po -k 30 \
	 --addendum emacs-lisp-intro-nl.addendum \
	 --localized emacs-lisp-intro-nl.texi
