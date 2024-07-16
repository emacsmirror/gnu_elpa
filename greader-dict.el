;;; greader-dict.el --- dictionary module for greader. -*- lexical-binding: t; -*-
;; 
;; Filename: greader-dict.el
;; Description:
;; Author: Michelangelo Rodriguez
;; Maintainer:
;; Created: Lun Gen  8 09:52:58 2024 (+0100)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; dictionary module for greader.
;; This module gives greader the ability to define different wais Of
;; to pronounce a given sequence of characters.
;;
;;
;; There are two types of items that you can add to
;; the dictionary:
;; `word'
;; This is a substitution where the word to be replaced
;; constitutes a word as a whole.
;; For example, the word "dog" will be replaced only if it is
;; surrounded by characters that do not constitute a word. (what is a
;; word may change depending on the major-mode
;; currently in use).
;; `match'
;; This is a "literal" substitution, that is, in which it is sufficient
;; that the word to be replaced is a substring of a word.
;; For example, if you add the match "dog", it will be replaced
;; also in the words "dogs", "doggy", ETC.
;; To add a word of type `word' simply execute the
;; command `C-r d a" (greader-dict-add-entry).
;; (when you launch the command, default values ​​will be proposed which
;; you can consult with the arrow keys.
;; The `greader-dict-add-entry' command includes  in its defaults
;; already existing definitions.
;; In this case, choosing one of these values ​​will change the
;; replacement to be applied.
;; To add a word like `match' run the command
;; `greader-dict-add-entry' with the prefix.
;; In this case, the proposed alternatives will only be those that
;; in the dictionary they are classified, precisely, as match.
;; The command is also useful when selecting text: in this
;; case, it will be proposed to add a match that includes only the
;; selected characters.
;; Matches can also be regular expressions, through
;; which you can create more refined filters than you can
;; just do with simple strings.
;;
;; visibility of dictionary.
;;
;; In general, you can choose between three dictionary visibilities:
;; `global'
;; The default dictionary visibility.
;; `mode'
;; This visibility is shared by all buffers in which a particular mode is
;; in effect.
;; For example, if you are visiting the buffer "foo.txt" in text-mode,
;; and you choose the visibility `mode', all the buffers in which
;; `text-mode' is active and in which 'mode' visibility is set, those
;; buffers all will refer to mode dictionary.
;; `buffer'
;; The most local visibility, in which the dictionary is valid only in
;; the current buffer.
;; You can set the dictionary visibility for each buffer.
;; Use `C-r d c" (greader-dict-change-dictionary) to change the
;; dictionary visibility in the current buffer.
;; If you instead wish to set a particular visibility in a particular
;; buffer or mode as default, you can add the following code snippet
;; in your .emacs file:
;; Suppose that you want to set dictionary `mode' visibility for
;; `info-mode':
;;Just copy the following snippet in your init file without quotes:
;; "(add-hook 'info-mode-hook
;; (lambda ()
;; (greader-dict-mode 1)
;; (greader-dict--set-file 'mode)))
;;
;; filters
;; Filters are an alternative way of implementing your pronunciation
;; rules.
;; Word and match abstractions exist to make simpler daily tasks, or,
;; put in other words, matches and words are regexp presets.
;; Matches in particular have the limitation that you must define a
;; rule in terms of human language entities, matches must have
;; necessarily an alphabetic part.
;; You cannot use matches to define rules for character substitution.
;; Filters instead allow you to use whatever you want in terms of
;; regexps, and to substitute them with all the constructs that the
;; regexp matcher of emacs allow.
;; filters however can become fastly inefficent, because the algoritm
;; used is crude when applying filters: take every filter and cycle
;; over the buffer until you have applied all the filters.
;; the dictionary feature, instead, can handle thousands of
;; definitions with a small decrease of performance.
;; I suggest that you should use word definitions when possible, even
;; if those are similar.
;; Use matches when you individuate a pattern that can work with a
;; common set of characters, using `shy groups'.
;; See the emacs manual for more information about regexp syntax and
;; related.
;; Use `greader-dict-filter-add' to add a new filter to the database,
;; `greader-dict-filter-remove' to remove an existing filter from the
;; database,
;; `greader-dict-filter-modify' to modify a key preserving its old
;; value.
;; to enable filters, use the command `greader-dict-toggle-filters',
;; it is also a customizable variable.
;; This module offers a command
;; `greader-dict-pronounce-in-other-language' that can be used for
;; earing how the tts pronounces a word in another language.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Copyright (C) 2023, 2024  Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(defgroup greader-dict nil
  "String substitution module for greader"
  :group 'greader)

;; variable definitions
(defvar greader-dict-match-indicator "\%\*"
  "Regexp that will be used for match delimiter.")

(defvar greader-dict-match-indicator "\%\*"
  "Regexp that will be used for match delimiter.")

(defvar-local  greader-dictionary nil)

(defvar-local  greader-dictionary nil)
(defvar greader-dict-match-indicator "\%\*"
  "Regexp that will be used for match delimiter.")
(defvar greader-dict--timer nil)
(defvar greader-dict--item-type-alist '((match
					 . "\%\*")
					(filter
					 . "\%f")
					(word . ""))
  "Item types and relative prefixes.")

;; This function saves the contents of the hash table.
(defvar greader-dict-directory (concat user-emacs-directory
				       ".greader-dict/")
  "The directory containing greader-dict files.")
(defvar-local greader-dict-filename "greader-dict.global"
  "File name where dictionary definitions are stored.")
(defvar greader-dict--current-reading-buffer (current-buffer))
;; We use this variable to know if greader-dictionary is saved after
;; the last modification.
(defvar-local greader-dict--saved-flag t)

;; greader-dict-mode.
(defvar-keymap greader-dict-mode-map
  :doc "keymap for `greader-dict-mode'."
  "C-r d a" #'greader-dict-add-entry
  "C-r d k" #'greader-dict-remove-entry
  "C-r d c" #'greader-dict-change-dictionary
  "C-r d l" #'greader-dict-pronounce-in-other-language
  "C-r d m" #'greader-dict-modify-key
  "C-r d s" #'greader-dict-save)

(defvar greader-dict--type-file-alternatives '(buffer mode global))

(defvar greader-dict-lang-history nil)

(defvar-local greader-filters nil
  "Hash table containing our filters.")

(defvar greader-dict-filter-indicator "\%f")

(defvar-keymap greader-dict-filter-map
  :doc "key bindings for greader-dict filter feature."
  "C-r d f a" #'greader-dict-filter-add
  "C-r d f m" #'greader-dict-filter-modify
  "C-r r" #'isearch-backward
  "C-r d f k" #'greader-dict-filter-remove)

(defvar-keymap greader-dict-filter-map
  :doc "key bindings for greader-dict filter feature."
  "C-r d f a" #'greader-dict-filter-add
  "C-r d f m" #'greader-dict-filter-modify
  "C-r r" #'isearch-backward
  "C-r d f k" #'greader-dict-filter-remove)

;; filters.
;; filters allow users to define arbitrary regexps to be replaced
;; either with empty strings or by another string.
;; It is necessary to conceptually abstract filters from other types of
;; match because the filters allow the use of any character and in any case, being applied as they are,
;; the user can better exploit the expressive power of regexps.
;; so filters are a separate feature, which we can consider an
;; "advanced" use case of greader-dict.
;;;###autoload
(define-minor-mode greader-dict-toggle-filters
  "enable or disable filters.
Filters allow you to replace every regexp you wish with something
else you wish.
While matches and words are conceived as facilities that are
designated to be user-friendly interfaces to regexps, with filters
you can unleash all
your expressiveness!
Filters and dictionary are considered independent features for now, so
you can enable filters without the extra payload given by
`greader-dict-mode'.
To use a filter you must first enable this mode, and, eventually, add
a filter.
So use `greader-dict-filter-add' to do that.
When you are prompted for the filter, you should insert the regexp
that must match to have the associated replacement.
You can use the usual `\\\\' expressions, shy groups and all the power
of regexps.
If you are interested in how to write a regexp please consult the info
node `(emacs) Regexps'."
  :keymap greader-dict-filter-map
  :lighter " gr-filters"
  (when greader-dict-toggle-filters
    (setq greader-filters (make-hash-table :test 'ignore-case))
    (setq greader-dict--current-reading-buffer (current-buffer))
    (unless greader-dictionary
      (greader-dict-mode 1)
      (greader-dict-mode -1))
    (greader-dict--filter-init)))

(defcustom greader-dict-include-sentences-in-defaults nil
  "Includi le parole della frase come alternative.
When active, the constituent words of the sentence currently in
reading will be added to the list of defaults (where it makes sense
to do it).
In this way anyone who wishes can search for the word to manipulate
using a menu instead of navigating the buffer."
  :type 'boolean)

;;;###autoload
(define-minor-mode greader-dict-mode
  "Dictionary module for greader.
With this mode it is possible to instruct greader to pronounce in an 
alternative way the words that the tts mispronounces in a given language.
There are two types of definitions understood by greader-dict-mode:
\"word definitions\" are those that must be surrounded by
Non-constituent word characters;
\"Match definitions\" are those that can be replaced regardless of
surrounding characters.
The definition type is determined when you add a new definition:
If you use the region to mark a word, you can select a partial word or
the entire word, and greader-dict-mode will understand that you want
to add a match definition.
If instead you add simply the word under the point, it will be added
as a word definition."
  :lighter " gr-dictionary"
  (cond
   (greader-dict-mode
    (setq greader-dictionary (make-hash-table :test 'ignore-case))
    (setq greader-dict--current-reading-buffer (current-buffer))
    (greader-dict-read-from-dict-file)
    (add-hook 'greader-after-get-sentence-functions
	      #'greader-dict--replace-wrapper 1)
    (add-hook 'buffer-list-update-hook #'greader-dict--update)
    (add-hook 'greader-after-change-language-hook
	      (lambda ()
		(when greader-dict-mode
		  (greader-dict-read-from-dict-file)))))))

;; THanks to the loved and alwais useful elisp reference.
(defun greader-dict--string-hash-ignore-case (a)
  (sxhash-equal (upcase a)))

(define-hash-table-test 'ignore-case
			'string-equal-ignore-case
			'greader-dict--string-hash-ignore-case)
(declare-function string-remove-suffix nil)
;; The following two functions deal, respectively, with
;; replace a dictionary item with the value specified in
;; `greader-dictionari' and its possible variants.
;; The `greader-dict-substitute-match' function takes care of substitution
;; an item even within a word, a sort of partial substitution.
;; The `greader-dict-substitute-word' function takes care of that instead
;; replace a dictionary item only if the sequence matches
;; replace is surrounded by one or more blank class characters.
;; This will allow the user to specify whether a given rule
;; pronunciation in the dictionary should be applied more literally,
;; (for example, a pronunciation rule can be defined such that if a
;; word contains the sequence "ez" it is replaced with the
;; sequence "es", for which, for example, "Rodriguez" would be replaced
;; with "Rodrigues").
(defun greader-dict-substitute-match (match)
  "Replace MATCH with the matching value in `greader-dictionary."
  (let ((normalized-match (string-remove-suffix
			   greader-dict-match-indicator match)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward normalized-match nil t)
	(replace-match (gethash match greader-dictionary))))))

(defun greader-dict-substitute-word (match)
  "Substitute MATCH only if it constitutes an entire word."

  (save-excursion
    (goto-char (point-min))
    (let ((word (concat "\\(\\W?\\)" "\\(" match "\\)" "\\(\\W+\\)"))
	  (alternative-word
	   (concat "\\(^\\)" "\\(" match "\\)" "\\(\\W+\\)"))
	  (end-word
	   (concat "\\(\\W+\\)" "\\(" match "\\)" "\\(\\W*$\\)")))
      (while (or (re-search-forward word nil t) (re-search-forward
						 alternative-word nil
						 t)
		 (re-search-forward end-word nil t))
	(let ((replacement
	       (concat (match-string 1)
		       (gethash match greader-dictionary)
		       (match-string 3))))
	  (replace-match replacement))
	(goto-char (point-min))))))

;; This function adds to the `greader-dictionary' variable the
;; key/value pair that you pass as arguments.
(defcustom greader-dict-save-after-time 30
  "Amount of idleness to wait before saving dictionary data.
A value of 0 indicates saving immediately."
  :type 'number)
(defun greader-dict-add (word replacement)
  "Add the WORD REPLACEMent pair to `greader-dictionary'.
If you want to add a partial replacement, you should
add `\*'to the end of the WORD string parameter."
  ;; We prevent an infinite loop if disallowing that key and values
  ;; are the same.
  (when (string-equal-ignore-case word replacement)
    (user-error "key and value are the same, aborting"))
  (puthash word replacement greader-dictionary)
  (setq greader-dict--saved-flag nil)
  (cond
   ((> greader-dict-save-after-time 0)
    (when (timerp greader-dict--timer)
      (cancel-timer greader-dict--timer))
    (run-with-idle-timer greader-dict-save-after-time nil
			 #'greader-dict-write-file))
   ((= greader-dict-save-after-time 0)
    (unless greader-dict--saved-flag
      (greader-dict-write-file)))
   (t
    (setq greader-dict--saved-flag t)
    nil)))

;; This function removes the association indicated by the key argument.
(defun greader-dict-remove (key)
  "Remove the association specified by KEY from the variable
`greader-dictionary'."
  (remhash key greader-dictionary)
  (setq greader-dict--saved-flag nil)
  (cond
   ((> greader-dict-save-after-time 0)
    (when (timerp greader-dict--timer)
      (cancel-timer greader-dict--timer))
    (run-with-idle-timer greader-dict-save-after-time nil
			 #'greader-dict-write-file))
   ((= greader-dict-save-after-time 0)
    (unless greader-dict--saved-flag
      (greader-dict-write-file)))
   (t
    (setq greader-dict--saved-flag t)
    nil)))

(defun greader-dict-item-type (key)
  "Return the type of KEY.
Possible return values are:
`word' for a wole word,
`match' for partial matches,
`filter' for filters.
There may be more in the future.
Return nil if KEY is not present in `greader-dictionary'."
  (when key
    (let (result)
      (catch 'key-found
	(dolist (type greader-dict--item-type-alist)
	  (if
	      (gethash
	       (concat key
		       (unless (string-suffix-p (cdr type) key)
			 (cdr type)))
	       greader-dictionary)
	      (progn
		(setq result (car type))
		(throw 'key-found result))
	    nil))))))

(defun greader-dict--get-key-from-word (word)
  "Return key related to WORD, nil otherwise."
  (setq word (string-trim word))
  (cond
   ((gethash word greader-dictionary)
    word)
   (t
    (let ((reduced-dictionary (make-hash-table :test 'ignore-case))
	  (key nil))
      (dolist (item (greader-dict--get-matches 'match))
	(puthash item (gethash item greader-dictionary)
		 reduced-dictionary))
      (catch 'key-matched
	(maphash
	 (lambda (k _v)
	   (setq k (string-remove-suffix greader-dict-match-indicator k))
	   (when (string-match k word)
	     (setq key (concat k greader-dict-match-indicator))
	     (throw 'key-matched key)))
	 reduced-dictionary))
      key      ))))

;; This function checks that, in the string you pass to it, there are
;; effectively words to be replaced. If so, use apis
;; previously defined to adequately replace the words that
;; could need it.
;; This is the function to add to
;; `greader-after-get-sentence-functions'.
;;;###autoload
(defun greader-dict-check-and-replace (text)
  "Return the TEXT passed to it, eventually modified according to
`greader-dictionary' and variants."
  (with-temp-buffer
    (setq greader-dictionary (buffer-local-value 'greader-dictionary
						 greader-dict--current-reading-buffer))
    (insert text)
    (goto-char (point-min))
    (when
	(buffer-local-value 'greader-dict-toggle-filters
			    greader-dict--current-reading-buffer) 
      (setq greader-filters (buffer-local-value 'greader-filters
						greader-dict--current-reading-buffer))
      (greader-dict-filters-apply))
    (if
	(buffer-local-value 'greader-dict-mode
			    greader-dict--current-reading-buffer)
	(progn
	  ;; We check if text is actually just one word, and in that case
	  ;; insert a new line at end of temp buffer.
	  (when (= (count-words (point-min) (point-max)) 1)
	    (save-excursion (goto-char (point-max)) (newline)))
	  (let ((inhibit-read-only t))
	    (re-search-forward "\\w" nil t)
	    (while (not (eobp))
	      (let*
		  ((key (greader-dict--get-key-from-word (
							  thing-at-point
							  'word))))
		(cond
		 ((equal (greader-dict-item-type key) 'word)
		  (greader-dict-substitute-word (string-remove-suffix
						 greader-dict-match-indicator
						 key)))
		 ((equal (greader-dict-item-type key) 'match)
		  (greader-dict-substitute-match key))
		 ((not (greader-dict-item-type key))
		  nil)))
	      (re-search-forward "\\W*\\w" nil 1))
	    (buffer-string)))
      (buffer-string))))

(defun greader-dict-write-file ()
  "Save greader-dictionary stored in `greader-dict-filename'."
  (unless (file-exists-p greader-dict-directory)
    (make-directory greader-dict-directory t))
  (with-temp-buffer
    (setq greader-dictionary (buffer-local-value 'greader-dictionary
						 greader-dict--current-reading-buffer))
    (setq greader-dict-filename (buffer-local-value
				 'greader-dict-filename
				 greader-dict--current-reading-buffer))
    (maphash
     (lambda (k v)
       (insert "\"" k "\"" "=" v "\n"))
     greader-dictionary)
    (write-region (point-min) (point-max)
		  (greader-dict--get-file-name)))
  (setq greader-dict--saved-flag t))

(defun greader-dict-read-from-dict-file (&optional force)
  "populate `greader-dictionary' with the contents of
`greader-dict-filename'.
If FORCE is non-nil, reading happens even if there are definitions not
yet saved.
If FORCE is nil \(the default\) then this function generates an
user-error and aborts the reading process."
  ;; This code is to provide a variable
  ;; `greader-dictionary' by default usable in the buffer
  ;; temporary where the replacements defined in `greader-after-get-sentence-functions' occur.
  (when (and (not greader-dict--saved-flag) (not force))
    (user-error "Dictionary has been modified and not yet saved"))
  (when (file-exists-p (greader-dict--get-file-name))
    (with-temp-buffer
      (setq greader-dictionary (buffer-local-value 'greader-dictionary
						   greader-dict--current-reading-buffer))
      (setq greader-dict-filename (buffer-local-value
				   'greader-dict-filename
				   greader-dict--current-reading-buffer))
      (setq greader-filters (buffer-local-value 'greader-filters
						greader-dict--current-reading-buffer))
      (setq greader-dict-toggle-filters (buffer-local-value
					 'greader-dict-toggle-filters greader-dict--current-reading-buffer))
      (insert-file-contents (greader-dict--get-file-name))
      (when-let ((lines (string-lines (buffer-string) t)))
	(dolist (line lines)
	  (setq line (split-string line "=" ))
	  (setf (car line) (car (split-string (car line) "\"" t)))
	  (let ((greader-dict-save-after-time -1))
	    (greader-dict-add (car line) (car (cdr line)))))
	(greader-dict--filter-init)
	(setq greader-dict--saved-flag t)))
    (add-hook 'buffer-list-update-hook #'greader-dict--update)))

;; Command for saving interactively dictionary data.
(defun greader-dict-save ()
  "Save dictionary data.
You should use this command when you want to save your dictionary and
`greader-dict-save-after-time' is set to a negative number.
Otherwise, data saving is done automatically when you add a definition
to the dictionary."
  (interactive)
  (let ((greader-dict--saved-flag nil))
    (greader-dict-write-file)))

(declare-function greader-get-sentence nil)
;; This command Adds a definition to `greader-dictionary'.
;; If the region is active and it does not constitute more than one word,
;; the command will propose the selected word as the original word to
;; substitute.
;; The selected word will be added to `greader-dictionary' as
;; "match", then the definition thus obtained can be applied to
;; any character sequence that includes it.
;; However, if the region is not active, this function will try to
;; determine the word to add through the function
;; `thing-at-point'. In case this function returns a word,
;; it will be used to propose it as the original word to be replaced.
;; In this last case, the word will be added to
;; `greader-dictionary' as "word", so it must constitute itself
;; a word to be replaced.
(defun greader-dict-add-entry (arg)
  "Add an entry to the dictionary.
If point is on a word, this function proposes to add that word as
default.
In this case, you can also use history commands to modify key already
present in the dictionary.
The word will be added as a word, but you can choice to add it as a
match using history commands when in the minibuffer.
If the region is active, and you have selected a word or a partial
word, it will be added as a match.
If neither the region is active nor point is on a word, simply asks
for definition and substitution, without defaults.
If called with prefix argument, ask for a match.
In this case you can type a regular expression.
You can use regular expressions to, for example, craft filters instead
of pronunciation rules.
If the customizable variable
`greader-dict-include-sentences-in-defaults' is enabled, when adding
an entry the defaults will include also the set of words that makes up
the current sentence."
  (interactive "P")
  (let (key value)
    (cond
     (arg
      (setq key (read-regexp "match to add or modify: "
			     (greader-dict--get-matches 'match)))
      (unless key
	(user-error "Input is empty: aborting"))
      (setq key (concat key greader-dict-match-indicator))
      (setq value (read-string (concat "substitute regexp "
				       (string-remove-suffix
					greader-dict-match-indicator
					key) " with:
")
			       nil nil(gethash key greader-dictionary)))
      (greader-dict-add key value))
     ((region-active-p)
      (when (= (count-words(region-beginning) (region-end)) 1)
	(setq key (concat (read-string "Original word to substitute:"
				       nil nil
				       (buffer-substring
					(region-beginning)
					(region-end)))
			  greader-dict-match-indicator))
	(setq value (read-string (concat "substitute match " key
					 "with: ")
				 nil nil
				 (gethash key greader-dictionary)))
	(greader-dict-add key value)))
     ((not (region-active-p))
      (if-let ((default-word (thing-at-point 'word)))
	  (progn
	    (setq key (read-string "Original word to substitute or
modify: "
				   nil
				   nil
				   (append (list default-word)(when
								  greader-dict-include-sentences-in-defaults
								(greader-dict--get-word-alternatives
								 (greader-get-sentence)))
					   (greader-dict--get-matches
					    'word))))
	    (setq value (read-string (concat "substitute word " key
					     " with: ")
				     (gethash key greader-dictionary)))
	    (greader-dict-add key value))
	(setq key (read-string "Word to add or modify: " nil nil
			       (greader-dict--get-matches 'word)))
	(setq value (read-string (concat "substitute " key " with: ")
				 nil nil
				 (gethash key greader-dictionary)))
	(greader-dict-add key value)))))
  (deactivate-mark t))

(declare-function hash-table-keys nil)
(defun greader-dict-remove-entry (key)
  "Remove KEY from the dictionary.
If KEY is not present, signal an user-error."
  (interactive
   (list
    (read-string "key to remove: "nil nil
		 (sort (hash-table-keys greader-dictionary)
		       (lambda (s1 s2)
			 (string-greaterp s2 s1))))))
  (unless (greader-dict-remove key)
    (user-error "Key not found.")))

(defun greader-dict-clear ()
  "Clean the definition table.
It does'nt save cleaned table in the definitions file automatically,
  instead you should save it manually if you want.
Please use `greader-dict-save' for that purpose."
  (interactive)
  (clrhash greader-dictionary)
  (setq greader-dict--saved-flag nil)
  (message "Cleaned."))
(defun greader-dict--replace-wrapper (text)
  "Function to add to `greader-after-get-sentence-functions'.
It simply calls `greader-dict-check-and-replace' with TEXT as its
argument, only if `greader-dict-mode' is enabled."
  (if (or greader-dict-mode greader-dict-toggle-filters)
      (greader-dict-check-and-replace text)
    text))
(declare-function greader-get-language nil)
(defun greader-dict--get-file-name ()
  "Return the absolute path of current dictionary file."
  (concat greader-dict-directory (greader-get-language) "/"
	  (buffer-local-value 'greader-dict-filename
			      greader-dict--current-reading-buffer)))

(defun greader-dict--set-file (type)
  "Set `greader-dict-filename' according to TYPE.
TYPE Must be a symbol, and accepted symbols are:
`buffer', `mode', and `global'.
See also the documentation of `greader-dict--file-type' For
technicalities."
  (cond
   ((not greader-dict--saved-flag)
    (greader-dict-write-file)))
  (cond
   ;; We use `setq-local' only for clarity.
   ((equal type 'buffer)
    (setq-local greader-dict-filename (concat (buffer-name) ".dict")))
   ((equal type 'mode)
    (setq-local greader-dict-filename
		(concat (symbol-name major-mode) ".dict")))
   ((equal type 'global)
    (setq-local greader-dict-filename "greader-dict.global"))
   (t
    (error (concat "type " (symbol-name type) " not valid as "
		   (symbol-name (type-of type)))))))


(defcustom greader-dict-ask-before-change t
  "Ask before changing the dictionary in current buffer.
If toggled on, when you attempt to change the dictionary and current
  dictionary table as data not yet saved,
  `greader-dict-change-dictionary will ask you if you want to save
  the current dictionary first.
If you answer no, you will loose that data.
If you answer yes, instead, data will be saved in the current
  dictionary before setting the dictionary at newone."
  :type 'boolean)

(defun greader-dict--file-type ()
  "Return the file type of dictionary for the current buffer.
The `file type' refers to the scope in a given context:
`buffer'
Means that it exists a file named `(concat buffer-file-name \".dict\")
in `greader-dict-directory'.
`mode'
Means it exists a file called `(concat major-mode \".dict\")' in
`greader-dict-directory'.
`global'
Means it exists a file called \"greader-dict.global\" in
`greader-dict-directory'."
  (let ((default-directory (concat greader-dict-directory
				   (greader-get-language) "/")))
    (cond
     ((string-equal (concat (buffer-name) ".dict")
		    greader-dict-filename)
      'buffer)
     ((string-equal (concat (symbol-name major-mode) ".dict")
		    greader-dict-filename)
      'mode)
     ((string-equal "greader-dict.global" greader-dict-filename)
      'global)
     (t 'global))))
(defun greader-dict--type-alternatives ()
  "Return the list of currently valid alternatives for dictionary."
  (let ((alternatives nil))
    (dolist (alternative greader-dict--type-file-alternatives)
      (unless (equal alternative (greader-dict--file-type))
	(push (symbol-name alternative) alternatives)))
    alternatives))

(defun greader-dict-change-dictionary (new-dict)
  "change the current dictionary.
You can choose between the alternatives by using the arrow keys when
asked."
  (interactive
   (list
    (read-string (concat "Change dictionary from "
			 (symbol-name
			  (greader-dict--file-type))
			 " to: ")
		 nil nil
		 (greader-dict--type-alternatives))))
  (unless greader-dict-mode
    (user-error "Please enable `greader-dict-mode' first."))
  (let ((old-dict (greader-dict--file-type))
	(response nil))
    (unless (equal new-dict old-dict)
      (cond
       ((and greader-dict-ask-before-change (not
					     greader-dict--saved-flag))
	(setq response (yes-or-no-p "There are definitions not yet
  saved; Do you want to save them before changing?"))
	(if response (greader-dict-write-file)
	  (setq
	   greader-dict--saved-flag
	   t))))
      (clrhash greader-dictionary)
      (clrhash greader-filters)
      (greader-dict--set-file (intern new-dict))
      (unless (file-exists-p (greader-dict--get-file-name))
	(shell-command-to-string
	 (concat "touch " greader-dict-filename)))
      (greader-dict--update)
      (greader-dict--update))))

;; (remove-hook 'buffer-list-update-hook #'greader-dict--update)))))

(defvar greader-reading-mode)
(defun greader-dict--update ()
  (when greader-dict-toggle-filters
    (setq greader-dict--current-reading-buffer (current-buffer))
    (let ((dict-mode-state greader-dict-mode))
      (greader-dict-mode 1)
      (greader-dict-read-from-dict-file t)
      (unless dict-mode-state
	(greader-dict-mode -1))))
  (when greader-dict-mode
    (setq greader-dict--current-reading-buffer (current-buffer))
    (unless greader-dict--saved-flag
      (greader-dict-write-file))
    ;; I decided to keep the following code for historical reasons and
    ;; memento.
    ;;   Indeed it is superfluous as it is, because "buffer-locality", so
    ;; the following conditional is not necessary.
    (unless greader-reading-mode
      (clrhash
       (buffer-local-value 'greader-dictionary
			   greader-dict--current-reading-buffer))
      (greader-dict-read-from-dict-file t))))
;; Questa funzione è solo di utilità e potrebbe essere rimossa o
;; modificata in qualsiasi momento.
(defun greader-dict-beep ()
  (beep))

(defun greader-dict-info ()
  "Print some information about current dictionary."
  (interactive)
  (let ((message
	 (concat "Current dictionary is "
		 (symbol-name (greader-dict--file-type))
		 " in file " greader-dict-filename " it has "
		 (number-to-string (hash-table-count
				    greader-dictionary))
		 " entries.")))
    (message "%s" message)))

(defun greader-dict--get-matches (type &optional decorate)
  "Return a list with keys classified as TYPE.
If TYPE is `all', all items in the current dictionary will be included."
  (let ((matches nil))
    (maphash
     (lambda (k _v)
       (cond
	((equal (greader-dict-item-type k) type)
	 (let ((match (and (string-remove-suffix
			    greader-dict-match-indicator k)
			   (string-remove-suffix
			    greader-dict-filter-indicator
			    k))))
	   (when decorate
	     (setq match
		   (concat match " \(" (gethash k greader-dictionary)
			   "\)")))
	   (push match matches)))
	((equal type 'all)
	 (let
	     ((match
	       (string-remove-suffix greader-dict-match-indicator k)))
	   (when decorate
	     (setq match
		   (concat match " \(" (gethash k greader-dictionary)
			   "\)")))
	   (push match matches)))))
     greader-dictionary)
    (sort
     matches
     (lambda (s1 s2)
       (string-greaterp s2 s1)))))
(defun greader-dict-modify-key (arg)
  "Modify a key \(_NOT_ a value associated with it!\).
While `greader-dict-add-entry can modify either keys or associated
values, `greader-dict-modify-key' allows you to modify the key
itself, without modifying the associated value.
if prefix ARG is non-nil, then this command proposes only keys that
are classified as matches.
When called without a prefix, it modifies only keys that are
classified as words."
  (interactive "P")
  (let ((key (read-string "key to modify: " nil nil (if arg
							(greader-dict--get-matches
							 'match)
						      (greader-dict--get-matches
						       'word))))
	(new-key nil))
    (unless key
      (user-error "Key not valid"))
    (if-let ((backup-value (gethash key greader-dictionary)))
	(progn
	  (setq new-key (read-string (concat "substitute key " key "
  with:")
				     nil nil key))
	  (unless new-key
	    (user-error "Invalid replacement"))
	  (greader-dict-remove key)
	  (greader-dict-add new-key backup-value))
      (user-error "Key not found"))))

(defun greader-dict--get-word-alternatives (text)
  "Return a list with a set of words in TEXT."
  (if-let ((alternatives text))
      (progn
	(setq alternatives nil)
	(dolist (word (split-string text "\\W" t))
	  (unless (member word alternatives)
	    (push word alternatives)))
	(reverse alternatives))
    (user-error "No text")))
(declare-function greader-set-language nil)
(declare-function greader-read-asynchronous nil)
;;;###autoload
(defun greader-dict-pronounce-in-other-language (word new-lang)
  "pronounce WORD in the language specified by NEW-LANG.
The currently configured backend will be used for the voice.
NEW-LANG should be an ISO code, compatible with the back-end you are
using.
If the variable `greader-dict-include-sentences-in-defaults is enabled, when
asked about the word to pronounce, the defaults will be a set of words
in the current sentence."

  (interactive
   (list
    (read-string "Word to pronounce: " nil t
		 (when greader-dict-include-sentences-in-defaults
		   (greader-dict--get-word-alternatives
		    (greader-get-sentence))))
    (read-string (concat "language in which you wish to listen:") nil
		 'greader-dict-lang-history)))
  (unless new-lang (user-error "No language specified"))
  (let ((old-lang (greader-get-language)))
    (greader-set-language new-lang)
    (greader-read-asynchronous word)
    (greader-set-language old-lang)))

(defun greader-dict--is-filter-p (key)
  "Return t if KEY is a filter based on
`greader-dict-filter-indicator'."
  (string-suffix-p greader-dict-filter-indicator key))

(defun greader-dict-filter-add (key value)
  "Add KEY as a filter with associated VALUE."
  (interactive
   (let*
       ((key (read-string "filter (regexp) to add or modify: " nil nil
			  (greader-dict--get-matches 'filter)))
	(value (read-string
		(concat "substitute regexp " key " with: ") nil nil
		(gethash (concat key greader-dict-filter-indicator)
			 greader-dictionary))))
     (list key value)))
  (greader-dict-add (concat key greader-dict-filter-indicator) value)
  (greader-dict--filter-init))

(defun greader-dict-filter-remove (key)
  "Remove KEY from the hash-table."
  (interactive
   (let ((key (read-string "filter to remove: " nil nil
			   (greader-dict--get-matches 'filter))))
     (list key)))
  (when (gethash (concat key greader-dict-filter-indicator)
		 greader-dictionary)
    (greader-dict-remove (concat key
				 greader-dict-filter-indicator))
    (greader-dict--filter-init)))

(defun greader-dict-filter-modify (key new-key)
  "Modify _Only_ KEY.
KEY must be an existing filter key, this command provides a list of
them accessible using the history commands.
NEW-KEY must not be empty, if empty, this command will signal an error."

  (interactive
   (let* ((key (read-string "key to modify: " nil nil
			    (greader-dict--get-matches 'filter)))
	  (new-key (read-string "new value: " nil nil key)))
     (unless
	 (gethash (concat key greader-dict-filter-indicator)
		  greader-dictionary)
       (user-error "Invalid key"))
     (when (string-empty-p new-key)
       (user-error "Invalid replacement, string is empty"))
     (list key new-key)))
  (let ((old-value (gethash (concat key greader-dict-filter-indicator)
			    greader-dictionary)))
    (greader-dict-filter-remove key)
    (greader-dict-filter-add new-key old-value))
  (greader-dict--filter-init))

(defun greader-dict--filter-init ()
  "Initialize filters hash table.
It works by subtracting from `greader-dictionary' the entries that are
classified as filters and, eventually, adding them to the filters
hash table."
  (when greader-filters (clrhash greader-filters))
  (maphash
   (lambda (k v)
     (when (and greader-dict-toggle-filters (string-suffix-p
					     greader-dict-filter-indicator
					     k))
       (puthash k v greader-filters)))
   greader-dictionary))

(defun greader-dict-filters-apply ()
  "Apply filters defined in sequence to the current buffer."
  (maphash
   (lambda (k v)
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward (string-remove-suffix
				  greader-dict-filter-indicator k)
				 nil t)
	 (replace-match v))))
   greader-filters))

(provide 'greader-dict)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; greader-dict.el ends here
