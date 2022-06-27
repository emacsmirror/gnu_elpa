(defgroup find-dupes-dired
  nil
  "Find duplicate files on local and/or remote filesystems."
  :tag "Find Dupes"
  :group 'dired)

(defcustom find-dupes-use-separator-file
  t
  "Whether to use a separator dummy file or not."
  :group 'find-dupes-dired
  :tag "Use separator dummy file"
  :type 'boolean)

(defcustom find-dupes-separator-file
  (concat "/tmp/-" (make-string 40 ?-))
  "Path and name of the separator file used for making search
results easier to discern. It will be created immediately before
and deleted as soon as possible after the search operation
finishes."
  :group 'find-dupes-dired
  :tag "Separator dummy file"
  :type 'string)

(defcustom find-dupes-checksum-exec
  "sha256sum"
  "Name of the executable used for creating file checksums for
comparison."
  :group 'find-dupes-dired
  :tag "Checksum executable"
  :type 'string)

(defcustom find-dupes-size-comparison-function
  '<
  "The comparison function used for sorting grouped results in
ascending or descending order."
  :group 'find-dupes-dired
  :tag "Ascending or descending file size sort order"
  :type '(choice (const :tag "Ascending" :value <)
                 (const :tag "Descending" :value >)))

(defcustom find-dupes-file-filter-functions
  nil
  "Filter functions applied to all files found in a directory. A
filter function must accept as its single argument the file and
return boolean t if the file matches a criteria, otherwise nil."
  :group 'find-dupes-dired
  :tag "File filter functions"
  :type 'hook)

(defvar find-dupes-directories nil
  "List of directories that will be searched for duplicate files.")

(defun find-dupes-checksum-file (file)
  "Create a checksum for `file', using executable defined by `find-dupes-checksum-exec'."
  (let* ((default-directory (file-name-directory (expand-file-name file)))
         (exec (executable-find find-dupes-checksum-exec t)))
    (unless exec
      (error "Checksum program %s not found in exec-path!" exec))
    (first (split-string
            (shell-command-to-string
             (concat exec " \"" (expand-file-name (file-local-name file)) "\""))
            nil
            t))))

(defun find-dupes--ensure-separator-file ()
  "Ensure that the separator file specified by `find-dupes-separator-file' exists."
    (unless (file-exists-p find-dupes-separator-file)
      (make-empty-file find-dupes-separator-file)))

(defun find-dupes--remove-separator-file ()
  "Remove the separator file specified by `find-dupes-separator-file'."
  (when (file-exists-p find-dupes-separator-file)
    (delete-file find-dupes-separator-file nil)))

(defmacro find-dupes-with-separator-file (&rest rest)
  `(unwind-protect
         (progn
           (when find-dupes-use-separator-file
             (find-dupes--ensure-separator-file))
           ,@rest)
     (when find-dupes-use-separator-file
       (find-dupes--remove-separator-file))))

(defun find-dupes--apply-file-filter-functions (files)
  (if (and find-dupes-file-filter-functions files)
      (dolist (filter-func find-dupes-file-filter-functions files)
        (setf files (delete-if-not filter-func files)))
    files))

(defun find-dupes--duplicate-files (directories)
  "Given one or more root directories, search below the directories
for duplicate files. Returns a hash-table with the checksums as
keys and a list of size and duplicate files as values."
  (cl-loop with files = (find-dupes--apply-file-filter-functions
                         (mapcan #'(lambda (d)
                                     (directory-files-recursively d ".*"))
                                       (ensure-list directories)))
           and same-size-table = (make-hash-table)
           and checksum-table = (make-hash-table :test 'equal)
           for f in files
           for size = (file-attribute-size (file-attributes f))
           do (setf (gethash size same-size-table)
                    (append (gethash size same-size-table) (list f)))
           finally
           (cl-loop for size being the hash-key in same-size-table using (hash-value same-size-files)
                    if (> (length same-size-files) 1) do
                    (cl-loop for f in same-size-files
                             for checksum = (find-dupes-checksum-file f)
                             do (setf (gethash checksum checksum-table)
                                      (append (gethash checksum checksum-table) (list f)))))
           (cl-loop with size
                    for same-files being the hash-value in checksum-table using (hash-key checksum)
                    do
                    (if (> (length same-files) 1)
                        (setf (gethash checksum checksum-table)
                              (cons (file-attribute-size (file-attributes (first same-files)))
                                    (sort same-files #'string<)))
                      (remhash checksum checksum-table)))
           (cl-return checksum-table)))

(defun find-dupes--generate-dired-list (&optional directories)
  "Generate a list of grouped duplicate files, separated by a
separator file specified by `find-dupes-separator-file'."
  (cl-loop with dupes-table = (find-dupes--duplicate-files (or directories
                                                               find-dupes-directories))
           with sorted-sums = (cl-sort
                               (cl-loop for k being the hash-key in dupes-table using (hash-value v)
                                        collect (list k (first v)))
                               find-dupes-size-comparison-function
                               :key #'second)
           for (checksum) in sorted-sums
           append (rest (gethash checksum dupes-table))
           when find-dupes-use-separator-file append (list find-dupes-separator-file)))

(defun find-dupes-revert-function (&optional arg noconfirm)
  "Revert function used instead of `dired-revert' for dired buffers generated by find-dupes."
  (message "Looking for remaining duplicate files...")
  (setq-local dired-directory
              (append (list (first dired-directory))
                      (find-dupes--generate-dired-list)))
  (message "Reverting buffer complete.")
  (find-dupes-with-separator-file
   (dired-revert)))

;;;###autoload
(defun find-dupes-dired (directories)
  "Find a list of duplicate files inside one or more directories
and show them in a dired buffer."
  (interactive (list (completing-read-multiple "Directories: "
                                               #'read-file-name-internal
                                               #'file-directory-p
                                               t
                                               nil
                                               nil
                                               default-directory)))
  (let ((default-directory "/")
        (truncated-dirs (truncate-string-to-width (string-join directories ", ") 40 0 nil t)))
    (message "Finding duplicate files in %s..." truncated-dirs)
    (if-let ((results (find-dupes--generate-dired-list directories)))
        (progn
          (message "Finding duplicate files in %s completed." truncated-dirs)
          (find-dupes-with-separator-file
           (dired (cons "/" results))
           (setq-local find-dupes-directories directories)
           (setq-local revert-buffer-function 'find-dupes-revert-function)))
      (message "No duplicate files found in %s." truncated-dirs))))

(provide 'find-dupes-dired)

;;; find-dupes-dired.el ends here
