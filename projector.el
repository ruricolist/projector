;;; projector.el --- project-oriented Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2009, 2010, 2011, 2012 Paul M. Rodriguez

;; Author: Paul M. Rodriguez <pmr@ruricolist.com>
;; Created: 2009-12-11
;; Keywords: project

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The natural unit of software development today is the project, not
;; the file. Emacs excels within the file, and lately it has acquired
;; some IDE-like capabilities for languages that require them. But
;; there is still need for a simple, fast, and pervasive mechanism for
;; discovering and navigating projects. Such a mechanism should be
;; scalable to large projects, mostly free of dependencies, and
;; integrated with the rest of Emacs.

;; This is what Projector is for. Projector detects projects and
;; maintains tags tables for them, automatically and in the
;; background. Using this data, it provides interactive (Ido-powered)
;; navigation between buffers, files, and definitions within projects,
;; as well as navigation between them.

;; In addition, for many commands, Projector defines a version that
;; works on the entire project as if it were a single file. For
;; example, M-x `edit-project' brings up the project in a single Dired
;; buffer, and M-x `occur-in-project' does what it says.
;; (Incidentally, `occur-in-project' with `occur-edit-mode' is
;; indispensable.)

;; To browse a list of all Projector commands, try M-x
;; `apropos-projector'.

;; The only key sequences Projector explicitly binds are for finding
;; tags. The default bindings are incompatible with how Projector
;; handles tags files.

;; Projector is written from the point of view that a project is not
;; just a set of files, but a mental context. Once you have opened a
;; project, you want to keep that project in front of you until you
;; explictly set it aside.

;; The algorithm that Emacs uses to replace killed buffers violates
;; this principle. Thus, when you kill a buffer, Projector arranges
;; for another buffer in the same project to replace the killed one.
;; This ensures that the current project remains current as long as
;; possible.

;; Usage:
;; (require 'projector)
;; (projector-mode 1)

;; Try M-x `apropos-projector' for a list of commands. The variable
;; `projector-map' holds an example keymap you might find useful.

;; (global-set-key (kbd "C-z") projector-map)

;; Note that Projector depends heavily on lexical binding. It will not
;; work in versions of Emacs earlier than 24.

;; Projector depends on GNU find. If it is not in the path, you should
;; set `projector-find-program' to point to it.



;;;; Code:

;;;; Other packages.
(require 'cl)
(require 'etags)
(require 'format-spec)
(require 'easymenu)
(require 'loadhist)                     ;for feature-file
(require 'tramp)
(require 'parse-time)                   ;for digit-char-p


;;;; Options
(defgroup projector nil
  "Project-oriented Emacs"
  :group 'programming
  :group 'tools
  :group 'extensions
  :tag "Projector")

(defcustom projector-interactive t
  "Should Projector use interactive completion?"
  :group 'projector
  :type '(choice (const :tag "Use interactive completion" t)
                 (const :tag "Use default completion" nil)))

(defun projector-completing-read (prompt collection &rest args)
  (if projector-interactive
      (apply #'ido-completing-read prompt
             ;; Ido only handles lists of strings.
             (if (listp collection)
                 collection
               (all-completions "" collection))
             args)
    (apply #'completing-read prompt collection args)))

(defcustom projector-designated-roots nil
  "Directories known to be project roots."
  :group 'projector
  :type '(repeat directory))

(defcustom projector-parents nil
  "Directories known to hold projects.
If you add ~/projects here then ~/projects/foo, ~/projects/bar,
&c. will be treated as projects."
  :group 'projector
  :type '(repeat directory))

(defcustom projector-plausible-roots
  ;; Start with the classics.
  '("AUTHORS" "BUGS" "CHANGELOG" "ChangeLog" "CHANGES" "CONTRIBUTORS"
    "COPYING" "CREDITS" "HACKING" "INSTALL" "LAST-VERSION" "LICENSE"
    "MAINTAINERS" "MANIFEST" "NEWS" "PKG-INFO" "RELEASE"
    "THANKS" "TODO" "VERSION"
    ;; README
    "README" "README.*"
    "configure" "configure.*"
    "?akefile"                          ;Makefile, Rakefile, &c.
    ;; Scripts
    "install.sh" "install-sh"
    ;; Java.
    "project.xml" "pom.xml" "build.xml"
    ;; Python
    "setup.py"
    ;; Lisp
    "package.lisp" "*.asd")
  "File names that imply the root of a project. May use
wildcards."
  :group 'projector
  :type '(repeat string))

(defcustom projector-ignored-extensions
  (eval-when-compile
    (require 'dired-x)
    (sort
     (delete ""
             (delete-dups
              (append
               ;; Remove directories.
               (remove-if (apply-partially #'string-match-p "/\\'")
                          completion-ignored-extensions)
               dired-patch-unclean-extensions
               dired-tex-unclean-extensions
               dired-latex-unclean-extensions
               dired-bibtex-unclean-extensions
               dired-texinfo-unclean-extensions
               exec-suffixes
               (list ".output" ".dvi" ".bak" "TAGS"
                     ".class" ".dll" ".o" ".so"
                     ".sql" ".sqlite" ".db" "#"
                     ".project" ".iml"
                     ".DS_Store"))))
     #'string-lessp))
  "List of extensions to omit from projects.

Local values for this variable are appended to the global value."
  :group 'projector
  :type '(repeat string)
  :safe 'projector-list-of-strings-p)

(defcustom projector-directory-exclusion-list
  (eval-when-compile
    (require 'vc-hooks)
    (append
     ;; Gleaned from the sources of Ack & Exuberant Ctags. I don't
     ;; know what most of them are.
     (list ".cdv" ".dep" ".dot" ".nib" ".plst" ".pc" "blib"
           "_sgbak" "autom4te.cache" "cover_db" "_build" "EIFGEN"
           "BitKeeper" "RESYNC" "PENDING" ".idea" "WEB-INF")
     vc-directory-exclusion-list))
  "List of directory names to ignore."
  :group 'projector
  :type '(repeat string)
  :safe 'projector-list-of-strings-p)

;; V. http://www.python.org/dev/peps/pep-0350/ for references.
(defcustom projector-code-tags
  '("!!!" "???" "BOGUS" "BUG" "FIXME" "HACK" "NOTE" "NB" "TODO" "XXX")
  "List of tags to generate a project TODO list.")

(defcustom projector-find-program (executable-find "find")
  "Use this find program.
Should be GNU find."
  :group 'projector
  :type '(file :must-match t)
  :risky t)

(defcustom projector-etags-program (executable-find "etags")
  "Use this etags program.
This could be the etags that comes with Emacs, or the one
provided by Exuberant Ctags."
  :group 'projector
  :type '(file :must-match t)
  :risky t)


;;;; Special variables.

(defvar dired-actual-switches)
(defvar dired-subdir-alist)
(defvar apropos-do-all)
(defvar apropos-accumulator)


;;;; Utilities: hacks for functions, buffers, strings, lists.

(eval-when-compile
  (defmacro if-let (bindings then &rest else)
    (declare (indent 2))
    (let ((varlist
           (if (consp (car bindings))
               bindings
             (list bindings))))
      `(let ,varlist
         (if (and ,@(mapcar #'car varlist))
             ,then
           ,@else)))))

(eval-when-compile
  (defmacro when-let (bindings &rest then)
    (declare (indent 1))
    `(if-let ,bindings
         (progn ,@then))))

(defcustom projector-trust-default-directory-modes
  '(magit-mode occur-mode)
  "Major modes where the project can be inferred from
`default-directory'."
  :group 'project
  :type '(repeat symbol))

(defun projector-buffer-file-name (&optional buffer)
  "Return file or directory of BUFFER.
That is, if buffer has no file, return the file of the base
buffer or the value of `list-buffers-directory'."
  (let ((buffer (or buffer (current-buffer))))
    ;; Find the real buffer from cloned buffers and the minibuffer.
    (setq buffer (or (buffer-base-buffer buffer)
                     (if (and (minibufferp buffer)
                              (minibuffer-selected-window))
                         (window-buffer (minibuffer-selected-window)))
                     buffer))
    (when-let (name
               (or (buffer-file-name buffer)
                   (buffer-local-value 'list-buffers-directory buffer)
                   (when (find
                          (buffer-local-value 'major-mode buffer)
                          projector-trust-default-directory-modes)
                     (buffer-local-value 'default-directory buffer))))
      ;; HACK W3M abuses `list-buffers-directory'.
      (unless (equal name (file-name-nondirectory name))
        name))))

(defun projector-list-of-strings-p (v)
  "Is V is a list of strings?"
  (when (consp v) (every #'stringp v)))

(defun projector-list-of-symbols-p (v)
  "Is V is a list of symbols?"
  (when (consp v) (every #'symbolp v)))

(defun projector-remove-prefix (prefix str)
  "Return STR sans PREFIX, or just STR."
  (if (string-prefix-p prefix str)
      (substring str (length prefix))
    str))

(eval-when-compile
  (defsubst string-suffix-p (str1 str2 &optional ignore-case)
    "Return non-nil if STR1 is a suffix of STR2."
    (assert (> (length str2) (length str1)))
    ;; T means a complete match, a number means a partial match or no match.
    (not (numberp (compare-strings
                   str1 nil nil
                   str2 (- (length str2) (length str1)) nil
                   ignore-case)))))

(defun projector-remove-suffix (suffix str)
  "Return STR sans SUFFIX, or just STR."
  (if (string-suffix-p suffix str)
      (substring str 0 (- (length suffix)))
    str))

(defun* projector-child-p (dir file &optional (root default-directory))
  "Is FILE a child of DIR?

Relative file names will be expanded under ROOT. (This only
matters when one file is relative and the other is absolute.)"
  (string-prefix-p
   (expand-file-name (file-name-as-directory dir) root)
   (expand-file-name file root)))

(defun* projector-map-path (fn &optional (file
                                          (or buffer-file-name
                                              list-buffers-directory)))
  "Call FN on every directory in the path of FILE.

Uses `locate-dominating-stop-dir-regexp'."
  (assert file nil "No file")
  (let (acc)
    (do ((dir (abbreviate-file-name (file-name-directory file))
              ;; Using `abbreviate-file-name' means "stop at ~/".
              (file-name-directory (directory-file-name dir)))
         (lastdir nil dir))
        ((or (null dir)
             (equal dir lastdir)
             (string-match locate-dominating-stop-dir-regexp dir))
         acc)
      (push (funcall fn dir) acc))))

(defun projector-file-lessp (file1 file2)
  "Return T if FILE1 should come before FILE2.
This is like STRING<, but it always puts the files in a directory
before its subdirectories."
  (let ((mm (mismatch file1 file2)))
    (if (zerop mm)
        (string< file1 file2)
      (or
       (and (find ?/ file2 :start mm)
            (not (find ?/ file1 :start mm)))
       (string< file1 file2)))))

(eval-when-compile
 (defsubst probe-file (file)
   (if (file-exists-p file)
       file
     nil)))

(defconst projector-etags-compressor-suffixes '("z" "Z" "gz" "GZ" "bz2")
  "Suffixes used by compressors that `etags' supports.
As specified in etags.c")

(defun projector-compressed-file-name (file)
  "Return FILE or its compressed name.
That is, if FILE exists, then return it. Otherwise, try FILE.gz,
FILE.bz2, and so on. If none of them exist, just return FILE.

This is necessary because etags records compressed files without
their compressed extensions."
  (or (probe-file file)
      (loop for ext in projector-etags-compressor-suffixes
            if (probe-file (concat file "." ext))
            return it
            ;; Fall through if the file doesn't exist.
            finally return file)))

(defun projector-uncompressed-file-name (file)
  "Return uncompressed name of FILE, or FILE.
I.e. for FILE.gz, return FILE."
  (let ((ext (file-name-extension file)))
    (when (and ext (not (string-equal ext "")))
      (or (when (eq system-type 'ms-dos)
            ;; Not that I've tested this with DOS, or expect any great
            ;; demand from DOS users, but this is how etags.c does it.
            (loop for suffix in projector-etags-compressor-suffixes
                  if (string-suffix-p suffix ext)
                  ;; Return file.c for file.cgz.
                  return (projector-remove-suffix suffix file)))
          (when (member ext projector-etags-compressor-suffixes)
            ;; Return file.c for file.c.gz.
            (file-name-sans-extension file))
          file))))

(defun projector-wait (proc msg)
  "Display MSG until PROC to finishes.
Nil for PROC means the current buffer's process."
  (when-let (proc (or proc (get-buffer-process (current-buffer))))
    (with-temp-message msg
      (while (accept-process-output proc)
        ;; The message only appears if we force redisplay.
        (sit-for 0)))))

(defun projector-union (&rest lists)
  "Set-union of LISTS, preserving the order in which each element first appears.

Optimized for very large lists."
  (let ((ht (make-hash-table :test 'equal
                             :size (reduce #'+ lists :key #'length))))
    (loop for elt in (apply #'append lists)
          unless (gethash elt ht)
          collect elt
          and do (setf (gethash elt ht) t))))

(eval-and-compile
  (defun projector-memoize (function)
    "Return a version of FUNCTION that caches its results.
This version knows the difference between a nil result and no
result."
    (let ((cache (make-hash-table :test 'equal))
          (no-value-marker (make-symbol "no-value")))
      (lambda (&rest args)
        (let ((value (gethash args cache no-value-marker)))
          (if (eq value no-value-marker)
              (setf (gethash args cache)
                    (apply function args))
            value))))))

(defalias 'projector-vc-root
  (projector-memoize
   (lambda (file)
     (unless (file-remote-p file)
       (when-let (backend (vc-backend file))
         (expand-file-name
          (vc-call-backend backend 'root file))))))
  "The nearest version-controlled directory to FILE.
This function is memoized.")

(defun projector-global+local (var)
  "Union of the global and local values of VAR."
  (union (if (default-boundp var)
             (default-value var))
         (if (local-variable-p var)
             (symbol-value var))))

(defun projector-expand-remote-files (relative-file-names)
  "Return RELATIVE-FILE-NAMES as absolute file names relative to
a remote host, using Tramp file name syntax."
  (let ((v (tramp-dissect-file-name (projector-root))))
    (mapcar (apply-partially
             #'tramp-make-tramp-file-name
             (tramp-file-name-method v)
             (tramp-file-name-user v)
             (tramp-file-name-host v))
            relative-file-names)))

(defalias 'projector-guess-root
  (projector-memoize
   (lambda (file)
     "Check for plausible-roots relative to buffer file."
     (when (and file
                (not (file-remote-p file))
                ;; Don't guess outside the home directory.
                (projector-child-p "~/" file))
       (block nil
         (projector-map-path
          (lambda (dir)
            (let ((default-directory dir))
              (dolist (root projector-plausible-roots)
                (when (file-expand-wildcards root)
                  (return dir)))))))
       nil)))
  "Guess the project root for the current buffer.
This function is memoized.")

(defun* projector-map-fileset (function &optional
                                        (fileset (projector-files t))
                                        (progress-message "Scanning fileset..."))
  "For each file in FILESET, call FUNCTION in a buffer holding
 the contents of that file.

FUNCTION should be a function of one argument, which will be the
name of the current file."
  (let ((progress (make-progress-reporter
                   progress-message
                   0 (length fileset)))
        (count 0))
    ;; Re-using the same buffer for everything is much faster than
    ;; creating a new temp buffer for each iteration.
    (with-temp-buffer
      (let ((inhibit-point-motion-hooks t)
            (inhibit-field-text-motion t)
            (buffer-undo-list t))
        (dolist (file fileset)
          (cond ((get-file-buffer file)
                 (erase-buffer)
                 (insert-buffer-substring-no-properties
                  (get-file-buffer file)))
                ((file-exists-p file)
                 (insert-file-contents file nil nil nil t)))
          (goto-char (point-min))
          (funcall function file)
          (progress-reporter-update progress (incf count)))))
    (progress-reporter-done progress)))

(eval-when-compile
  (defmacro* let/defun (bindings &body body)
    "Hack to make lexical closures over `defun'."
    `(macrolet ((defun (name lambda-list &body body)
                  `(defalias ',name
                     (lambda ,lambda-list ,@body))))
       (let ,bindings
         ,@body))))

(eval-when-compile
  (defsubst mappend (f &rest lists)
    "Like `mapcan', but using `append' instead of `nconc'."
    (apply #'append (apply #'mapcar* f lists))))

(defun merge-tables (hash-tbl &rest tbls)
  "Merge the hash tbls in TBLS into HASH-TBL."
  (dolist (tbl tbls)
    (maphash
     (lambda (k v)
       (setf (gethash k hash-tbl) v))
     tbl))
  hash-tbl)

(defun projector-debounce (fun wait)
  "Return a version of FUN that only runs after WAIT seconds.
The analogy here is debouncing in electronics, where a wait may
be imposed to filter out redundant signals from contact bounce."
  (let (timeout)
    (lambda (&rest args)
      (when timeout
        (cancel-timer timeout))
      (setf timeout
            (run-with-timer
             wait nil
             (lambda ()
               (setf timeout nil)
               (apply fun args)))))))


;;;; Hooks.

(let/defun
 (hooks)

 (defun projector-register-hook (hook fun)
   "Schedule FUN to be added to HOOK."
   (pushnew (cons hook fun) hooks))

 (defun projector-load-hook ()
   "Install Projector's hooks."
   (loop for (hook . function) in hooks
         do (add-hook hook function)))

 (defun projector-unload-hook ()
   "Remove Projector's hooks for `unload-feature'."
   (loop for (hook . function) in hooks
         do (remove-hook hook function))
   (projector-unload-handlers)))

(defun projector-maybe-bubble-project (&optional force)
  "Try to ensure the the current buffer will be replaced by
another project buffer after it is killed."
  (and (symbolp this-command)
       (or force (get this-command 'bubble-before))
       (projector-root-safe)
       (loop with buffers = (projector-buffer-list)
             for b being the buffers
             unless (memq b buffers)
             do (bury-buffer b))))

(projector-register-hook 'pre-command-hook 'projector-maybe-bubble-project)

(put 'kill-buffer 'bubble-before t)
;; (put 'switch-to-buffer 'bubble-before t)
;; (put 'switch-to-buffer-other-window 'bubble-before t)
;; (put 'switch-to-buffer-other-frame 'bubble-before t)
;; (put 'display-buffer 'bubble-before t)
(put 'insert-buffer 'bubble-before t)
(put 'View-kill-and-leave 'bubble-before t)


;;;; Etags

(eval-and-compile

  (defun etags-help ()
    "Return the help message from the etags we're using."
    (shell-command-to-string
     (format "%s --help" projector-etags-program)))

  (defun guess-etags-flavor ()
    "GNU etags or Exuberant Ctags?"
    (let ((help (etags-help)))
      (cond
       ((string-prefix-p "Usage:" help)
        'gnu)
       ((string-prefix-p "Exuberant Ctags" help)
        'exuberant)))))

(defvar etags-flavor (guess-etags-flavor)
  "'gnu for GNU etags, 'exuberant for Exuberant Ctags.")

(defvar etags-whitelist
  (eval-when-compile
    (split-string ".ads .adb .ada .a .asm .def .inc .ins .s .sa
  .S .src .c .h .C .c++ .cc .cpp .cxx .H .h++ .hh .hpp .hxx .M
  .pdb .cs .hs .COB .cob .erl .hrl .fth .tok .F .f .f90 .for .htm
  .html .shtml .java .cl .clisp .el .l .lisp .LSP .lsp .ml .lua
  .LUA Makefile makefile GNUMakefile Makefile.in Makefile.am .lm
  .m .p .pas .pl .pm .php .php3 .php4 .ps .psw .pc .prolog .py
  .oak .sch .scheme .SCM .scm .SM .sm .ss .t .bib .clo .cls .ltx
  .sty .TeX .tex .texi .texinfo .txi .y .y++ .ym .yxx .yy"
                  nil t))
  "File extensions known to be supported by GNU Etags.")


;;;; Updating.

(defconst +projector-new-project-marker+ "new project")

(defmacro ensure (place value)
  `(or ,place (setf ,place ,value)))

(defvar projector-projects
  (make-hash-table :test 'equal :weakness 'value)
  "Weak hash table holding project data structures.")

(defstruct (projector-project
            (:constructor make-projector-project
                          (&key root
                                (tags-table (make-hash-table :test 'equal))
                                (files-table (make-hash-table :test 'equal))
                                (files (all-completions "" files-table))
                                (mtime (current-time)))))
  "Data structure for a project."
  tags-table files-table files root mtime)

(defun projector-project? (object)
  (or (projector-project-p object)
      (eq object +projector-new-project-marker+)))

(defvar projector-project nil
  "The current project.")
(make-variable-buffer-local 'projector-project)

(defun ensure-project ()
  "Return the current project, or build it."
  (if (projector-project? projector-project)
      ;; (and (projector-project-p projector-project)
      ;;      (equal (projector-project-root projector-project)
      ;;             (projector-root)))
      (if (projector-project-p projector-project)
          projector-project
        (signal 'projector-busy nil))
    (let* ((root (projector-root))
           (project (gethash root projector-projects)))
      (if (projector-project-p project)
          (setf projector-project project)
        (projector-build-project root)))))

(defun projector-update (&optional force)
  "Build or rebuild the project."
  (interactive (list t))
  (condition-case nil
      (let ((root (projector-root)))
        (when force (setf (gethash root projector-known-roots) :project))
        (projector-build-project root))
    (no-project
     (condition-case nil
         (call-interactively 'projector-build-project)
       (projector-busy nil)))))

(defalias 'projector-maybe-update
  (projector-debounce
   (lambda ()
     (interactive)
     (when (projector-root-safe)
       (projector-update)))
   (* 60 5))
  "Rebuild the tags for the current project.
This function is debounced.")

(defalias 'new-project 'projector-build-project)

(defalias 'update-project 'projector-update)

(projector-register-hook 'after-save-hook 'projector-maybe-update)


;;;; Moving parts.

;; errors
(put 'projector-error 'error-conditions '(error projector-error))
(put 'projector-error 'error-message "Projector error")

(put 'no-project 'error-conditions '(error projector-error file-error no-project))
(put 'no-project 'error-message "No project")

(put 'projector-busy 'error-conditions '(error projector-error projector-busy))
(put 'projector-busy 'error-message "Still indexing project")

(put 'unwritable-project 'error-conditions '(error projector-error file-error unwritable-project))
(put 'unwritable-project 'error-message "Project isn't writable to start with")

(defun projector-dir= (a b)
  "Are A and B the same directory?"
  (equal (abbreviate-file-name
          (file-name-as-directory
           (expand-file-name a)))
         (abbreviate-file-name
          (file-name-as-directory
           (expand-file-name b)))))

(defun projector-dir-hash (a)
  "Canonicalize A as a directory and hash it."
  (sxhash (abbreviate-file-name
           (file-name-as-directory
            (expand-file-name a)))))

(define-hash-table-test 'dir=
  'projector-dir= 'projector-dir-hash)

(defun projector-known? (root)
  (gethash root projector-known-roots))

(defun projector-ensure-root (root &optional table)
  (unless (eql (gethash root table) :project)
    (setf (gethash root table) :project))
  root)

(defun projector-add-interesting-subdirs (dir table)
  (let ((fulldir (expand-file-name (file-name-as-directory dir))))
    (when (file-directory-p dir)
      (loop for file in (directory-files fulldir)
            for absolute = (file-name-as-directory
                            (expand-file-name file fulldir))
            ;; The idea is to put the most expensive tests last.
            when (and (not (eq ?. (aref file 0)))
                      (not (member file projector-directory-exclusion-list))
                      (not (gethash file table))
                      (file-directory-p absolute))
            do (projector-ensure-root absolute table)
            and collect file))))

(defun projector-notice-explicit-parents (table msg)
  (loop for dir in projector-parents
        do (when msg (message "Scanning %s..." dir))
        if (projector-add-interesting-subdirs dir table)
        nconc it))

(defun projector-notice-dir-locals (table)
  (loop for entry in dir-locals-directory-cache
        for root = (car entry)
        unless (gethash root table)
        do (setf (gethash root table) :project)
        and collect root))

(defun projector-scan-for-new-projects (&optional table msg)
  (let ((new (nconc (projector-notice-explicit-parents table msg)
                    (projector-notice-dir-locals table))))
    (if (and new msg)
        (message "%d new:\n%s"
                 (length new)
                 (mapconcat #'abbreviate-file-name new "\n"))
      (when msg (message "No new projects found.")))))

(defvar projector-known-roots
  (let ((table (make-hash-table :test 'dir=)))
    (dolist (root projector-designated-roots)
      (projector-ensure-root root table))
    (dolist (parent projector-parents)
      (projector-ensure-root parent table))
    (projector-scan-for-new-projects table)
    (setf (gethash "~/" table) :stop)
    table)
  "Project roots known to Projector.")

(defun* projector-discover (&optional (table projector-known-roots) msg)
  "Scan `projector-parents' for new projects."
  (interactive (list projector-known-roots t))
  (projector-scan-for-new-projects table msg))

(defun* projector-project-of-file (file)
  (let ((last-dir nil)
        (home "~/"))
    (projector-map-path
     (lambda (dir)
       (if (projector-dir= dir home)
           nil
         (case (gethash dir projector-known-roots)
           (:project (return-from projector-project-of-file dir))
           (:parent (return-from projector-project-of-file last-dir))
           (:stop (signal 'no-project nil))
           ;; Ignore :forget values, as they may be subprojects.
           (:forget nil)))
       (setf last-dir dir))
     file)
    nil))

(defun projector-roots ()
  "Return all project roots known to Projector."
  (let (roots)
    (maphash
     (lambda (k v)
       (when (eq v :project)
         (push k roots)))
     projector-known-roots)
    (nreverse roots)))

(defvar projector-root-functions
  '(projector-project-of-file
    projector-vc-root
    projector-guess-root))

(defun projector-root-of-file (file)
  (if file
      (if-let (hit (run-hook-with-args-until-success
                    'projector-root-functions file))
          (expand-file-name hit)
        (signal 'no-project nil))
    (signal 'no-project "No file")))

(defun* projector-root (&optional (buffer (current-buffer)))
  (let ((root (projector-root-of-file
               (projector-buffer-file-name buffer))))
    (projector-ensure-root root projector-known-roots)
    root))

(defun projector-root-safe (&optional buffer)
  "Return root if there is one, nil otherwise."
  (condition-case nil
      (projector-root buffer)
    (no-project nil)))

(defun projector-assert-current-project ()
  (condition-case nil
      (projector-root)
    (no-project
     (if (or buffer-file-name list-buffers-directory)
         (error "%s" (substitute-command-keys
                      "No project; \\[projector-update] to start one")))
     (error "No project; buffers without file names cannot have projects"))))

(defun projector-previous-project ()
  (or (projector-root-safe)
      (loop for b being the buffers
            if (projector-root-safe b)
            return it)))

(defface projector-read-only-project
  '((t :foreground "red"))
  "Face for names of read-only projects."
  :group 'projector)

(defface projector-writable-project
  '((t :foreground "green"))
  "Face for names of writable projects."
  :group 'projector)

(defcustom projector-small-project-limit 150
  "Maximum size of a small project.
Small projects are loaded without asking."
  :group 'projector
  :type 'integer)

(defalias 'projector-small-project-p
  (projector-memoize
   (lambda (root)
     (< (length (directory-files root nil nil t))
        projector-small-project-limit)))
  "Return non-nil if ROOT is a small project.
Whether a project is small depends on the value of
 `projector-small-project-limit'.")

(defun projector-notice-project ()
  (condition-case nil
      (when-let (root (projector-root-safe))
        (cond ((projector-known? root)
               (ensure-project))
              ((let (use-dialog-box)
                 (unless (projector-small-project-p root)
                   (y-or-n-p (format "Load %s as a project? " root))))
               (setf (gethash root projector-known-roots) :project)
               (ensure-project))
              (t (forget-project root))))
    (projector-busy nil)))

(projector-register-hook 'find-file-hook 'projector-notice-project)

(projector-register-hook 'dired-mode-hook 'projector-notice-project)

(defun projector-mode-line-name ()
  "Format `projector-root' to show in mode line."
  (if (and (or buffer-file-name list-buffers-directory)
           (projector-root-safe))
      (let ((name (file-name-nondirectory
                   (directory-file-name
                    (projector-root)))))
        (when (projector-project? projector-project)
          (propertize
           (truncate-string-to-width (concat " " name) 20 nil nil t)
           'face
           (if (file-writable-p name)
               'projector-writable-project
             'projector-read-only-project))))
    ""))


;;;; Building the find command.

;; Dynamic parameters.
;; We want `find' to return relative file names, by default, so we can
;; expand them properly when the project is remote.
(defvar projector-find-dir    ".")
(defvar projector-find-type   "-type f")
(defvar projector-find-action  nil)
(defvar projector-find-exec    nil)
(defvar projector-find-mtime   nil)

(defun projector-find-command ()
  "Return command to run `find'."
  (format-spec
   ;; This way of building the parameters for `find' is adapted, with
   ;; modifications, from `rgrep' (q.v.).
   "%f %d %m \\( -path %D \\) -prune -o \! \\( -name '.' -o -iname %X \\) %t %a"
   (format-spec-make
    ?f projector-find-program
    ?d projector-find-dir
    ?m (if projector-find-mtime
           (format-time-string "-newermt \"@%s\"" projector-find-mtime)
         "")
    ?t projector-find-type
    ?a (if projector-find-action
           (concat "-print0 | xargs -0 " projector-find-action)
         "-print")
    ;; Exclude these directores.
    ?D (mapconcat (lambda (arg)
                    (concat "\\*/" arg))
                  (projector-global+local 'projector-directory-exclusion-list)
                  " -o -path ")
    ;; Ignore these extensions.
    ?X (mapconcat (lambda (arg)
                    (concat "\\*" arg))
                  (projector-global+local 'projector-ignored-extensions)
                  " -o -iname "))))

(defun projector-start-find-process (name buffer)
  ;; Use a pipe as it is faster, especially with a filter.
  (let ((process-connection-type nil))
   (start-file-process-shell-command
    name buffer (projector-find-command))))

(defun start-gnu-etags (dir mtime)
  (let ((projector-find-action
         (concat projector-etags-program " -o -"))
        (projector-find-mtime mtime)
        (default-directory dir))
    (projector-start-find-process
     (format "%s (gnu etags)" (abbreviate-file-name dir))
     nil)))

(defun start-exuberant-etags (dir)
  (let ((process-connection-type t)
        (default-directory dir))
    (start-file-process-shell-command
     (format "%s (exuberant ctags)" (abbreviate-file-name dir))
     nil
     (concat projector-etags-program " -R -f -"))))

(defun start-etags (dir &optional mtime)
  (case etags-flavor
    (gnu (start-gnu-etags dir mtime))
    (exuberant (start-exuberant-etags dir))))


;;;; Building a table.

;; NOTE The way projects are built and kept track of is contrived to
;; work with garbage collection. Project structs are stored in a
;; value-weak hash table and referenced only via buffer-local
;; bindings, so once all the buffers that reference them are gone they
;; become available for GC.

(defvar *projector-project-locks* nil)

(defun projector-lock-project (dir)
  (pushnew dir *projector-project-locks* :test #'equal))

(defun projector-unlock-project (dir)
  (setf *projector-project-locks*
        (delete dir *projector-project-locks*)))

(defun projector-project-lockedp (dir)
  (member dir *projector-project-locks*))

(defun projector-make-etags-filter (dir tags files)
  "Return an Etags process filter.
The filter closes over two hash tables, one for TAGS and one for
FILES."
  (labels ((alignedp
            (line)
            ;; The equivalent regex can overflow.
            ;; (string-match-p "^[^,\C-a\d]+.*,[0-9]+$" line)
            (and (not (memq (aref line 0) '(?\, ?\C-a ?\d)))
                 (digit-char-p (aref line (1- (length line))))
                 (when-let (pos (position-if-not #'digit-char-p line
                                                 :from-end t))
                   (eq ?\, (aref line pos)))))
           (deduce-name
            (pattern)
            ;; ETAGS.EBNF: "An implicit tag name is deduced from the pattern by
            ;; discarding the last character if it is one of ` \f\t\n\r()=,;',
            ;; then taking all the rightmost consecutive characters in the
            ;; pattern which are not one of those."
            (when (string-match
                   "\\([^ \f\t\n\r()=,;]+\\)[ \f\t\n\r()=,;]?\\'"
                   pattern)
              (match-string 1 pattern))))
    (let (file
          (fragment nil))  ;May hold a broken line from the last call.
      (lambda (proc string)
        (when (stringp string)
          (condition-case err
              (let ((lines (split-string string "[\n\f]" t)))
                (when fragment
                  (if (and (alignedp (first lines))
                           (alignedp fragment))
                      (setf lines (cons fragment lines)
                            fragment nil))
                  (setf lines (cons (concat fragment (first lines)) (rest lines))
                        fragment nil))
                (let (line)
                  (block nil
                    (while lines
                      ;; If a line was broken across two calls to the filter,
                      ;; stitch it back together.
                      (if (rest lines)
                          (setq line (first lines)
                                lines (rest lines))
                        (setq fragment (first lines))
                        (return nil))
                      (let ((line (substring line 0 (min (length line) 10000))))
                        ;; Discard Exuberant Ctags errors
                        (unless (string-prefix-p "etags:" line)
                          (let ((parts (split-string line "[\d\C-a,]")) tag)
                            (case (length parts)
                              ;; A file.
                              (2 (setf file (convert-standard-filename (first parts))))
                              ;; A direct tag.
                              (3 (setf tag
                                       (make-tag :file file
                                                 :text (first parts)
                                                 :name (deduce-name (first parts))
                                                 :line (second parts)
                                                 :position (third parts))))
                              ;; A pattern tag.
                              (4 (setf tag
                                       (make-tag :file file
                                                 :text (first parts)
                                                 :name (second parts)
                                                 :line (third parts)
                                                 :position (fourth parts)))))
                            (when tag
                              (push tag (gethash (tag-name tag) tags))
                              (push tag (gethash file files))))))))))
            (error
             (set-process-filter proc nil)
             (kill-process proc)
             (projector-unlock-project dir)
             (if debug-on-error
                 (signal (car err) (cdr err))
               (error "Cannot build table: %s" (cdr err))))))))))

(defun projector-build-project (dir)
  "Build a tags table for DIR."
  (interactive
   (list
    (read-directory-name "Project root: "
                         (or (projector-guess-root (projector-buffer-file-name))
                             default-directory))))
  (when dir
    (setq dir (file-name-as-directory (expand-file-name dir)))
    (projector-ensure-root dir projector-known-roots)
    (unless (projector-project-lockedp dir)
      (let* ((default-directory dir)
             (existing (when-let (project (gethash dir projector-projects))
                         (when (projector-project-p project)
                           project)))
             (proc (start-etags dir
                                (when existing
                                  (projector-project-mtime existing)))))
        (unless existing
          (setf (gethash dir projector-projects)
                +projector-new-project-marker+
                projector-project
                +projector-new-project-marker+))
        (projector-lock-project dir)
        (set-process-coding-system proc 'raw-text)
        (set-process-query-on-exit-flag proc nil)
        (let ((tags-table (make-hash-table :test 'equal))
              (files-table (make-hash-table :test 'equal))
              (dir dir) (buffer (current-buffer)))
          (set-process-filter
           proc
           (projector-make-etags-filter dir tags-table files-table))
          (set-process-sentinel
           proc
           (lambda (_proc event)
             (when (equal event "finished\n")
               (with-current-buffer buffer
                 (if existing
                     (progn
                       (merge-tables (projector-project-tags-table existing)
                                     tags-table)
                       (merge-tables (projector-project-files-table existing)
                                     files-table)
                       (setf (projector-project-mtime existing)
                             (current-time)))
                   (progn
                     (setf projector-project
                           (setf (gethash dir projector-projects)
                                 (make-projector-project
                                  :root dir
                                  :tags-table tags-table
                                  :files-table files-table))))))
               (projector-unlock-project dir)))))
        (unless (projector-project-p (gethash dir projector-projects))
          (signal 'projector-busy nil))
        proc))))

(defun projector-tags-completion-table ()
  "Get or rebuild completion table for project tags table."
  (projector-project-tags-table (ensure-project)))


;;;; API.

(defun projector-files (&optional absolute)
  "Return a list of project files."
  (let ((files
         (mapcar #'projector-compressed-file-name
                 (projector-project-files (ensure-project)))))
    (when absolute
      (setq files
            (mapcar
             `(lambda (f) (expand-file-name f ,(projector-root)))
             files)))
    (if (file-remote-p (projector-root))
        (projector-expand-remote-files files)
      files)))

(defun projector-recent ()
  "If `recentf' is available, return recent project files."
  (if (boundp 'recentf-list)
      (intersection (projector-files t) recentf-list)))

(defun projector-not-recent ()
  "If `recentf' is loaded, return project files not accessed
  recently."
  (if (boundp 'recentf-list)
      (set-difference (projector-files t) recentf-list)
    (projector-files t)))

(defun projector-open ()
  "List files in `projector-files' that are being visited."
  (loop for b in (projector-buffer-list)
        if (projector-buffer-file-name b)
        collect it))

(defun projector-dirs (&optional absolute)
  (delete-dups
   (delete ""
           (mapcar #'file-name-directory
                   (projector-files absolute)))))

(defun projector-files-and-dirs (&optional absolute)
  (delete ""
          (nconc (projector-files absolute)
                 (projector-dirs absolute))))

(defun projector-filter-buffer (b)
  "Return B if it is a presentable buffer."
  (unless (or (eq (buffer-local-value 'major-mode b) 'tags-table-mode)
              (string-prefix-p " " (buffer-name b)))
    b))

(defun* projector-buffer-list (&optional (project (projector-root-safe)))
  (when project
    (loop for b being the buffers
          when (projector-buffer-file-name b)
          when (projector-child-p project it)
          when (projector-filter-buffer b)
          collect it)))


;;;; Tags.

(defun projector-list-tags (&optional file)
  (let ((file (or file buffer-file-name))
        (root (projector-root)))
    (mapcar #'tag-name
            (gethash (file-relative-name file root)
                     (projector-project-files-table (ensure-project))))))

(defun projector-list-open-tags ()
  "Return a list of tags in open files.
If the current buffer is in the project its tags should come
first."
  (let ((all (projector-files t))
        (open (projector-open)))
    (loop for f in open
          if (member (projector-uncompressed-file-name f) all)
          nconc (projector-list-tags (car it)))))

(eval-when-compile
  (defmacro push-tag-mark ()
    `(ring-insert
      find-tag-marker-ring
      (point-marker))))

(defstruct (tag
            (:constructor make-tag
                          (&key name file text line position
                                &aux (line (string-to-number line))
                                (position (string-to-number position)))))
  name file text line position)

(defun* projector-search-with-window (pattern &optional
                                              (offset 1000)
                                              (factor 3))
  "The search logic from `etags-goto-tag-location'.
Look for a match within a windw of OFFSET chars, growing by a
factor of FACTOR with each iteration."
  (do ((startpos (point))
       (offset offset (* factor offset))
       (found (looking-at pattern)
              (search-forward pattern (+ startpos offset) t)))
      ((or found (bobp)) found)
    (goto-char (- startpos offset))))

(defun projector-goto-tag-location (tag)
  (let ((pattern (tag-text tag)))
    (cond ((tag-position tag)
           (goto-char (tag-position tag)))
          ((tag-line tag)
           (goto-char (point-min))
           (forward-line (1- (tag-line tag)))))
    (if (projector-search-with-window pattern)
        (progn (search-forward pattern nil t)
               (goto-char (line-beginning-position)))
      (error "%s not found in %s" pattern buffer-file-name))))

(defun projector-goto-tag (tag)
  ;; (push-mark)
  (push-tag-mark)
  (set-buffer (find-file-noselect (expand-file-name
                                   (tag-file tag)
                                   (projector-root))))
  (projector-goto-tag-location tag))

(defun projector-read-tag (arg)
  (let* ((sap (symbol-at-point))
         (tag (let ((c (try-completion (symbol-name sap)
                                       (projector-tags-completion-table))))
                (when (or (stringp c) (eq c t))
                  c))))
    (if (or arg (null sap) (null tag))
        (projector-completing-read
         (if tag
             (format "Find tag (default %s): " tag)
           "Find tag: ")
         (projector-union
          (when tag
            (all-completions tag (projector-tags-completion-table)))
          (projector-list-open-tags)
          (all-completions "" (projector-tags-completion-table)))
         nil t)
      (symbol-name sap))))

(defun projector-find-tag-noselect (tag)
  (save-current-buffer
    (push-tag-mark)
    (if-let (matches (gethash tag (projector-tags-completion-table)))
        (if (cdr matches)
            (progn
              (message
               (substitute-command-keys
                "%d matches for %s; \\[pop-tag-mark] for next.")
               (length matches) tag)
              (while matches
                (projector-goto-tag (pop matches))
                (when (cdr matches) (push-tag-mark))))
          (projector-goto-tag (car matches)))
      (error "No matches for %s" tag))
    (current-buffer)))

(defun projector-find-tag (arg)
  "Jump to a definition."
  (interactive "P")
  (projector-assert-current-project)
  (switch-to-buffer
   (projector-find-tag-noselect
    (projector-read-tag arg))))

(defun projector-find-tag-other-window (arg)
  "Jump to a definition in another window."
  (interactive "P")
  (projector-assert-current-project)
  (switch-to-buffer-other-window
   (projector-find-tag-noselect
    (projector-read-tag arg))))

(defun projector-find-tag-other-frame (arg)
  "Jump to a definition in another frame."
  (interactive "P")
  (projector-assert-current-project)
  (switch-to-buffer-other-frame
   (projector-find-tag-noselect
    (projector-read-tag arg))))

(defun projector-pop-to-tag (arg)
  "Jump to a definition using `pop-to-buffer'."
  (interactive "P")
  (projector-assert-current-project)
  (pop-to-buffer
   (projector-find-tag-noselect
    (projector-read-tag arg))))

(defun projector-tags-apropos (pattern)
  "Show tags matching PATTERN."
  (let* ((table (projector-tags-completion-table))
         (completions (all-completions "" table))
         (matches (loop for c in completions
                        if (string-match pattern c)
                        collect (gethash c table)))
         (pt (with-current-buffer standard-output (point))))
    (dolist (tag matches)
      (princ (format "[%s]: " (abbreviate-file-name (tag-file tag))))
      (princ (tag-text tag))
      (when (string-prefix-p "(" (tag-text tag))
        (princ "...)"))
      (with-current-buffer standard-output
        (make-text-button pt (point)
                          'tag tag
                          'action (lambda (button)
                                    (projector-goto-tag
                                     (button-get button 'tag)))
                          'follow-link t
                          'face tags-tag-face
                          'type 'button))
      (terpri)
      (forward-line 1))))

(defun query-replace-in-project (from to &optional delimited)
  "Replace FROM with TO interactively."
  (interactive (query-replace-read-args "Query replace in project (regexp)" t t))
  (tags-query-replace from to delimited '(projector-files t)))

(defun projector-search (regexp)
  "Cycle through matches for REGEXP in project."
  (interactive "sSearch in project (regexp): ")
  (tags-search regexp '(projector-files t)))


;;;; Cycling project files.

(defun projector-advance-file (file files)
  "Get the next file from FILE in FILES."
  (let ((tail (member (projector-uncompressed-file-name file) files)))
    (find-file
     ;; Again, a compressed file will need its extension restored.
     (projector-compressed-file-name
      (expand-file-name
       (if (cdr tail) (cadr tail) (car files))
       (projector-root))))))

(defun projector-next-file (file)
  "Visit the next project file in cyclic order."
  (interactive (list (expand-file-name buffer-file-name)))
  (projector-assert-current-project)
  (projector-advance-file file
                          (sort (projector-files t) #'projector-file-lessp)))

(defun projector-previous-file (file)
  "Edit the previous project file in cyclic order."
  (interactive (list (expand-file-name buffer-file-name)))
  (projector-assert-current-project)
  (projector-advance-file file
                          (sort (projector-files t)
                                (lambda (f1 f2)
                                  (not (projector-file-lessp f1 f2))))))


;;;; Cycling project buffers.

(defun projector-next-buffer ()
  "Switch to the next project buffer in cyclic order."
  (interactive)
  (if (projector-root-safe)
      (let ((buffers (projector-buffer-list)))
        (when (second buffers)
          (bury-buffer (car buffers))
          (switch-to-buffer (second buffers))))
    (next-buffer)))

(defun projector-previous-buffer ()
  "Switch to the last project buffer in cyclic order."
  (interactive)
  (if (projector-root-safe)
      (let ((buffers (projector-buffer-list)))
        (when (second buffers)
          (bury-buffer (car buffers))
          (switch-to-buffer (second buffers))))
    (previous-buffer)))


;;;; Switching buffers and files.

(defun projector-dummy-buffer--uniquify (f d alist)
  "Draw from D to make F unique in ALIST and GIVENS."
  (do* ((depth 0 (+ 1 depth))
        (last nil maybe)
        (maybe f (uniquify-get-proposed-name f d depth)))
      ((equal last maybe) nil)
    (unless (or (get-buffer maybe) (assoc maybe alist))
      (return maybe))))

(defun projector-dummy-buffer--rationalize (files)
  "Return an alist whose keys are unique names for FILES."
  (if (featurep 'uniquify)
      (loop for file in files
            for f = (file-name-nondirectory file)
            for d = (directory-file-name
                     (file-name-directory file))
            collect (cons (projector-dummy-buffer--uniquify f d files) file))
    (loop with root = (projector-root)
          for file in files
          for f = (file-relative-name file root)
          collect (cons f file))))

(defun projector-dummy-buffer--alist ()
  "Return a pseudo-alist of unique names.
The leading elements are the buffer elements; the rest are conses
of (name . filename) for dummy buffers."
  (when-let (files (ignore-errors (projector-files t)))
    (projector-dummy-buffer--rationalize
     (loop for f in (projector-union
                     (projector-recent)
                     files)
           ;; Remove open files.
           unless (get-file-buffer f)
           collect f))))

(defun projector-read-buffer (prompt)
  (projector-assert-current-project)
  (let* ((dummies (projector-dummy-buffer--rationalize
                   (remove-if #'get-file-buffer
                              (projector-union (projector-recent)
                                               (projector-files t)))))
         (choice
          (projector-completing-read
           prompt
           (projector-union
            (mapcar #'buffer-name
                    (delete-if #'get-buffer-window (projector-buffer-list)))
            (mapcar #'car dummies))
           nil t))
         (buffer
          (or (get-buffer choice)
              (find-file-noselect (cdr (assoc choice dummies))))))
    buffer))

(defun projector-switch-to-buffer (buffer)
  "Select BUFFER, a buffer in project."
  (interactive (list (projector-read-buffer "Buffer: ")))
  (switch-to-buffer buffer))

(defun projector-kill-buffer (buffer)
  "Kill BUFFER, a buffer in project."
  (interactive (list (projector-read-buffer "Buffer: ")))
  (kill-buffer buffer))

(defun projector-insert-buffer (buffer)
  "Insert contents of BUFFER, a buffer in project."
  (interactive (list (projector-read-buffer "Buffer: ")))
  (with-no-warnings
    (insert-buffer buffer)))

(defun projector-switch-to-buffer-other-window (buffer)
  "Seelect BUFFER, a buffer in project."
  (interactive (list (projector-read-buffer "Buffer: ")))
  (switch-to-buffer-other-window buffer))

(defun projector-switch-to-buffer-other-frame (buffer)
  "Select BUFFER, a buffer in project."
  (interactive (list (projector-read-buffer "Buffer: ")))
  (switch-to-buffer-other-frame buffer))

(defun projector-read-file-name (prompt &rest _ignore)
  (let* ((dummies (projector-dummy-buffer--rationalize
                   (projector-union
                    (projector-not-recent)
                    (projector-recent))))
         (choice
          (projector-completing-read
           prompt
           (mapcar #'car dummies)
           nil t)))
    (or (when-let (b (get-buffer choice))
          (buffer-file-name b))
        (cdr (assoc choice dummies)))))

(defun projector-find-file (file)
  "Visit a project file.
List files not recently accessed first."
  (interactive (list (projector-read-file-name "Find file: ")))
  (switch-to-buffer (find-file-noselect file)))

(defun projector-find-alternate-file (file)
  "Like `find-alternate-file' but choose from project files."
  (interactive (list (projector-read-file-name "Find alternate file: ")))
  (find-alternate-file file))

(defun projector-insert-file (file)
  "Like `insert-file' but choose from project files."
  (interactive (list (projector-read-file-name "Insert file: ")))
  (with-no-warnings
    (insert-file file)))

(defun projector-find-file-other-window (file)
  "Visit a project file in another window.
List files not recently accessed first."
  (interactive (list (projector-read-file-name "Find file: ")))
  (switch-to-buffer-other-window (find-file-noselect file)))

(defun projector-find-file-other-frame (file)
  "Visit a project file in another frame.
List files not recently accessed first."
  (interactive (list (projector-read-file-name "Find file: ")))
  (switch-to-buffer-other-frame (find-file-noselect file)))


;;;; Occur in project.

(declare-function grep-read-regexp "grep")

(defun occur-in-project (regexp &optional nlines)
  "Show all lines in project matching REGEXP.

Quickly scans the entire project for any files containing matches
for REGEXP and passes the list of matches on to `multi-occur'.

This is analogous to `grep-in-project', but it does everything in
Emacs, without any external dependencies."
  (interactive (list
                (read-regexp "List lines matching regexp"
                             (when (symbol-at-point)
                               (regexp-quote
                                (symbol-name (symbol-at-point)))))
                (prefix-numeric-value current-prefix-arg)))
  (multi-occur
   (let (buffers)
     (projector-map-fileset
      (lambda (file)
        (when (re-search-forward regexp nil t)
          ;; We could is opening a lot of files, so just ignore any
          ;; unsafe local variables.
          (let* ((enable-local-variables :safe)
                 (file (ignore-errors (or (get-file-buffer file)
                                          (find-file-noselect file t)))))
            (push file buffers))))
      (projector-files t))
     (nreverse buffers))
   regexp nlines))

(defun count-matches-in-project (regexp)
  "How many matches for REGEXP in project?"
  (interactive (list (read-regexp "Count matches for")))
  (let ((count 0))
    (projector-map-fileset
     (lambda (_file)
       (incf count (count-matches regexp (point-min) (point-max))))
     (projector-files t))
    (when (called-interactively-p 'interactive)
      (message "I count %d occurrences of \"%s\" in this project."
               count regexp))
    count))

(defun count-words-in-project ()
  "How many words, lines, and characters in project?"
  (interactive)
  (let ((words 0)
        (lines 0)
        (chars 0))
    (projector-map-fileset
     (lambda (_file)
       (incf words (count-words (point-min) (point-max)))
       (incf lines (count-lines (point-min) (point-max)))
       (incf chars (point-max)))
     (projector-files t))
    (message "Project has %d line%s, %d word%s, and %d character%s."
             lines (if (= lines 1) "" "s")
             words (if (= words 1) "" "s")
             chars (if (= chars 1) "" "s"))))

(defalias 'count-lines-in-project 'count-words-in-project)

(defun count-definitions-in-project ()
  "How many symbols are defined in project?"
  (interactive)
  (let ((count (length (all-completions "" (projector-tags-completion-table)))))
    (when (called-interactively-p 'interactive)
      (message "Project has %d definition%s."
               count (if (> count 1) "s" ""))
      count)))

(defun count-files-in-project ()
  "How many files in project?"
  (interactive)
  (let ((count (length (projector-files))))
    (when (called-interactively-p 'interactive)
      (message "Project has %d file%s."
               count (if (> count 1) "s" ""))
      count)))

(defun code-tags-in-project ()
  "Show all TODO, FIXME, &c. comments in project.

The list of code tags to search for is determined by
`projector-code-tags'."
  (interactive)
  (let ((search-upper-case t))
    (occur-in-project
     (regexp-opt
      (mapcar #'regexp-quote
              projector-code-tags)
      'words))))

(defalias 'todo-in-project 'code-tags-in-project)

(defun projector-run-grep (regexp root)
  "Run `grep' with REGEXP across project."
  (let* ((default-directory root)
         (projector-find-action
          ;; Use `zgrep' since etags includes compressed files.
          (format "zgrep %s -nH -e %s"
                  (when case-fold-search "-i")
                  (shell-quote-argument regexp))))
    (grep ;; Hide the command.
     (propertize (projector-find-command) 'invisible t))))

(defun grep-in-project (regexp)
  "Grep project for REGEXP."
  (interactive
   (progn
     (require 'grep)
     (projector-assert-current-project)
     (list (grep-read-regexp))))
  (projector-assert-current-project)
  (grep-compute-defaults)
  (let* ((root (projector-root))
         (default-directory root))
    (with-current-buffer (projector-run-grep regexp root)
      ;; Annex the buffer to its project.
      (setq list-buffers-directory root))))


;;;; Read-only projects.

(defun projector-root-regexp (root)
  "Return pattern matching only filenames that start with ROOT."
  (concat (rx string-start)
          (regexp-quote root)))

(defun toggle-project-read-only (&optional arg)
  "Change whether project is read-only.
Called once, this sets all buffers currently visiting files in
the project read-only and makes the entire project appear
unwritable.

Called twice, it makes the buffers and the project appear
writable again.

This only works when the project is writable in the first place."
  (interactive "P")
  (projector-assert-current-project)
  ;; Lest a modified buffer be set read-only.
  (save-some-buffers
   nil
   `(lambda () (memq (current-buffer) ,'(projector-buffer-list))))
  (let* ((regexp (projector-root-regexp (projector-root)))
         (handler (assoc regexp file-name-handler-alist)))
    ;; The directory must be writable in the first place.
    (unless (or handler (file-writable-p (projector-root)))
      (signal 'unwritable-project nil))
    ;; If the handler isn't set, or if ARG is greater than zero, we
    ;; are setting the project read-only. Otherwise we are making it
    ;; writable again.
    (let ((read-only (if arg
                         (> (prefix-numeric-value arg) 0)
                       (null handler))))
      (if read-only
          (add-to-list 'file-name-handler-alist
                       (cons regexp 'projector-handle-file-writable-p))
        (setq file-name-handler-alist (delete handler file-name-handler-alist)))
      (dolist (b (projector-buffer-list))
        (if (buffer-file-name b)
            (with-current-buffer b
              (with-no-warnings         ;We want view-mode to come on.
                (toggle-read-only (if read-only 1 0))))))
      (message "%s" (if read-only
                        "Project is read-only"
                      "Project is writable")))))

(defun projector-handle-file-writable-p (operation &rest _args)
  "Like `file-writable-p', but always return nil."
  (if (eq operation 'file-writable-p) nil (error "Bad operation")))

;; Only call the handler to determine whether a file is writable.
(put 'projector-handle-file-writable-p 'operations '(file-writable-p))

(defun projector-remove-handler (root)
  "Unconditionally remove handlers for ROOT."
  (setq file-name-handler-alist
        (delete (cons (projector-root-regexp root)
                      'projector-handle-file-writable-p)
                file-name-handler-alist)))

(defun projector-unload-handlers ()
  "Remove any file name handlers Projector has set."
  (setq file-name-handler-alist
        (rassq-delete-all 'projector-handle-file-writable-p
                          file-name-handler-alist)))


;;;; Dired in project.

;; TODO With remote projects the buffer includes the shell prompt.
(defun projector-dired-buffer (&optional revert)
  "Return a buffer listing project with `dired'."
  (projector-assert-current-project)
  (let* ((root (projector-root))
         (name (format "*%s*" (file-name-nondirectory
                               (directory-file-name root)))))
    ;; Just switch to the buffer if it exists (unless we are
    ;; reverting).
    (if (and (null revert) (get-buffer name))
        (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          ;; The following is hacked from `find-dired'.
          (require 'find-dired)
          (erase-buffer)
          (insert "  " root ":\n")
          ;; Alas, the listings don't align properly using -h.
          (dired-mode root (or dired-actual-switches "-ld"))
          (setq default-directory root
                list-buffers-directory root)
          ;; For project specific exclusions.
          (hack-dir-local-variables-non-file-buffer)
          (let* ((projector-find-type "") ;Include directories.
                 ;; Using `dired-actual-switches' makes the listing
                 ;; sortable.
                 (projector-find-action (concat "ls " dired-actual-switches))
                 (proc (projector-start-find-process
                        "list project" (current-buffer))))
            (set (make-local-variable 'mode-line-process) '(":%s"))
            ;; No need to reinvent the wheel.
            (set-process-filter    proc 'find-dired-filter)
            (set-process-sentinel  proc 'find-dired-sentinel)
            ;; Initialize the process marker; it is used by the filter.
            (move-marker (process-mark proc) 1 (current-buffer))
            (set-process-query-on-exit-flag proc nil)
            (set (make-local-variable 'dired-subdir-alist)
                 (list (cons root (point-min-marker))))
            (set (make-local-variable 'revert-buffer-function)
                 (lambda (_ignore-auto _noconfirm) (projector-dired-buffer t)))
            ;; (dired-advertise)
            ;; Return current buffer.
            (current-buffer)))))))

(defun edit-project ()
  "Show entire project in a `dired' buffer."
  (interactive)
  (switch-to-buffer (projector-dired-buffer)))

(defun edit-project-other-window ()
  (interactive)
  (switch-to-buffer-other-window (projector-dired-buffer)))

(defun edit-project-other-frame ()
  (interactive)
  (switch-to-buffer-other-frame (projector-dired-buffer)))

(defun display-project ()
  "Edit project in another window."
  (interactive)
  (display-buffer (projector-dired-buffer)))

(defun projector-show-file-buffer ()
  (if (derived-mode-p 'dired-mode)
      (when-let (fname (dired-file-name-at-point))
        (or (get-file-buffer fname)
            (find-file-noselect fname)))
    (let ((filename
           (file-relative-name
            (or buffer-file-name
                (when list-buffers-directory
                  (directory-file-name list-buffers-directory)))
            (projector-root))))
      (with-current-buffer (projector-dired-buffer)
        (projector-wait nil "Listing project...")
        (goto-char (point-min))
        (if (re-search-forward (concat " " filename (rx line-end)) nil t)
            (dired-move-to-filename))
        (current-buffer)))))

(defun projector-show-file ()
  "Given a file buffer, jump to its listing.
Given a listing, jump to buffer of the file at point.

This is like `dired-jump', but calling it twice takes you back
where you started."
  (interactive)
  (pop-to-buffer (projector-show-file-buffer)))

(defun projector-show-file-other-window ()
  "Like `projector-show-file', but in other window."
  (interactive)
  (switch-to-buffer-other-window (projector-show-file-buffer)))

(defun projector-show-file-other-frame ()
  "Like `projector-show-file', but in other frame."
  (interactive)
  (switch-to-buffer-other-frame (projector-show-file-buffer)))


;;;; Commands that act on the project.

(defun projector-open-projects ()
  (delq nil (mapcar
             (lambda (b)
               (with-current-buffer b
                 (when projector-project
                   (projector-root-safe))))
             (buffer-list))))

(defun projector-project-alist ()
  (projector-discover)
  (projector-dummy-buffer--rationalize
   (mapcar #'directory-file-name
           (projector-union
            (projector-open-projects)
            (projector-roots)))))

(defun projector-read-project (prompt)
  (let ((roots (projector-project-alist)))
    (file-name-as-directory
     (cdr
      (assoc
       (projector-completing-read
        prompt
        (sort (mapcar #'car roots)
              (lambda (a b)
                ;; Sometimes project names (e.g. in Quicklisp) include
                ;; dates or version numbers; put the newest ones
                ;; first.
                (not (string-lessp a b))))
        nil t)
       roots)))))

(defun find-project-noselect (root)
  (or (car (projector-buffer-list root))
      (let ((buffer-file-name root))
        (projector-dired-buffer))))

(defun find-project (root)
  "Visit another project."
  (interactive (list (projector-read-project "Project: ")))
  (prog1 (switch-to-buffer (find-project-noselect root))
    (delete-other-windows)))

(defun swap-projects (which)
  "Switch back to the last project in the stack."
  (interactive "p")
  (switch-to-buffer
   (find-project-noselect
    (if (projector-root-safe)
        (nth which (projector-open-projects))
      (first (projector-open-projects))))))

(defun find-project-other-window (root)
  "Visit another project in another window."
  (interactive (list (projector-read-project "Project: ")))
  (switch-to-buffer-other-window (find-project-noselect root)))

(defun find-project-other-frame (root)
  "Visit another project in another frame."
  (interactive (list (projector-read-project "Project: ")))
  (switch-to-buffer-other-frame (find-project-noselect root)))

(defun forget-project (project)
  "Forget PROJECT."
  (interactive (list (projector-root)))
  (projector-remove-handler project)
  (setf (gethash project projector-known-roots) :forget
        (gethash project projector-projects) nil)
  (message "Forgot project %s" project))

(defun load-project (y/n)
  "Load every file in project."
  (interactive
   (progn
     (projector-assert-current-project)
     (list (y-or-n-p
            (format "Open all %d files in %s? "
                    (length (projector-files t))
                    (abbreviate-file-name (projector-root)))))))
  (when y/n
    (mapc 'find-file-noselect (projector-files t))
    (list-buffers-in-project)))

(defun bury-project (root &optional replace)
  "Dismiss project from your sight."
  (interactive (list (projector-root) t))
  (let ((buffers (projector-buffer-list root)))
    (mapc 'bury-buffer (reverse buffers))
    (when replace
      (mapc 'replace-buffer-in-windows buffers))))

(defalias 'hide-project 'bury-project)

(defun projector-save-project ()
  "Check project for buffers that need saving."
  (interactive)
  (save-some-buffers
   nil
   `(lambda () (memq (current-buffer) ',(projector-buffer-list)))))

(defun kill-project (yes)
  "Kill every project buffer."
  (interactive
   (progn
     (projector-assert-current-project)
     (list (y-or-n-p
            (format "Kill all %d buffers in project %s? "
                    (length (projector-buffer-list))
                    (abbreviate-file-name (projector-root)))))))
  (when yes
    (projector-save-project)
    (mapc 'kill-buffer (projector-buffer-list))))

(defalias 'close-project 'kill-project)


;;;; Multi-isearch

(defun isearch-in-project ()
  "Start incremental search.
Search open files first."
  (interactive)
  (projector-assert-current-project)
  (let ((multi-isearch-pause nil))
    (multi-isearch-files
     (projector-union
      (projector-open)
      (projector-recent)
      (projector-files t)))))

(defun isearch-regexp-in-project ()
  "Start incremental regex search.
Search open files first."
  (interactive)
  (projector-assert-current-project)
  (multi-isearch-files-regexp
   (projector-union
    (projector-open)
    (projector-recent)
    (projector-files t))))



;;;; Trivial commands.
(defun list-buffers-in-project ()
  "Like `list-buffers' in project."
  (interactive)
  (projector-assert-current-project)
  (pop-to-buffer (list-buffers-noselect nil (projector-buffer-list))))

(defun vc-in-project ()
  "Show version control status of project.
Cf. `vc-dir'."
  (interactive)
  (projector-assert-current-project)
  (vc-dir (projector-root)))

;;;; Projector mode.

(defvar projector-map
  (let ((map (make-sparse-keymap)))
    (define-key map "4"     (make-sparse-keymap))
    (define-key map "5"     (make-sparse-keymap))
    (define-key map "\C-j"  'projector-show-file)
    (define-key map "4\C-j" 'projector-show-file-other-window)
    (define-key map "5\C-j" 'projector-show-file-other-frame)
    (define-key map "\C-k"  'kill-project)
    (define-key map "\C-q"  'toggle-project-read-only)
    (define-key map "\C-b"  'list-buffers-in-project)
    (define-key map "."     'find-tag)
    (define-key map "4."    'find-tag-other-window)
    (define-key map "5."    'find-tag-other-frame)
    (define-key map "a"     'projector-search)
    (define-key map "b"     'projector-switch-to-buffer)
    (define-key map "4b"    'projector-switch-to-buffer-other-window)
    (define-key map "5b"    'projector-switch-to-buffer-other-frame)
    (define-key map "d"     'edit-project)
    (define-key map "4d"    'edit-project-other-window)
    (define-key map "5d"    'edit-project-other-frame)
    (define-key map "f"     'projector-find-file)
    (define-key map "\C-f"  'projector-find-file)
    (define-key map "4f"    'projector-find-file-other-window)
    (define-key map "4\C-f" 'projector-find-file-other-window)
    (define-key map "5f"    'projector-find-file-other-frame)
    (define-key map "5\C-f" 'projector-find-file-other-frame)
    (define-key map "g"     'grep-in-project)
    (define-key map "k"     'hide-project)
    (define-key map "l"     'projector-list-tags)
    (define-key map "n"     'new-project)
    (define-key map "o"     'occur-in-project)
    (define-key map "\C-o"  'display-project)
    (define-key map "p"     'find-project)
    (define-key map "4p"    'find-project-other-window)
    (define-key map "5p"    'find-project-other-frame)
    (define-key map "q"     'query-replace-in-project)
    (define-key map "r"     'projector-discover)
    (define-key map "t"     'projector-todo-list)
    (define-key map "u"     'update-project)
    (define-key map "v"     'vc-in-project)
    (define-key map "x"     'projector-next-buffer)
    (define-key map "z"     'projector-previous-buffer)
    (define-key map "\C-s"  'isearch-in-project)
    (define-key map "\M-s"  'isearch-regexp-in-project)
    (define-key map "\C-\M-s" 'isearch-regexp-in-project)
    map))

(defvar projector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap find-tag] 'projector-pop-to-tag)
    (define-key map [remap find-tag-other-window] 'projector-find-tag-other-window)
    (define-key map [remap find-tag-other-frame] 'projector-find-tag-other-frame)
    (define-key map [next-buffer] 'projector-next-buffer)
    (define-key map [previous-buffer] 'projector-previous-buffer)
    map)
  "Keymap for `projector-mode'.")

(defvar projector-menu)
(easy-menu-define projector-menu projector-mode-map "Projector"
  `("Projector"
    :visible (or buffer-file-name list-buffers-directory)
    ["Update Project" projector-update :visible (projector-root-safe)]
    ["New Project" projector-update :visible (not (projector-root-safe))]
    "----"
    ["List Project Buffers" projector-list-buffers (projector-root-safe)]
    ["Edit Project" edit-project (projector-root-safe)]
    "----"
    ["Next Project File" projector-next-file (projector-root-safe)]
    ["Previous Project File" projector-previous-file (projector-root-safe)]
    ["Next Project Buffer" projector-next-buffer (projector-root-safe)]
    ["Previous Project Buffer" projector-previous-buffer (projector-root-safe)]
    "----"
    ("Search in Project" :enable (projector-root-safe)
     ["Search Project..." projector-search (projector-root-safe)]
     ["Grep Project..." grep-in-project (projector-root-safe)]
     ["Incremental Search..." isearch-in-project]
     ["Incremental Regexp Search..." isearch-regexp-in-project])
    ["Replace In Project..." query-replace-in-project (projector-root-safe)]
    ["Occur in Project" occur-in-project (projector-root-safe)]
    "----"
    ["Project VC Status" vc-in-project]
    ["Make Project Read-Only" toggle-project-read-only
     :visible (when buffer-file-name (file-writable-p (buffer-file-name)))]
    ["Make Project Writable" toggle-project-read-only
     :visible (when buffer-file-name (not (file-writable-p (buffer-file-name))))]
    "----"
    ["Switch Project..." find-project]))

(easy-menu-add projector-menu projector-mode-map)

(defun projector-apropos ()
  "Show all Projector commands, their bindings and documentation."
  (interactive)
  (require 'apropos)
  (let ((apropos-do-all nil)
        (%apropos-print (symbol-function 'apropos-print)))
    (labels ((projector-command-p (symbol)
                                  (and (commandp symbol)
                                       (equal (symbol-file symbol)
                                              (feature-file 'projector)))))
      (letf (((symbol-function 'apropos-print)
              (lambda (&rest args)
                (setq apropos-accumulator
                      (delete-if-not #'projector-command-p
                                     apropos-accumulator
                                     :key #'car))
                (apply %apropos-print args))))
        (apropos-command "^projector-\\|-project$" nil)))))

;;;###autoload
(defalias 'apropos-projector 'projector-apropos)

;;;###autoload
(define-minor-mode projector-mode
  "Minor mode for Projector projects.
Make project commands available globally and display the current
project in the mode line of project files.

Commands:

\\{projector-mode-keymap}
 "
  :group 'projector
  :global t
  :lighter (:eval (projector-mode-line-name))
  :keymap projector-mode-map
  :require 'projector
  (if projector-mode
      (projector-load-hook)
    (progn
      (projector-unload-hook)
      (clrhash projector-projects)))
  (setq default-tags-table-function (when projector-mode 'projector-table)))

(dolist (x '(no-project
             projector-busy
             unwritable-project))
  (add-to-list 'debug-ignored-errors x))

(provide 'projector)
;;; projector.el ends here
