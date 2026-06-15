;;; git-package.el --- Install Emacs packages via git -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Requires: ((git "1.7.2.3"))
;; Homepage: https://github.com/mnewt/git-package
;; Keywords: config package git


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package adds support for installing packages directly from git
;; repositories.

;; For more information, see README.org.

;;; Code:

(require 'seq)
(require 'cl-seq)
(require 'package)
(require 'info)


;;; Variables

(defcustom git-package-user-dir (expand-file-name "git" user-emacs-directory)
  "Directory containing the user's git packages."
  :group 'package
  :type 'string)

(defcustom git-package-load-autoloads nil
  "If non-nil, then load the autoloads file when activating a package.

This is not always necessary. Once the package is on `load-path'
and/or `custom-theme-load-path', the package can be required,
autoloads can be added manually. `use-package' can require the
package and/or generate autoloads using keywords such as
`:commands'."
  :group 'package
  :type 'boolean)

(defvar git-package--packages nil
  "Alist specifying packages ensured by `git-package'.

CAR is the package's local name as a symbol.

CDR is a Plist that contains the information needed to fetch the
package via git.")

(defvar git-package--read-package-history nil
  "History for `git-package-upgrade' command.")

(defvar git-package--byte-compile-ignore '("^\\..*" ".*-\\(pkg\\|autoloads\\)\\.el\\'")
  "Ignore these files during byte compilation.

This is a list of regular expressions.")

(defvar git-package--buffer "*git-package*"
  "Activity log for `git-package'.")


;;; Functions

(defun git-package--shell-command (command)
  "Run COMMAND and format the output in `git-package-buffer'."
  (with-current-buffer git-package--buffer
    (insert " > " command)
    (let ((shell-command-dont-erase-buffer t))
      (shell-command command git-package--buffer))
    (insert "\n" (make-string 80 ?-) "\n")))

(defun git-package--absolute-dir (dir)
  "Ensure DIR is an absolute path."
  (if (file-name-absolute-p dir)
      dir
    (expand-file-name dir git-package-user-dir)))

(defun git-package--dirty-p (&optional dir)
  "Return non-nil if the git repo DIR is dirty."
  (not
   (= 0 (length (shell-command-to-string
                 (format "git -C '%s' status --porcelain"
                         (expand-file-name (or dir default-directory))))))))

(defun git-package--package-names ()
  "List the package names activated by `git-package'."
  (mapcar (lambda (p) (symbol-name (car p))) git-package--packages))

(defun git-package--byte-compile (dir files)
  "Byte compile FILES in DIR.

FILES is a list of relative paths to .el files. Wildcards will be
expanded."
  (let ((default-directory dir))
    (dolist (file (seq-mapcat #'file-expand-wildcards files))
      (unless (seq-some (lambda (re) (string-match-p re file))
                        git-package--byte-compile-ignore)
        (save-window-excursion
          (with-demoted-errors (byte-compile-file file t)))))))

(defun git-package--add-info-nodes (dir)
  "If they exist, add Info nodes from DIR.

This is the way `package.el' does it."
  (when (file-exists-p (expand-file-name "dir" dir))
    (require 'info)
    (info-initialize)
    (push dir Info-directory-list)))

(defun git-package--read-package (prompt)
  "PROMPT the user for a package name.

Return the symbol"
  (let ((package-names (mapcar (lambda (p) (symbol-name (plist-get (cdr p) :name)))
                               git-package--packages)))
    (intern (completing-read
             (or prompt "Git package: ") package-names nil t
             ;; Pre-select the current project if it was installed with
             ;; `git-package'.
             (when-let* ((project-dir (locate-dominating-file default-directory ".git"))
                         (package (file-name-base (directory-file-name project-dir)))
                         (member-p (member package package-names)))
               package)
             git-package--read-package-history))))

(defun git-package--normalize (config &optional name)
  "Turn CONFIG into a normalized Plist.

CONFIG is a string or Plist.

NAME is a symbol."
  (let* ((config (cond
                  ((stringp config) (list :url config))
                  ((consp config)
                   (cond
                    ((stringp (car config))
                     (apply #'list :url (car config) (cdr config)))
                    ((keywordp (car config))
                     config)
                    (:else
                     (setq name (car config))
                     (cdr config))))))
         (url (plist-get config :url))
         (dir (or (plist-get config :dir)
                  (replace-regexp-in-string "\\.git\\'" ""
                                            (file-name-nondirectory url))))
         (files (let ((d (plist-get config :files)))
                  (cond
                   ((stringp d) (list d))
                   ((consp d) d)
                   ((not d) '("*.el")))))
         ;; When no :ref is given, leave it nil so a fresh clone stays on the
         ;; remote's default branch (main, master, or otherwise).
         (ref (plist-get config :ref))
         (name (or name (plist-get config :name) (intern dir))))
    ;; Validate and assemble the config plist.
    (if (and name
             (symbolp name)
             (stringp url)
             (stringp dir)
             (listp files)
             (or (null ref) (stringp ref)))
        (list :name name
              :url url
              :dir dir
              :files files
              :ref ref)
      (user-error "CONFIG is not a string or a Plist with a :url key"))))

(defun git-package--install (config)
  "Install the package described by CONFIG."
  (let* ((name (plist-get config :name))
         (default-directory (git-package--absolute-dir (plist-get config :dir)))
         (wc (current-window-configuration))
         (pkg-desc (progn (dired default-directory)
                          (with-demoted-errors (package-dir-info)))))
    (when (package-desc-p pkg-desc)
      ;; Download and install the dependencies using `package.el' if the above
      ;; step successfully created a `package-desc'.
      (let* ((requires (package-desc-reqs pkg-desc))
             (transaction (package-compute-transaction nil requires)))
        (package-download-transaction transaction)))
    (when-let ((command (plist-get config :command)))
      (compile command))
    (package-generate-autoloads name default-directory)
    (git-package--byte-compile default-directory (plist-get config :files))
    (git-package--add-info-nodes default-directory)
    (set-window-configuration wc)))

(defun git-package--activate (config)
  "Activate the package described by CONFIG."
  (let* ((name (plist-get config :name))
         (name-string (symbol-name name))
         (dir (expand-file-name (plist-get config :dir) git-package-user-dir)))
    ;; Track the package
    (add-to-list 'git-package--packages (cons name config))
    ;; Add all directories specified by :files to the `load-path'.
    (dolist (file (plist-get config :files))
      (add-to-list 'load-path (file-name-directory (expand-file-name file dir))))
    ;; KLUDGE: Add to `custom-theme-load-path' if we have a theme. All themes
    ;; end in `-theme' or `theme.el', right?
    (when (or (string-suffix-p "-theme" name-string)
              (string-suffix-p "-theme.el" name-string))
      (add-to-list 'custom-theme-load-path dir))
    ;; Load autoloads if we are instructed to do so.
    (when git-package-load-autoloads
      (load (expand-file-name (concat name-string "-autoloads.el") dir) t t))))

(defun git-package-ensure (config)
  "Ensure that a git package described by CONFIG is installed."
  (let* ((config (git-package--normalize config))
         (name (plist-get config :name))
         (dir (expand-file-name (plist-get config :dir) git-package-user-dir)))
    (unless (file-exists-p dir)
      (message "git-package is cloning package %s..." name)
      (shell-command (format "git -C '%s' clone '%s' '%s'"
                             git-package-user-dir
                             (plist-get config :url)
                             dir)
                     "*git-package*")
      ;; Check out the ref (should work for branch, hash, or tag).
      (when-let (ref (plist-get config :ref))
        (shell-command (format "git -C %s checkout %s" dir ref) "*git-package*"))
      (git-package--install config))
    (git-package--activate config)
    name))


;;; Commands

;;;###autoload
(defun git-package (&rest config)
  "Ensure that the git package described by CONFIG is installed.

Add the package to the `load-path', and, if it's a theme, to
`custom-theme-load-path'.

Load the package's autoloads if `git-package-autoloads' is non-nil.

CONFIG is a string or Plist with at least a :url key."
  (git-package-ensure (git-package--normalize config)))

;;;###autoload
(defun git-package-delete (config)
  "Delete package described by CONFIG."
  (interactive (list (completing-read "Delete package: "
                                      (git-package--package-names)
                                      nil t)))
  (delete-directory (expand-file-name (plist-get config :dir))))

;;;###autoload
(defun git-package-delete-unused ()
  "Delete unused packages in `git-package-user-dir'.

Unused packages are defined as directories on disk in the
`git-package-user-dir' that have not been activated in the
current Emacs session using `git-package'."
  (interactive)
  (let* ((active (mapcar (lambda (p) (plist-get (cdr p) :dir))
                         git-package--packages))
         (on-disk (directory-files git-package-user-dir nil "^[^.]+.*" t))
         (unused (cl-set-difference on-disk active :test #'string=)))
    (when (yes-or-no-p (format "Delete packages: %s? " unused))
      (dolist (dir unused)
        (message "Deleting git package: %s..." dir)
        (delete-directory (expand-file-name dir git-package-user-dir) t t))
      (message "Done."))))

;;;###autoload
(defun git-package-install (url)
  "Install a PACKAGE from a git URL."
  (interactive "sInstall package from git url: ")
  (let ((name (git-package url)))
    (message "Package %s is installed." (symbol-name name))))
  
;;;###autoload
(defun git-package-reinstall (package)
  "Re-install PACKAGE.

You may want to re-install the package after you modify the source files.

Note that this does not fetch changes from the git repository if
the package if it is already installed. For that, use
`git-package-upgrade'."
  (interactive (list (git-package--read-package "Reinstall git package: ")))
  (let ((config (alist-get package git-package--packages)))
    (git-package--install config)
    (git-package--activate config)))

;;;###autoload
(defun git-package-upgrade (package)
  "Upgrade PACKAGE.

PACKAGE is a symbol, which should be a key in `git-package--packages'.

Checkout the :ref, fetch changes, and reinstall the package."
  (interactive (list (git-package--read-package "Upgrade git package: ")))
  (let* ((config (alist-get package git-package--packages))
         (default-directory (expand-file-name (plist-get config :dir)
                                              git-package-user-dir)))
    (message "git-package upgrading package %s..." (plist-get config :name))
    ;; Delete automatically generated files so the repo doesn't appear dirty (at
    ;; least not because of `git-package').
    (shell-command "rm -f *.elc *-pkg.el *-autoloads.el" "*git-package*")
    (when (or (not (git-package--dirty-p))
              (while (cl-case (downcase
                               (read-key (concat
                                          "The package `"
                                          (symbol-name (plist-get config :name))
                                          "' with local repo at ["
                                          default-directory
                                          "] is dirty."
                                          " Choose an action:\n"
                                          "[R]eset to HEAD and continue"
                                          " (changes will be lost!)\n"
                                          "[S]kip fetching and continue\n"
                                          "[A]bort\n"
                                          "? ")))
                       (?r (unless (= 0 (shell-command "git reset HEAD --hard"))
                             (error "Resetting the repo at %s failed"
                                    default-directory)))
                       (?s nil)
                       (?a (user-error "Aborted package upgrade"))
                       (_ t))))
      (shell-command (format "git checkout %s" (plist-get config :ref))
                     "*git-package*")
      (shell-command "git fetch" "*git-package*"))
    (git-package--install config)))

;;;###autoload
(defun git-package-upgrade-all-packages ()
  "Upgrade all git ensured packages."
  (interactive)
  (message "Upgrading all git-package packages...")
  (dolist (package git-package--packages)
    (git-package-upgrade (car package)))
  (message "Upgrading all git-package packages...done."))


(provide 'git-package)

;;; git-package.el ends here
