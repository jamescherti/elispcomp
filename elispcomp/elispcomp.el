;;; elispcomp.el --- Compile Elisp code from the command-line -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1
;; URL: https://github.com/jamescherti/elispcomp
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Lisp code that recursively byte-compile and native-compile .el files.

;;; Code:

(defun elispcomp-load-el (filename)
  "Execute the Elisp code in the file FILENAME located in the home directory."
  (let ((user-init-file
         (expand-file-name filename "~")))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t)
      (message "[INFO] Load Elisp code: %s" user-init-file))))

(defun elispcomp-add-load-paths (paths-string)
  "Add directories specified in PATHS-STRING to `load-path`.
PATHS-STRING should be a string containing directories separated by newline
characters."
  (let ((paths (split-string paths-string "\n" t)))
    (dolist (path paths)
      (let ((default-directory (expand-file-name path)))
        (normal-top-level-add-subdirs-to-load-path)))))

(defun elispcomp-getenv (name)
  "Ensure the environment variable NAME is declared and return its value.
If the environment variable is not declared, signal an error."
  (let ((value (getenv name)))
    (unless value
      (error "The environment variable %s is not declared" name))
    value))

(defun elispcomp-byte-and-native-compile-load-path ()
  "Byte compile and native compile all paths in `load-path'."
  (dolist (path load-path)
    (when (file-directory-p path)
      (message "[BYTE-COMPILE] %s" path)
      (byte-recompile-directory path 0)
      (message "[NATIVE-COMPILE] %s" path)
      (native-compile-async path t))))

(let* ((default-directory (elispcomp-getenv "EMACS_BYTE_COMP_DIR"))
       (load-path-list (elispcomp-getenv "EMACS_LOAD_PATH_LIST"))
       (eln-cache-dir (elispcomp-getenv "EMACS_ELN_CACHE_DIR"))
       (jobs (elispcomp-getenv "EMACS_NATIVE_COMP_ASYNC_JOBS_NUMBER"))
       (compile-initial-load-paths-p
        (/= 0 (string-to-number
               (elispcomp-getenv "EMACS_COMPILE_INITIAL_LOAD_PATHS"))))
       (ensure-native-comp-available
        (/= 0 (string-to-number (elispcomp-getenv "EMACS_ENSURE_NATIVE_COMP_AVAILABLE"))))
       (byte-comp-enabled
        (/= 0 (string-to-number (elispcomp-getenv "EMACS_BYTE_COMP_ENABLED"))))
       (native-comp-enabled
        (/= 0 (string-to-number (elispcomp-getenv "EMACS_NATIVE_COMP_ENABLED"))))
       (native-comp-available (and (featurep 'native-compile)
                                   (fboundp 'native-comp-available-p)
                                   (native-comp-available-p))))
  ;; Say no to interactive questions (e.g., vterm asks the user to compile the
  ;; shared library)
  (fset 'y-or-n-p #'(lambda(&rest _args) nil))

  (when (and native-comp-enabled
             ensure-native-comp-available
             (not native-comp-available))
    (error "Native comp is not available"))

  ;; SET ELN-CACHE DIR
  (if (and native-comp-enabled
           native-comp-available
           (not (string= eln-cache-dir "")))
      (cond ((fboundp 'startup-redirect-eln-cache) ; Emacs >= 28
             (startup-redirect-eln-cache eln-cache-dir))
            ((boundp 'native-comp-eln-load-path)
             (setcar native-comp-eln-load-path
                     (expand-file-name
                      (convert-standard-filename eln-cache-dir)
                      user-emacs-directory)))
            (t (error "Cannot change the eln-cache-dir directory"))))
  (when byte-comp-enabled
    (setq no-byte-compile nil)
    (setq byte-compile-verbose t)
    (setq byte-compile-warnings t))

  (when (and native-comp-enabled native-comp-available)
    (when (and jobs (not (string= jobs "")))
      (setq native-comp-async-jobs-number (string-to-number jobs)))
    (setq native-comp-async-report-warnings-errors t)
    (setq native-comp-warning-on-missing-source t)
    (setq native-comp-verbose 0)
    (setq native-comp-deferred-compilation t)
    (setq native-comp-jit-compilation t)
    (setq package-native-compile nil))

  ;; SHOW MESSAGES
  (message "[INFO] Recursively compile the directory: %s"
           default-directory)
  (message "[INFO] Byte comp enabled: %s" byte-comp-enabled)
  (message "[INFO] Native comp enabled: %s" native-comp-enabled)
  (message "[INFO] Ensure native comp available: %s"
           ensure-native-comp-available)
  (message "[INFO] Jobs: %s" jobs)
  (message "[INFO] Emacs user directory: %s" user-emacs-directory)
  (message "[INFO] eln-cache directory: %s" eln-cache-dir)
  (message "[INFO] Load path directories:\n%s" load-path-list)

  ;; Load .elispcomp.el if it exists
  (elispcomp-load-el ".elispcomp.el")

  ;; BEFORE ADDING LOAD PATHS: BYTE COMPILE AND NATIVE COMPILE STANDARD PATHS
  (when compile-initial-load-paths-p
    (dolist (path load-path)
      (when (file-directory-p path)
        (when byte-comp-enabled
          (message "[TASK] Byte compile initial load-path: %s" path)
          (byte-recompile-directory path 0))

        (when native-comp-enabled
          (message "[TASK] Native compile initial load-path: %s" path)
          (native-compile-async path t)))))

  ;; LOAD DIRECTORIES
  (elispcomp-add-load-paths load-path-list)

  (message "")

  ;; BYTE-COMP
  (when byte-comp-enabled
    (message "[TASK] Byte compile")
    (byte-recompile-directory default-directory 0))

  ;; NATIVE-COMP
  (when (and native-comp-enabled native-comp-available)
    (message "[TASK] Native compile")
    (native-compile-async default-directory t)
    (while (> (length comp-files-queue) 0)
      (sleep-for 0.2))))

(provide 'elispcomp)
;;; elispcomp.el ends here
