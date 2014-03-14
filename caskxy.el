;;; caskxy.el --- Control Cask on Emacs

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aki2o/caskxy
;; Version: 0.0.1
;; Package-Requires: ((log4e "0.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides the interface to control Cask on Emacs.

;;; Dependency:
;; 
;; - log4e.el ( see <https://github.com/aki2o/log4e> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'caskxy)

;;; Configuration:
;; 
;; ;; Make config suit for you. About the config item, see Customization or eval the following sexp.
;; ;; (customize-group "caskxy")

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "caskxy/" :docstring t)
;; `caskxy/tester-backend'
;; Feature of running test.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "caskxy/" :docstring t)
;; `caskxy/set-emacs'
;; Set the condition of emacs runtime.
;; `caskxy/set-location'
;; Set the condition of project path.
;; `caskxy/show-condition'
;; Show current condition.
;; `caskxy/do-cask-command'
;; Execute the command of Cask.
;; `caskxy/set-tester-backend'
;; Set BACKEND to `caskxy/tester-backend'.
;; `caskxy/run-test'
;; Run test of TEST-FILE.
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "caskxy/" :docstring t)
;; `caskxy/add-tester-backend'
;; Add a test function.
;; `caskxy/build-ert'
;; Not documented.
;; `caskxy/build-el-expectations'
;; Not documented.
;; `caskxy/filter-el-expectations'
;; Not documented.
;; `caskxy/build-ert-expectations'
;; Not documented.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.2.1 (i386-mingw-nt5.1.2600) of 2012-12-08 on GNUPACK
;; - log4e.el ... Version 0.2.0


;; Enjoy!!!


;; Avoid warning
(eval-when-compile
  (defun erte-find-test-other-window (n) nil))

(eval-when-compile (require 'cl))
(require 'log4e)
(require 'package nil t)
(require 'ert nil t)
(require 'el-expectations nil t)
(require 'ert-expectations nil t)

(defgroup caskxy nil
  "Control Cask in Emacs."
  :group 'convenience
  :prefix "caskxy/")

(defcustom caskxy/tester-backend 'ert
  "Feature of running test."
  :type 'symbol
  :group 'caskxy)


(log4e:deflogger "caskxy" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                    (error . "error")
                                                    (warn  . "warn")
                                                    (info  . "info")
                                                    (debug . "debug")
                                                    (trace . "trace")))
(caskxy--log-set-level 'trace)


(defvar caskxy--cask-features '(cask-cli cask cask-bootstrap))
(defvar caskxy--cask-depend-features '(s dash f commander git epl))
(defvar caskxy--cask-commands '("package" "install" "update" "upgrade" "exec" "init" "version" "list"
                                "info" "help" "load-path" "path" "package-directory" "outdated"))
(defvar caskxy--cask-directory (expand-file-name (concat (getenv "HOME") "/.cask/")))
(defvar caskxy--cask-elisp (concat caskxy--cask-directory "cask-cli.el"))
(defvar caskxy--cask-command (concat caskxy--cask-directory "bin/cask"))
(defvar caskxy--cask-location default-directory)
(defvar caskxy--install-command-enable nil)
(defvar caskxy--exec-result-buffer-name " *Caskxy Exec*")
(defvar caskxy--tester-backends nil)


;;;;;;;;;;;;;
;; Utility

(defun* caskxy--show-message (msg &rest args)
  (apply 'message (concat "[CASKXY] " msg) args)
  nil)

(defmacro caskxy--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defmacro caskxy--save-package-info (&rest body)
  (declare (indent 0))
  `(let ((bkup-package-archives       (when (featurep 'package) package-archives))
         (bkup-package-user-dir       (when (featurep 'package) package-user-dir))
         (bkup-package-activated-list (when (featurep 'package) package-activated-list)))
     (unwind-protect
         (progn ,@body)
       (when (featurep 'package)
         (caskxy--trace "start restore package condition")
         (setq package-archives       bkup-package-archives)
         (setq package-user-dir       bkup-package-user-dir)
         (setq package-activated-list bkup-package-activated-list)
         (package-initialize)))))

(defun caskxy--reload-cask-elisp ()
  (caskxy--trace "start reload cask elisp.")
  (dolist (f (append caskxy--cask-features
                     (reverse caskxy--cask-depend-features)))
    (when (featurep f)
      (unload-feature f)))
  (defadvice cask-cli/install (around caskxy activate)
    (when caskxy--install-command-enable
      ad-do-it))
  (defadvice cask-setup-project-variables (after caskxy activate)
    (setq package-user-dir (cask-elpa-dir)))
  (load caskxy--cask-elisp))

(defun caskxy--unload-cask-elisp ()
  (caskxy--trace "start unload cask elisp.")
  (dolist (f (append caskxy--cask-features
                     (reverse caskxy--cask-depend-features)))
    (when (featurep f)
      (unload-feature f)))
  (dolist (f caskxy--cask-depend-features)
    (require f nil t)))

(defun caskxy--detect-emacs-version ()
  (caskxy--trace "start detect emacs version.")
  (caskxy--get-pure-elisp-result "emacs-version"))

(defun caskxy--detect-load-path ()
  (caskxy--trace "start detect load path.")
  (split-string
   (caskxy--get-pure-elisp-result "(mapconcat 'identity load-path \\\"\\n\\\")")
   "\n"))

(defun caskxy--get-pure-elisp-result (elisp)
  (shell-command-to-string
   (caskxy--get-emacs-batch-command (format "--eval \"(princ %s)\"" elisp))))

(defun caskxy--get-emacs-batch-command (argstr)
  (caskxy--trace "start get emacs batch command : %s" argstr)
  (format "'%s' -Q --batch %s" (or (getenv "EMACS") "emacs") argstr))

(defun caskxy--do-exec (&optional cmdstr)
  (when (not cmdstr)
   (setq cmdstr (read-string "Command: ")))
  (caskxy--trace "start do exec : %s" cmdstr)
  (let ((bkup-load-path-env (getenv "EMACSLOADPATH"))
        (bkup-path-env (getenv "PATH")))
    (unwind-protect
        (progn
          (setenv "EMACSLOADPATH" (caskxy/do-cask-command "load-path"))
          (setenv "PATH" (caskxy/do-cask-command "path"))
          (shell-command cmdstr caskxy--exec-result-buffer-name))
      (setenv "EMACSLOADPATH" bkup-load-path-env)
      (setenv "PATH" bkup-path-env)
      t)))

(defun caskxy--seek-test-files (&optional dir)
  (caskxy--trace "start seek test files : %s" dir)
  (loop with dir = (directory-file-name (or dir caskxy--cask-location))
        for node in (directory-files dir t "\\`[^._]")
        if (and (file-regular-p node)
                (string-match "\\<test\\>" node)
                (string-match "\\.el\\'" node))
        collect (progn (caskxy--trace "found test : %s" node)
                       node)
        else if (file-directory-p node)
        append (caskxy--seek-test-files node)))

(defun caskxy--make-load-file-option (load-files)
  (or (mapconcat (lambda (f) (format "-l '%s'" f)) load-files " ")
      ""))


;;;;;;;;;;;;;;;;;;;;
;; Tester Backend

;;;###autoload
(defun* caskxy/add-tester-backend (backend &key builder filter)
  "Add a test function.

BACKEND is the feature of the function.
BUILDER is the function to make a command string for test.
  The function receives one argument that is the list of test file path.
FILTER is the function to do something for the buffer of the test result.
  The function receives one argument that is the buffer object of the test result.
  If nothing to do, this value is no need."
  (caskxy--trace "start add tester backend[%s]. builder[%s] filter[%s]" backend builder filter)
  (cond ((not (symbolp backend))
         (caskxy--show-message "Failed add tester backend. Backend is not symbol : %s" backend))
        ((not (functionp builder))
         (caskxy--show-message "Failed add tester backend. Builder is not function : %s" builder))
        ((and filter
              (not (functionp filter)))
         (caskxy--show-message "Failed add tester backend. Filter is not function : %s" filter))
        (t
         (caskxy--awhen (assq backend caskxy--tester-backends)
           (setq caskxy--tester-backends (delq it caskxy--tester-backends)))
         (push `(,backend . (:builder ,builder :filter ,filter)) caskxy--tester-backends))))

(defun caskxy/build-ert (test-files)
  (format "%s -f ert-run-tests-batch-and-exit" (caskxy--make-load-file-option test-files)))

(caskxy/add-tester-backend 'ert :builder 'caskxy/build-ert)

(defun caskxy/build-el-expectations (test-files)
  (format "%s -f batch-expectations" (caskxy--make-load-file-option test-files)))

(defun caskxy/filter-el-expectations (buff)
  (caskxy--trace "start filter el expectations.")
  (with-current-buffer buff
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[0-9].+\\([0-9]\\) failures, \\([0-9]+\\) errors" nil t)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face
                           (if (and (string= "0" (match-string 1))
                                    (string= "0" (match-string 2)))
                               exps-green-face
                             exps-red-face))))))

(caskxy/add-tester-backend 'el-expectations
                           :builder 'caskxy/build-el-expectations
                           :filter 'caskxy/filter-el-expectations)

(defun caskxy/build-ert-expectations (test-files)
  (caskxy/build-el-expectations test-files))

(caskxy/add-tester-backend 'ert-expectations :builder 'caskxy/build-ert-expectations)


;;;;;;;;;;;;;;;;;;
;; User Command

;;;###autoload
(defun caskxy/set-emacs (emacs)
  "Set the condition of emacs runtime.

EMACS is the path/command of the emacs runtime used for test."
  (interactive
   (list (read-file-name "Select Emacs Runtime (emacs): " "/" "emacs")))
  (caskxy--trace "start set emacs : %s" emacs)
  (setenv "EMACS" (if (and (stringp emacs)
                           (not (string= emacs "")))
                      (expand-file-name emacs)
                    "emacs"))
  (caskxy--show-message "Set '%s'" (getenv "EMACS")))

;;;###autoload
(defun caskxy/set-location (cask-file)
  "Set the condition of project path.

CASK-FILE is the path of 'Cask' file in the tested project."
  (interactive
   (list (read-file-name "Select 'Cask' file: " nil "Cask" t)))
  (caskxy--trace "start set location : %s" cask-file)
  (setq caskxy--cask-location (file-name-directory (expand-file-name cask-file)))
  (caskxy--show-message "Set '%s'" caskxy--cask-location))

;;;###autoload
(defun caskxy/show-condition ()
  "Show current condition."
  (interactive)
  (message (concat "[Cask Condition]\n"
                   (format "  Emacs Runtime: %s\n" (or (getenv "EMACS") "emacs"))
                   (format "  Project Path: %s\n" caskxy--cask-location)
                   (format "  Tester: %s" caskxy/tester-backend))))

;;;###autoload
(defun caskxy/do-cask-command (command)
  "Execute the command of Cask.

COMMAND is the string equals the sub command of 'cask' command on shell."
  (interactive
   (list (completing-read "Select Command: " caskxy--cask-commands nil t nil '() "install")))
  (if (not (file-exists-p caskxy--cask-elisp))
      (caskxy--show-message "Not found '%s'. Do you not yet install cask?" caskxy--cask-elisp)
    (caskxy--trace "start do cask command : %s" command)
    (caskxy--save-package-info
      (let ((emacs-version (caskxy--detect-emacs-version))
            (load-path (caskxy--detect-load-path))
            (default-directory caskxy--cask-location)
            (caskxy--install-command-enable nil))
        (setq package-activated-list nil)
        (caskxy--reload-cask-elisp)
        (setq caskxy--install-command-enable t)
        (unwind-protect
            (if (string= command "exec")
                (caskxy--do-exec)
              (caskxy--trace "call cask-cli command : %s" command)
              (funcall (intern-soft (concat "cask-cli/" command))))
          (caskxy--unload-cask-elisp))))))

;;;###autoload
(defun caskxy/set-tester-backend (backend)
  "Set BACKEND to `caskxy/tester-backend'."
  (interactive
   (list (completing-read "Select Backend (ert): " (mapcar 'car caskxy--tester-backends) nil t nil '() 'ert)))
  (caskxy--trace "start set tester backend : %s" backend)
  (when (and (stringp backend)
             (not (string= backend "")))
    (setq backend (intern backend)))
  (if (not (symbolp backend))
      (caskxy--show-message "Invalid value : %s" backend)
    (setq caskxy/tester-backend backend)
    (caskxy--show-message "Set '%s" backend)))

;;;###autoload
(defun caskxy/run-test (test-file)
  "Run test of TEST-FILE.

TEST-FILE is the path of test file.
But if TEST-FILE is 'all, do the tests of all test files in the project."
  (interactive
   (list (completing-read "Select Test (all): "
                          (append (list 'all)
                                  (caskxy--seek-test-files))
                          nil t nil '() 'all)))
  (caskxy--trace "start run test : %s" test-file)
  (when (y-or-n-p (format "Start '%s' test using '%s ?" test-file caskxy/tester-backend))
    (let* ((test-files (cond ((eq test-file 'all) (caskxy--seek-test-files))
                             (t                   (list test-file))))
           (bkendinfo (or (assoc-default caskxy/tester-backend caskxy--tester-backends)
                          (caskxy--show-message "Unknown Backend : %s" caskxy/tester-backend)))
           (builder (or (when bkendinfo (plist-get bkendinfo :builder))
                        (caskxy--show-message "Not exists builder of %s" caskxy/tester-backend)))
           (filter (when bkendinfo (plist-get bkendinfo :filter)))
           (cmdarg (when builder
                     (caskxy--trace "call builder function : %s" builder)
                     (funcall builder test-files)))
           (load-tested (format "-L '%s'" (directory-file-name caskxy--cask-location)))
           (cmd (when (and (stringp cmdarg)
                           (not (string= cmdarg "")))
                  (caskxy--get-emacs-batch-command (concat load-tested " " cmdarg)))))
      (when cmd
        (caskxy--do-exec cmd)
        (caskxy--awhen (and filter
                            (get-buffer caskxy--exec-result-buffer-name))
          (when (buffer-live-p it)
            (caskxy--trace "call filter function : %s" filter)
            (funcall filter it)))))))


(provide 'caskxy)
;;; caskxy.el ends here
