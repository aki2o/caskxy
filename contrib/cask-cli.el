(require 'url-http)

(defvar url-http-end-of-headers nil)

(defadvice url-retrieve-synchronously (around fix-for-ntemacs activate)
  (if (not (executable-find "curl"))
      ad-do-it
    (let* ((url (ad-get-arg 0))
           (buff (generate-new-buffer (format " *%s*" url)))
           (coding-system-for-read 'binary)
           (coding-system-for-write 'binary))
      (message "Contacting by cURL : %s" url)
      (save-window-excursion
        (call-process "curl" nil buff t "-s" "-i" url))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (set-buffer-file-coding-system 'raw-text)
          (dolist (var '(url-http-end-of-headers
                         url-http-response-version
                         url-http-response-status))
            (set (make-local-variable var) nil))
          (goto-char (point-min))
          (when (re-search-forward "^\r*$" nil t)
            (setq url-http-end-of-headers (set-marker (make-marker) (point)))
            (url-http-clean-headers))))
      (setq ad-return-value buff))))

(require 'cask-cli (concat (directory-file-name (getenv "HOME")) "/.cask/cask-cli.el"))

