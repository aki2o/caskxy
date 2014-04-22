(require 'caskxy)
(require 'ert-expectations)

(expectations
  (desc "with-clean-env exist env")
  (expect t
    (loop for v in (caskxy--get-relate-env-vars)
          if (or (not (getenv v))
                 (string= (getenv v) ""))
          do (setenv v "hoge"))
    (loop for v in (caskxy--get-relate-env-vars)
          always (getenv v)))
  (desc "with-clean-env clean env")
  (expect t
    (caskxy--with-clean-env
      (and (not (getenv "EMACSLOADPATH"))
           (not (getenv "INSIDE_EMACS")))))
  (desc "with-clean-env restore env")
  (expect t
    (caskxy--with-clean-env
      (loop for v in (caskxy--get-relate-env-vars)
            do (setenv v "")))
    (loop for v in (caskxy--get-relate-env-vars)
          always (not (string= (getenv v) ""))))
  )

