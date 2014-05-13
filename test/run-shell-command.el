(require 'caskxy)
(require 'el-expectations)

(expectations
  (desc "run-shell-command not result-as-string")
  (expect (mock (shell-command "ls -l" "*Caskxy Result*"))
    (stub shell-command-to-string => nil)
    (caskxy--run-shell-command "ls -l"))
  (desc "run-shell-command result-as-string")
  (expect (mock (shell-command-to-string "ls -l 2>/dev/null"))
    (stub shell-command => nil)
    (caskxy--run-shell-command "ls -l" t))
  )

