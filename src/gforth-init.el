(autoload 'forth-mode "gforth" "Forth mode" t)
(autoload 'run-forth "gforth" "Run Forth" t)
(add-to-list 'auto-mode-alist '("\.fs$" . forth-mode))
