;; BBN bitgraph terminal.

(load (concat term-file-prefix "vt100") nil t) ;BG keyboard is VT100 clone
(autoload 'bg-mouse-report "bg-mouse")
(global-set-key "\e:" 'bg-mouse-report)
(send-string-to-terminal "\e:0;7;;;360;512;9;16;9;16c")
