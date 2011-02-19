(jkl/add-exec-paths "C:/prp/Python27"
		    "C:/MingW/msys/1.0/bin"
		    "C:/prp/Mercurial"
		    "C:/prp/Aspell/bin")

(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat jkl/pkg-path "yasnippet-0.6.1c/snippets"))
