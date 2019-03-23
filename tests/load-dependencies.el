"
Loads the required external dependencies needed to run the tests
"

;; TODO: Consider using Cask instead
;;      https://cask.readthedocs.io/en/latest/guide/introduction.html

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 '(s websocket))
