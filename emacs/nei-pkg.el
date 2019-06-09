;; Elisp packaging is still very much work in progress.
;;
;; Currently the goal is to make NEI available via quelpa
;; (https://framagit.org/steckerhalter/quelpa):
;;
;; If you have MELPA configured you can get it with:
;;
;; M-x package-install RET quelpa RET
;;
;; Then evaluate (with M-:) the following expression:
;;
;; (quelpa '(nei :fetcher github :repo "pyviz/nei" :branch "master" :files ("emacs/*.el")))
;;
;; 

(define-package "nei"
  "0.0.7"
  "Notebook Editor Interface"
  '((emacs "25")
    (websocket "1.7")
    (htmlize "1.54")
    (s "1.11.0")
    ))