(defun git-prompt ()
  "Return string to be displayed about git status"
  (car (last (split-string (shell-command-to-string "git  status | head -1"))))
  )

(defun path-prompt ()
  "Return string to be displayed about the path"
  (let ((dirs (split-string (eshell/pwd) "\/" t)))
       (if (> (length dirs) 3)
           (concat ".../" (mapconcat 'identity (nthcdr (- (length dirs) 2) dirs) "/"))
         (eshell/pwd)
       )
  )
)

(defun k8s-prompt ()
  "Return a fancy string with context/cluster name and namespace"
  (concat
   (with-face "(" :background prompt-bg :foreground "white")
   (with-face "\u2388" :background prompt-bg :foreground "blue")
   (with-face "|" :background prompt-bg :foreground "white")
   (with-face (replace-regexp-in-string "\n$" "" (shell-command-to-string "kubectl config current-context | cut -d. -f1-2 2>/dev/null"))
              :background prompt-bg :foreground "dark red")
   (with-face ":" :background prompt-bg :foreground "white")
   (with-face (replace-regexp-in-string "\n$" "" (shell-command-to-string "kubectl config view --minify --output 'jsonpath={..namespace}' 2>/dev/null"))
              :background prompt-bg :foreground "turquoise")
   (with-face ")" :background prompt-bg :foreground "white")
   )
  )

(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist)
  )
  
(defun shk-eshell-prompt ()
  (let ((prompt-bg "#000"))
    (concat
     (k8s-prompt)
     (with-face (concat (path-prompt) " ") :background prompt-bg :foreground "blue")
     (with-face
      (or (ignore-errors (format "(%s)" (git-prompt))) "")
      :background prompt-bg :foreground "green")
     (if (= (user-uid) 0)
         (with-face " #" :foreground "red")
       " $")
     " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)
