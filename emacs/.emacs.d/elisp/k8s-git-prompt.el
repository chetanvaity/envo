;; A prompt for EShell with:
;;  - Kubernetes context
;;  - Shortened pwd
;;  - Git branch

;; TBD:
;;  - Eliminate the "(.git)" which appears if pwd is not in a Git repo 
;;  - Git status color
;;  - Better pwd shortening


(defun get-theme (theme-name)
  "Some color themes to work with my favourite emacs color themes"
  (cond ((string-equal theme-name "dark-laptop") '((bg . "black") (wheel . "blue")
                                                   (punctuation . "white") (cluster . "red")
                                                   (namespace . "turquoise") (path . "blue")
                                                   (git . "green")))
        ((string-equal theme-name "robin-hood") '((bg . "#304020") (wheel . "DodgerBlue1")
                                                  (punctuation . "NavajoWhite") (cluster . "coral")
                                                  (namespace . "turquoise") (path . "DodgerBlue1")
                                                  (git . "SpringGreen1")))
        ))

(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun git-prompt ()
  "Return string to be displayed about git status"
  (car (last (split-string (shell-command-to-string "git  status | head -1")))))

(defun path-prompt ()
  "Return string to be displayed about the path"
  (let ((dirs (split-string (eshell/pwd) "\/" t)))
       (if (> (length dirs) 3)
           (concat ".../" (mapconcat 'identity (nthcdr (- (length dirs) 2) dirs) "/"))
         (eshell/pwd))))

(defun k8s-prompt ()
  "Return a fancy string with context/cluster name and namespace"
  (concat
   (with-face "(" :background prompt-bg :foreground (cdr (assoc 'punctuation theme)))
   (with-face "\u2388" :background prompt-bg :foreground (cdr (assoc 'wheel theme)))
   (with-face "|" :background prompt-bg :foreground (cdr (assoc 'punctuation theme)))
   (with-face (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                                  "kubectl config current-context | cut -d. -f1-2 2>/dev/null"))
              :background prompt-bg :foreground (cdr (assoc 'cluster theme)))
   (with-face ":" :background prompt-bg :foreground (cdr (assoc 'punctuation theme)))
   (with-face (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                                  "kubectl config view --minify --output 'jsonpath={..namespace}' 2>/dev/null"))
              :background prompt-bg :foreground (cdr (assoc 'namespace theme)))
   (with-face ")" :background prompt-bg (cdr (assoc 'punctuation theme)))))
  
(defun k8s-git-eshell-prompt ()
  (let ((prompt-bg "#304020") (theme (get-theme "robin-hood")))
    (concat
     (k8s-prompt)
     (with-face (concat (path-prompt) " ") :background prompt-bg :foreground (cdr (assoc 'path theme)))
     (with-face
      (or (ignore-errors (format "(%s)" (git-prompt))) "")
      :background prompt-bg :foreground (cdr (assoc 'git theme)))
     (if (= (user-uid) 0)
         (with-face " #" :foreground "red")
       " $")
     " ")))
(setq eshell-prompt-function 'k8s-git-eshell-prompt)
(setq eshell-highlight-prompt nil)
