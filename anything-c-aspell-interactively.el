(defvar anything-aspell-interactively-program "aspell"
  "An aspell program file name for the `start-process'")
(defvar anything-aspell-interactively-lang nil
  "A language code to be passed as the aspell program's \"-l\" option if any.")
(defvar anything-c-source-aspell-interactively
  '((name . "Aspell interactively")
    (match identity)
    (candidates
     . (lambda ()
         (let* ((args `("-a" ,@(anything-aif anything-aspell-interactively-lang
                                             `("-l" ,it))))
                (proc (apply 'start-process "aspell-process" nil
                             anything-aspell-interactively-program args)))
           (prog1 proc
             (process-send-string proc (concat anything-pattern "\n"))))))
    (candidate-transformer
     . (lambda (cands)
         (let ((collect2 (lambda (line pattern)
                           (case (aref line 0)
                             (?* (list (cons (concat "* " pattern) pattern)))
                             (?& (let ((orig&miss (split-string line ": ")))
                                   (append
                                    (list (cons (car orig&miss) pattern))
                                    (split-string (cadr orig&miss) ", "))))
                             (t nil))))
               (skipp (lambda (x)
                        (or (equal "" x) (string-match "^@(#)" x)))))
           (apply 'append
                  (mapcar* collect2
                           (remove-if skipp cands)
                           (split-string anything-pattern "\\W+"))))))
    (action . (("Insert" . insert)
               ("Copy result to kill-ring" . kill-new)))
    (persistent-action . kill-new)
    (requires-pattern . 3)))
;; (anything 'anything-c-source-aspell-interactively)
(defun anything-c-source-aspell-interactively ()
  (interactive)
  (anything 'anything-c-source-aspell-interactively
            (when current-prefix-arg (word-at-point))
            nil nil nil
            "*anything aspell interactively*"))

(provide 'anything-c-aspell-interactively)
;;; anything-c-aspell-interactively.el ends here
