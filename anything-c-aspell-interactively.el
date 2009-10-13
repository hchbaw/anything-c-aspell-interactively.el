(defvar anything-aspell-interactively-program "aspell"
  "An aspell program file name for the `start-process'")
(defvar anything-aspell-interactively-lang nil
  "A language code to be passed as the aspell program's \"-l\" option if any.")
(defun* anything-aspell-interactively-any-buffer->sources
    (&optional (any-name (anything-attr 'name))
               (any-buffer anything-buffer)
               (action (anything-attr 'action))
               (persistent-action (anything-attr 'persistent-action)))
  "Convert un-classificed aspell's ANY-NAME candidates on ANY-BUFFER into classified sources for `anything'.

  ACTION and PERSISTENT-ACTION are for the classified sources' respectively."
  (labels
      ((source (name cands)
         `((name . ,(format "Aspell Classified (%s)" name))
           (candidates ,@(nreverse cands))
           (action . ,action)
           (persistent-action . ,persistent-action)))
       (rec (xs name cands acc)
         (cond ((and (null xs) name)
                (nreverse (cons (source name cands) acc)))
               ((null xs) nil)
               ((and (consp (car xs)) name)
                (rec (cdr xs) (caar xs) (list (cdar xs))
                     (cons (source name cands) acc)))
               ((consp (car xs))
                (rec (cdr xs) (caar xs) (list (cdar xs)) acc))
               (t
                (rec (cdr xs) name (cons (car xs) cands) acc)))))
    (rec (with-current-buffer any-buffer
           (save-excursion
             (let* ((beg (loop with pos
                               initially (goto-char (point-min))
                               while (setq pos
                                           (next-single-property-change
                                            (point) 'anything-header))
                               do (goto-char pos)
                               if (equal any-name
                                         (buffer-substring-no-properties
                                          (point-at-bol) (point-at-eol)))
                               return pos))
                    (end (or (progn
                               (forward-line 1)
                               (next-single-property-change (point)
                                                            'anything-header))
                             (point-max))))
               (when (and beg end)
                 (save-restriction
                   (narrow-to-region beg end) ;; To one and only one source.
                   (loop until (eobp)
                         initially (goto-char (point-min)) (forward-line 1)
                         for line = (buffer-substring-no-properties
                                     (point-at-bol) (point-at-eol))
                         unless (equal "" line)
                         collect (case (aref line 0)
                                   ((?* ?&)
                                    (cons line
                                          (progn
                                            (goto-char (+ 2 (point)))
                                            (word-at-point))))
                                   (t line))
                         do (forward-line 1)))))))
         nil nil nil)))
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
    (action-transformer
     . (lambda (actions _selection)
         (let ((ss (anything-aspell-interactively-any-buffer->sources)))
           (append actions
                   (when ss
                     (list
                      (cons
                       "Classify with anything"
                       (lexical-let ((ss ss))
                         (lambda (_)
                           (anything-aif (get-buffer anything-action-buffer)
                               (kill-buffer it))
                           (anything-other-buffer
                            ss
                            "*anything aspell classification*"))))))))))
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

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "anything-aspell-interactively-any-buffer->sources")
      (expect nil
        (with-temp-buffer
          (anything-aspell-interactively-any-buffer->sources
           "aspell" (current-buffer) "action" "persistent-action")))
      (expect nil
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   (list
                    (propertize "not aspell" 'anything-header t)
                    "foo"
                    "bar")
                   "\n"))
          (anything-aspell-interactively-any-buffer->sources
           "aspell" (current-buffer) "action" "persistent-action")))
      (expect nil
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   (list
                    (propertize "aspell" 'anything-header t)
                    "foo"
                    "bar")
                   "\n"))
          (anything-aspell-interactively-any-buffer->sources
           "aspell" (current-buffer) "action" "persistent-action")))
      (expect '(((name . "Aspell Classified (* foo)")
                 (candidates "foo" "bar")
                 (action . "action")
                 (persistent-action . "persistent-action")))
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   (list
                    (propertize "aspell" 'anything-header t)
                    "* foo"
                    "bar")
                   "\n"))
          (anything-aspell-interactively-any-buffer->sources
           "aspell" (current-buffer) "action" "persistent-action")))
      (expect '(((name . "Aspell Classified (& foo)")
                 (candidates "foo" "foo")
                 (action . "action")
                 (persistent-action . "persistent-action"))
                ((name . "Aspell Classified (* bar)")
                 (candidates "bar" "blah")
                 (action . "action")
                 (persistent-action . "persistent-action")))
        (with-temp-buffer
          (insert (mapconcat
                   'identity
                   (list
                    "not aspell"
                    "dummy"
                    ""
                    (propertize "aspell" 'anything-header t)
                    "& foo"
                    "foo"
                    "* bar"
                    "blah")
                   "\n"))
          (anything-aspell-interactively-any-buffer->sources
           "aspell" (current-buffer) "action" "persistent-action")))
      )))

(provide 'anything-c-aspell-interactively)
;;; anything-c-aspell-interactively.el ends here
