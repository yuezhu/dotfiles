;; -*- lexical-binding: t; -*-

;;
;; Utility functions
;;
(defun save-buffer-file-name ()
  "Show the full path file name in the minibuffer, and push it
into the kill ring."
  (interactive)
  (if (not buffer-file-name)
      (message "This buffer is not visiting a file on the disk")
    (kill-new buffer-file-name)
    (message "Copied '%s'" buffer-file-name)))

(defun adjust-window-dimension-transient ()
  "Adjust window dimension with transient."
  (interactive)
  (let ((echo-keystrokes nil))
    (message "Adjust window dimension: {: shrink horizontally, }: enlarge horizontally, [: shrink vertically, ]: enlarge vertically, 0: balance windows")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "{") #'shrink-window-horizontally)
       (define-key map (kbd "}") #'enlarge-window-horizontally)
       (define-key map (kbd "[") #'shrink-window)
       (define-key map (kbd "]") #'enlarge-window)
       (define-key map (kbd "0") #'balance-windows)
       map)
     t)))

(defun insert-timestamp ()
  "Insert the timestamp with selected format."
  (interactive)
  (insert (format-time-string
           (completing-read "Timestamp format: "
                            '("%m/%d/%y"
                              "%H:%M:%S"
                              "%m/%d/%y %H:%M:%S"
                              "%a %b %d %H:%M:%S %Z %Y"))
           (current-time))))

(provide 'init-util)
