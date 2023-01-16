
;;; Eli’s

(defun scroll-up-benchmark ()
  (interactive)
  (let ((oldgc gcs-done)
        (oldtime (float-time))
        (count 0))
    (condition-case nil (while t (scroll-up) (redisplay) (cl-incf count))
      (error (message "GCs: %d Elapsed time: %f seconds Second/scroll: %f"
                      (- gcs-done oldgc) (- (float-time) oldtime)
                      (/ (- (float-time) oldtime) count))))))

(defun scroll-up-benchmark ()
  (interactive)
  (let ((oldgc gcs-done)
        (oldtime (float-time)))
    (condition-case nil (while t (scroll-up) (redisplay))
      (error (message "GCs: %d Elapsed time: %f seconds"
                      (- gcs-done oldgc) (- (float-time) oldtime))))))

(defun scroll-up-by-40-benchmark ()
  (interactive)
  (let ((oldgc gcs-done)
        (oldtime (float-time)))
    (condition-case nil (while t (scroll-up 40) (redisplay))
      (error (message "GCs: %d Elapsed time: %f seconds"
                      (- gcs-done oldgc) (- (float-time) oldtime))))))

(defun scroll-up-insert-quote-benchmark ()
  (interactive)
  (let ((oldgc gcs-done)
        (oldtime (float-time)))
    (condition-case nil
        (while t
          (scroll-up 50)
          (when (zerop (mod (1- (line-number-at-pos)) 100))
            (insert "\""))
          (redisplay))
      (error (message "GCs: %d Elapsed time: %f seconds"
                      (- gcs-done oldgc) (- (float-time) oldtime))))))

(defun xdisp-eob-benchmark ()
  (benchmark-run 100
    (progn
      (with-current-buffer (find-file "~/src/emacs/src/xdisp.c")
	    (end-of-buffer))
      (kill-buffer "xdisp.c"))))

;;; Alan’s

(defmacro time-it (&rest forms)
  "Time the running of a sequence of forms using `float-time'.
Call like this: \"M-: (time-it (foo ...) (bar ...) ...)\"."
  `(let ((start (float-time)))
     ,@forms
     (- (float-time) start)))

(defun time-scroll (&optional arg)
  (interactive "P")
  (message "%s"
           (time-it
            (condition-case nil
                (while t
                  (if arg (scroll-down) (scroll-up))
                  (sit-for 0))
              (error nil)))))
