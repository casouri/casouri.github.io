;;; atomic-buffer.el ---       -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; Variable and helpers

(defvar-local masterp nil
  "Non-nil if this is a master buffer.")

(defvar-local minionp nil
  "Non-nil if this is a minion buffer.")

(defun get-master (minion)
  "Return the master buffer of MINION buffer."
  (get-buffer "master"))

(defun create-minion (master)
  "Create the minion buffer for MASTER buffer."
  (with-current-buffer (get-buffer-create "minion")
    (insert "Minion\n\n(I’m following Master!)")
    (setq minionp t)))

(defun get-minion (master)
  "Return the minion buffer of MASTER buffer."
  (get-buffer "minion"))

;;; Show/hide minion window

(defun show-minion (minion master-window)
  "Show MINION next to MASTER-WINDOW."
  (set-window-dedicated-p
   (display-buffer-in-atom-window
    minion `((side . below) (window . ,master-window)
             (window-height . 0.3)))
   t))

(defun delete-minion-window (minion-window)
  "Delete MINION-WINDOW."
  (set-window-parameter minion-window 'window-atom nil)
  (delete-window minion-window))

;;; Sync windows

(defun share-parent-any (win win-list)
  "Return non-nil if WIN and any window in WIN-LIST shares parent."
  (cl-labels ((share-parent (a b) (eq (window-parent a)
                                      (window-parent b))))
    (cl-loop for w in win-list
             if (share-parent win w)
             return t
             finally return nil)))

(defun sync-window (_)
  "Make sure each minion is next to each master."
  (cl-labels ((in-the-right-place
               ;; Is this minion-window out-of-place?
               (minion-window master-windows)
               (share-parent-any minion-window
                                 master-windows))
              (has-minion-next-to-it
               ;; Does this master-window has a minion-window next to it?
               (master-window minion-windows)
               (share-parent-any master-window
                                 minion-windows)))
    (dolist (buf (buffer-list))
      (cond ((buffer-local-value 'minionp buf)
             ;; Delete minion windows that are out-of-place.
             (let* ((minion buf)
                    (minion-windows (get-buffer-window-list minion))
                    (master (get-master minion))
                    (master-windows (get-buffer-window-list master)))
               (dolist (minion-window minion-windows)
                 (if (not (in-the-right-place
                           minion-window master-windows))
                     (delete-minion-window minion-window)))))
            ((buffer-local-value 'masterp buf)
             ;; Make sure each master has a minion window next to it.
             (let* ((master buf)
                    (master-windows (get-buffer-window-list master))
                    (minion (get-minion master))
                    (minion-windows (get-buffer-window-list minion)))
               (when minion
                 (dolist (master-window master-windows)
                   (if (not (has-minion-next-to-it
                             master-window minion-windows))
                       (show-minion minion master-window))))))))))

(define-minor-mode auto-sync-mode
  "Auto sync minion and master."
  :global t
  :lighter ""
  (if auto-sync-mode
      (add-hook 'window-buffer-change-functions #'sync-window)
    (remove-hook 'window-buffer-change-functions #'sync-window)))

;;; Show minion minor mode

(define-minor-mode show-minion-mode
  "Show minion."
  :lighter ""
  (setq masterp t)
  (if show-minion-mode
      (progn (create-minion (current-buffer))
             (sync-window nil))
    (let ((minion (get-minion (current-buffer))))
      (dolist (window (get-buffer-window-list minion))
        (delete-minion-window window))
      (kill-buffer minion))))

(provide 'atomic-buffer)

;;; atomic-buffer.el ends here
