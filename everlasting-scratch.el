;;; everlasting-scratch.el --- The *scratch* that lasts forever -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/everlasting-scratch
;; Version: 0.1.1
;; Created: 2022-04-01
;; Keywords: convenience, tool
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a global minor mode `everlasting-scratch-mode'
;; that causes the scratch buffer to respawn after it's killed and with
;; its content restored.
;;
;; @note: borrowed lots of stuff from immortal-scratch, thanks mate.

;;; Usage:
;;
;;  (add-hook 'after-init-hook 'everlasting-scratch-mode)

;;; Change Log:
;;
;; 0.1.1 Add sync timer to save *scratch* every 30 secs.

;;; Code:

(require 'desktop)

(defgroup everlasting-scratch nil
  "Respawn scratch buffer when it's killed."
  :group 'convenience)


(defvar everlasting-scratch--sync-timer nil
  "Everlasting-Scratch sync timer.")


(defun everlasting-scratch--run-timers ()
  "Run sync timer to save buffer message every 30 secs."
  (setq everlasting-scratch--sync-timer
        (run-with-timer (* 10 3)
                        (* 10 3)
                        'everlasting-scratch-save)))


(defun everlasting-scratch--cancel-timers ()
  "Cancel sync timers."
  (when everlasting-scratch--sync-timer
    (cancel-timer everlasting-scratch--sync-timer)
    (setq everlasting-scratch--sync-timer nil)))


(defun everlasting-scratch-respawn ()
  "Create *scratch* buffer if it doesn't exist."

  (with-current-buffer (get-buffer-create "*scratch*")
    (with-temp-message ""
      (when (zerop (buffer-size))
        (insert (decode-coding-string initial-scratch-message 'utf-8))
        (set-buffer-modified-p nil)
        (funcall initial-major-mode)
        (everlasting-scratch-mode)))))


(defun everlasting-scratch-kill ()
  "Add to `kill-buffer-query-functions' to respawn scratch when it is killed."

  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (let ((kill-buffer-query-functions kill-buffer-query-functions))
        (remove-hook 'kill-buffer-query-functions #'everlasting-scratch-kill)
        (kill-buffer "*scratch*")
        (everlasting-scratch-respawn)
        nil)
    t ;; not scratch
    ))


(defun everlasting-scratch-save (&rest _)
  "Save *scratch* buffer content before kill."

  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (unless (zerop (buffer-size))
        (setq initial-scratch-message
              (encode-coding-string (buffer-substring-no-properties (point-min) (point-max)) 'utf-8))))))


;;;###autoload
(defun everlasting-scratch-restore ()
  "Restore *scratch* buffer content.

Manually restore scratch content,
e.g: invoking after `desktop-change-dir'."
  (interactive)
  (with-temp-message "*scratch* restored"
    (when (get-buffer "*scratch*")
      (with-current-buffer "*scratch*"
        (read-only-mode -1)
        (erase-buffer)
        (fundamental-mode)
        (insert (decode-coding-string initial-scratch-message 'utf-8))
        (set-buffer-modified-p nil)
        (funcall initial-major-mode)
        (everlasting-scratch-mode))
      (switch-to-buffer "*scratch*"))
    (sit-for 0.5)))


;;;###autoload
(define-minor-mode everlasting-scratch-mode
  "When the scratch buffer is killed, immediately respawn it with content restored."

  :group 'everlasting-scratch
  :global t
  :lighter ""

  (if everlasting-scratch-mode
      (progn
        (add-hook 'kill-buffer-query-functions #'everlasting-scratch-kill)
        ;; survive manual kill
        (advice-add #'everlasting-scratch-kill :before #'everlasting-scratch-save)
        ;; survive emacs exit
        (advice-add #'save-buffers-kill-emacs :before #'everlasting-scratch-save)
        ;; save *scratch* to desktop file
        (setq desktop-globals-to-save
              (add-to-list 'desktop-globals-to-save 'initial-scratch-message))
        ;; save *scratch* every 30 secs
        (everlasting-scratch--cancel-timers)
        (everlasting-scratch--run-timers))
    ;; restoration
    (everlasting-scratch--cancel-timers)
    (setq desktop-globals-to-save
          (delete 'initial-scratch-message desktop-globals-to-save))
    (advice-remove #'save-buffers-kill-emacs #'everlasting-scratch-save)
    (advice-remove #'everlasting-scratch-kill #'everlasting-scratch-save)
    (remove-hook 'kill-buffer-query-functions #'everlasting-scratch-kill)))


(provide 'everlasting-scratch)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; everlasting-scratch.el ends here
