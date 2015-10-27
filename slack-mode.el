;; -*- lexical-binding: t -*-

(defun slack-start ()
  (interactive)
  (switch-to-buffer (slack/buffer))
  (slack-mode))

(define-derived-mode slack-mode
  text-mode
  "Slack"
  "Major mode for Slack"
  (slack/start))

(setq slack/now-channel "general")

(define-key slack-mode-map "i" 'slack/input-message)
(define-key slack-mode-map "c" 'slack/change-channel)

(defun slack/input-message ()
  (interactive)
  (let* ((message (read-from-minibuffer (concat slack/now-channel ": "))))
    (slack/send-message (slack/get-channelid-by-name slack/now-channel) message)))

(defun slack/change-channel ()
  (interactive)
  (let ((channel (read-from-minibuffer (concat
                                        "Change channel: "
                                        slack/now-channel
                                        " -> "))))
    (setq slack/now-channel channel)))

(defun slack/move-to-prompt ()
  (if (eq major-mode 'slack-mode)
      (goto-char (point-max))))

(defun slack/enable-move-to-prompt ()
  (add-hook 'pre-command-hook 'slack/move-to-prompt))
