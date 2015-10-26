;; -*- lexical-binding: t -*-
(require 'websocket)
(require 'json)
(require 'web)

(setq slack/token nil)
(setq slack/api-base "https://slack.com/api")
(defun slack/get-api-url (method)
  (concat slack/api-base
          (concat "/" method)))
(setq slack/rtm-start-url (slack/get-api-url "rtm.start"))

(defun slack/log-debug (log)
  (message (concat "[debug]slack.el: " log)))
(defun slack/log (log)
  (message (concat "[info]slack.el: " log)))
(defun slack/log-error (log)
  (message (concat "[error]slack.el: " log)))

(defun slack/rpc-call (method query callback)
  (let ((url (concat (slack/get-api-url method)
                     query))
        (cb (lambda (httpc header data)
              (funcall callback data))))
    (web-http-get
     cb
     :url url)))

(defun slack/parse-rtm-start-response (jsonstr)
  (let* ((json-object-type 'hash-table)
         (jsonobj (json-read-from-string jsonstr)))
    (gethash "url" jsonobj)))


(defun slack/start ()
  (slack/rpc-call
   "rtm.start"
   (concat "?token=" slack/token)
   (lambda (data)
     (let ((wsurl (slack/parse-rtm-start-response data)))
       (setq slack/websocket
             (websocket-open
              wsurl
              :on-message (lambda (websocket frame)
                            (slack/log-debug (websocket-frame-payload frame)))
              :on-close (lambda (websocket) (slack/log "closed"))))))))

(defun slack/stop ()
  (websocket-close slack/websocket))
