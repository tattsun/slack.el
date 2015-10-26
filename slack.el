(require 'websocket)
(require 'json)
(require 'web)

(setq slack/token nil)
(setq slack/rtm-start-url "https://slack.com/api/rtm.start")

(setq slack/rtm-start-query-callback nil)
(setq slack/rtm-start-query-old-buffer nil)
(defun slack/rtm-start-query (callback)
  (setq slack/rtm-start-query-callback callback)
  (setq slack/rtm-start-query-old-buffer (current-buffer))
  (let ((url (concat slack/rtm-start-url
                     (concat "?token=" slack/token))))
    (web-http-get
     (lambda (httpc header data)
       (funcall slack/rtm-start-query-callback (slack/parse-rtm-start-response data)))
     :url url)))
(defun slack/parse-rtm-start-response (jsonstr)
  (let* ((json-object-type 'hash-table)
         (jsonobj (json-read-from-string jsonstr)))
    (gethash "url" jsonobj)))

(defun slack/start ()
  (slack/rtm-start-query
   (lambda (wsurl)
     (websocket-open
      wsurl
      :on-message (lambda (websocket frame)
                    (insert (websocket-frame-payload frame)))))))
