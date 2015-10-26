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

(setq slack/channellist (make-hash-table))
(defun slack/init-channellist ()
  (slack/rpc-call
   "channels.list"
   (concat "?token=" slack/token)
   (lambda (data)
     (let* ((json-object-type 'hash-table)
            (json (json-read-from-string data))
            (channellist (gethash "channels" json)))
       (mapcar
        '(lambda (channel)
           (let ((id (gethash "id" channel))
                 (name (gethash "name" channel)))
             (slack/add-to-channellist id name)))
        channellist)
       ))))
(defun slack/add-to-channellist (channelid channelname)
  (puthash (intern channelid) (list 'name channelname) slack/channellist))
(defun slack/get-from-channellist (channelid)
  (gethash (intern channelid) slack/channellist))

(setq slack/userlist (make-hash-table))
(defun slack/init-userlist ()
  (slack/rpc-call
   "users.list"
   (concat "?token=" slack/token)
   (lambda (data)
     (let* ((json-object-type 'hash-table)
            (json (json-read-from-string data))
            (memberlist (gethash "members" json)))
       (mapcar
        '(lambda (member)
           (let ((id (gethash "id" member))
                 (name (gethash "name" member)))
             (slack/add-to-userlist id name)))
        memberlist)
       ))))
(defun slack/add-to-userlist (userid username)
  (puthash (intern userid) (list 'name username) slack/userlist))
(defun slack/get-from-userlist (userid)
  (gethash (intern userid) slack/userlist))

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

(defun slack/buffer ()
  (get-buffer-create "*slack*"))
(defun slack/add-message (channelname username message)
  (with-current-buffer (slack/buffer)
    (insert (concat username "@" channelname ": " message "\n"))))

(defun slack/handle-payload (payload)
  (message payload)
  (let* ((json-object-type 'hash-table)
        (json (json-read-from-string payload))
        (type (gethash "type" json)))
    (cond ((equal type "message")
           (let* ((userid (gethash "user" json))
                 (channelid (gethash "channel" json))
                 (username (plist-get (slack/get-from-userlist userid) 'name))
                 (channelname (plist-get (slack/get-from-channellist channelid) 'name)))
             (slack/add-message channelname username (gethash "text" json))))
          (t nil))))

(defun slack/start ()
  (slack/init-userlist)
  (slack/init-channellist)
  (slack/rpc-call
   "rtm.start"
   (concat "?token=" slack/token)
   (lambda (data)
     (let ((wsurl (slack/parse-rtm-start-response data)))
       (setq slack/websocket
             (websocket-open
              wsurl
              :on-message (lambda (websocket frame)
                            (slack/handle-payload
                             (decode-coding-string (websocket-frame-payload frame) 'utf-8)))
              :on-close (lambda (websocket) (slack/log "closed"))))))))

(defun slack/stop ()
  (websocket-close slack/websocket))
