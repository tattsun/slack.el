;; -*- lexical-binding: t -*-
(require 'websocket)
(require 'json)
(require 'web)
(require 'cl-lib)

(defvar slack/token nil)
(defvar slack/api-base "https://slack.com/api")
(defun slack/get-api-url (method)
  (concat slack/api-base
          (concat "/" method)))
(defvar slack/rtm-start-url (slack/get-api-url "rtm.start"))
(defvar slack/websocket nil)

(defvar slack/channellist (make-hash-table))
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
        channellist)))))
(defun slack/add-to-channellist (channelid channelname)
  (puthash (intern channelid) (list 'name channelname) slack/channellist))
(defun slack/get-from-channellist (channelid)
  (gethash (intern channelid) slack/channellist))
(defun slack/get-channelid-by-name (channelname)
  (let ((found '()))
    (maphash (lambda (k v)
               (if (string= (plist-get v 'name) channelname)
                   (push k found)))
             slack/channellist)
    (car found)))


(defvar slack/userlist (make-hash-table))
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
  (with-current-buffer (slack/log-buffer)
    (goto-char (point-max))
    (insert "[debug]slack.el: " log "\n")))
(defun slack/log (log)
  (message "[info]slack.el: %s" log))
(defun slack/log-error (log)
  (message "[error]slack.el: %s" log))

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
    (cond ((gethash "ok" jsonobj)
           (gethash "url" jsonobj))
          (t (slack/log-error (concat "request websocket failed: jsonstr"))))))

(defun slack/buffer ()
  (get-buffer-create "*slack*"))
(defun slack/log-buffer ()
  (get-buffer-create "*slack-log*"))
(defun slack/add-message (channelname username message)
  (with-current-buffer (slack/buffer)
    (goto-char (point-max))
    (insert (concat username "@" channelname ": " message "\n"))))

(defun slack/handle-payload (payload)
  (slack/log-debug payload)
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

(defun slack/send-message (channel message)
  (let ((jsonobj (list :id 1
                       :type "message"
                       :channel channel
                       :text message)))
    (websocket-send-text slack/websocket (json-encode jsonobj))))

(defun slack/start ()
  (slack/stop)
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
  (cond ((null slack/websocket) nil)
        (t (websocket-close slack/websocket)))
  (setq slack/websocket nil))
