(require 'url)
(require 'xml)
(require 'sha1-el)
(eval-when-compile (require 'cl))

(defgroup hatena nil
  "Hatena."
  :group 'applications)

(defcustom hatena/username-function
  #'(lambda () (read-string "[Hatena] Username: "))
  "Function to retrieve user name for Hatena."
  :type 'function
  :group 'hatena)

(defcustom hatena-blog/blog-id-function
  #'(lambda () (read-string "[Hatena] blog-id: "))
  "Function to retrieve blog id."
  :type 'function
  :group 'hatena)

(defcustom hatena-blog/api-key-function
  #'(lambda () (read-passwd "[Hatena] blog api-key: "))
  "Function to retrieve blog api-key for Hatena."
  :type 'function
  :group 'hatena)

(defvar hatena/username nil
  "Username for Hatena.")
(defvar hatena-blog/blog-id nil
  "blog-id for Hatena.")
(defvar hatena-blog/api-key nil
  "Password for Hatena.")

;; :constructor などを指定することで名前を変えることができる
;; 通常は (defstruct NAME SLOTS ...) で、
;; make-NAME の constructor
;; copy-NAME の copier
;; NAME-p の predicate などがそれぞれ作られるため
(defstruct (hatena-blog/entry (:constructor hatena-blog/make-entry)
			      (:predicate hatena-blog/entry-p))
  type id user title created updated source content)

(defsubst hatena-blog--api-decode (str)
  (when (stringp str) (decode-coding-string str 'utf-8)))

(defsubst hatena-blog--api-parse-date (date)
  (date-to-time (replace-regexp-in-string
		 "\\(\\+[0-9][0-9]\\):\\([0-9][0-9]\\)$" "\\1\\2" date)))

(defsubst hatena-blog/api-id-epoch (id)
  (car-safe (last (split-string (or (car-safe (last (split-string id ":"))) "") "-"))))

(defsubst hatena-blog--api-buffer-content ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defsubst hatena-blog/api-iso-date ()
  (format-time-string "%Y-%m-%dT%TZ" (current-time) t))

(defsubst hatena-blog/api-update-entry (entry new)
  (setf (hatena-blog/entry-id entry) (hatena-blog/entry-id new))
  (setf (hatena-blog/entry-user entry) (hatena-blog/entry-user new))
  (setf (hatena-blog/entry-title entry) (hatena-blog/entry-title new))
  (setf (hatena-blog/entry-created entry) (hatena-blog/entry-created new))
  (setf (hatena-blog/entry-updated entry) (hatena-blog/entry-updated new))
  (setf (hatena-blog/entry-content entry) (hatena-blog/entry-content new)))

;; APIs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst hatena-blog/service-uri ()
  (let ((hatena-id (hatena/username))
	(blog-id (hatena-blog/blog-id)))
    (format "https://blog.hatena.ne.jp/%s/%s/atom" hatena-id blog-id)))

(defsubst hatena-blog/collection-uri ()
  (format "%s/entry" (hatena-blog/service-uri)))

(defsubst hatena-blog/member-uri (entry-id)
  (format "%s/%s" (hatena-blog/collection-uri) entry-id))

(defun hatena/username ()
  "Username for Hatena."
  (when (and (not hatena/username) (functionp hatena/username-function))
    (setq hatena/username (funcall hatena/username-function)))
  (unless hatena/username (error "No username specified"))
  hatena/username)

(defun hatena-blog/blog-id ()
  "Blog id for Hatena."
  (when (and (not hatena-blog/blog-id) (functionp hatena-blog/blog-id-function))
    (setq hatena-blog/blog-id (funcall hatena-blog/blog-id-function)))
  (unless hatena-blog/blog-id (error "No blog id specified"))
  hatena-blog/blog-id)

(defun hatena/blog/api-key ()
  "API-key for Hatena."
  (when (and (not hatena-blog/api-key) (functionp hatena-blog/api-key-function))
    (setq hatena-blog/api-key (funcall hatena-blog/api-key-function)))
  (unless hatena-blog/api-key (error "No api-key specified"))
  hatena-blog/api-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WSSE 認証ヘッダを計算
(defun hatena-blog/api-wsse ()
  "Make X-WSSE HTTP header field.
`hatena/username` and `hatena/password` are used to make the header field."
  (let* ((user (hatena/username))
	 (created (hatena-blog/api-iso-date))
	 (nonce (sha1 created))
	 (digest (concat nonce created hatena-blog/api-key))
	 (digest (base64-encode-string (sha1-binary digest)))
	 (nonce (base64-encode-string nonce)))
    (cons "X-WSSE"
	  (mapconcat #'identity
		     (list (format "UsernameToken Username=\"%s\"" user)
			   (format "PasswordDigest=\"%s\"" digest)
			   (format "Nonce=\"%s\"" nonce)
			   (format "Created=\"%s\"" created))
		     ", "))))

;; リクエストベース API
(defun hatena-blog/api-request (uri &optional method headers data)
  "Request URI with METHOD."
  (let* ((url-request-method (or method (or (and data "POST") "GET")))
	 (headers (cons (hatena-blog/api-wsse) headers))
	 (url-request-extra-headers headers)
	 (url-request-data (and data (encode-coding-string data 'utf-8)))
	 (buf (url-retrieve-synchronously uri)))
    (prog1
	(with-current-buffer buf
	  (let ((txt (hatena-blog--api-buffer-content))
	    (xml (xml-parse-region (point-min) (point-max))))
	    (cons (split-string txt "\n") xml)))
      (kill-buffer buf))))

;; リスト取得API
(defun hatena-blog/api-entries (type)
  "Retrieve hatena-blog entries."
  (let* ((uri (hatena-blog/collection-uri))
	 (res (hatena-blog/api-request uri)))
    (mapcar #'(lambda (entry)
		(let ((entry (hatena-blog/api-decode-entry entry)))
		  (setf (hatena-blog/entry-type entry) type)
		  entry))
	    (loop for e in (cdr (car (cdr res)))
		  when (and (listp e) (equal (nth 0 e) 'entry))
		  collect e))))

;; 保存 API
(defun hatena-blog/api-save (entry &optional terms update draft)
  "Save ENTRY."
  (unless (hatena-blog/entry-type entry) (error "Entry type not specified"))
  (let* ((type (hatena-blog/entry-type entry))
	 (user (hatena-blog/entry-user entry))
	 (title (hatena-blog/entry-title entry))
	 (content (hatena-blog/entry-source entry))
	 method uri data)
    (cond
     ((hatena-blog/entry-id entry) ; update
      (setq uri (hatena-blog/member-uri (hatena-blog/api-id-epoch (hatena-blog/entry-id entry))) method "PUT"))
     (t ; new post
      (setq uri (hatena-blog/collection-uri) method "POST" update t)))
    (setq data (hatena-blog/api-data-xml title content terms draft))
    (let ((result (hatena-blog/api-request uri method nil data)))
      (when (eq (car-safe (car-safe (cdr result))) 'entry)
	(let ((new-entry (hatena-blog/api-decode-entry (cadr result))))
	  (hatena-blog/api-update-entry entry new-entry)))
      (caar result))))

;; DELETE API
(defun hatena-blog/api-delete (entry)
  "Delete ENTRY."
  (let ((uri (hatena-blog/member-uri (hatena-blog/api-id-epoch (hatena-blog/entry-id entry))))
	(method "DELETE"))
    (caar (hatena-blog/api-request uri method))))

;; TODO: 更新の際に、タグ情報も付与するようにしないといけない
(defun hatena-blog/api-publish (entry)
  "Publish ENTRY. ENTRY must be a draft entry."
  (unless (eq (hatena-blog/entry-type entry) 'draft)
    (error "Publishing non-draft entry"))
  (hatena-blog/api-get-source entry)
  (hatena-blog/api-save entry nil t nil))

;; ソースを取得
(defun hatena-blog/api-get-source (entry &optional force)
  "Return source of ENTRY.
If no source is stored in ENTRY or FORCE is non-nil, then
retrieve it from the remote service."
  (when (and (or force (not (hatena-blog/entry-source entry)))
             (hatena-blog/entry-id entry))
    (let* ((uri (hatena-blog/member-uri (hatena-blog/api-id-epoch (hatena-blog/entry-id entry))))
           (source (nth 1 (hatena-blog/api-request uri)))
           (content (hatena-blog--api-decode
                     (nth 2 (assq 'content source)))))
      (setf (hatena-blog/entry-source entry) content)))
  (hatena-blog/entry-source entry))

;; internal functions

;; パースしてデータを取得
;; (<tag> (attiributes) (children))
(defun hatena-blog/api-decode-entry (entry)
  (let ((id (nth 2 (assq 'id entry)))
        (user (nth 2 (nth 2 (assq 'author entry))))
        (title (hatena-blog--api-decode (nth 2 (assq 'title entry))))
        (created (hatena-blog--api-parse-date (nth 2 (assq 'published entry))))
        (updated (hatena-blog--api-parse-date (nth 2 (assq 'updated entry))))
        (content (hatena-blog--api-decode (nth 2 (assq 'content entry)))))
    (hatena-blog/make-entry
     :id id :user user :title title :content content
     :created created :updated updated)))

;; エスケープが必要な文字について、順次エスケープしていく
(defun hatena-blog/api-escape (s)
  (mapc #'(lambda (x)
	    (setq s
		  (replace-regexp-in-string (car x) (cdr x) s)
		  )
	    )
	'(("&" . "&amp;") (">" . "&gt;") ("<" . "&lt;") ("\"" . "&quot;")))
  s)

(defun hatena-blog/api-create-term-request (terms)
  (if (listp terms)
      (apply #'concatenate 'string
	     (mapcar #'(lambda (term)
			 (if (stringp term)
			     (format "<category term='%s' />" term)
			   ""))
			 terms))
    "")
  )

(defun hatena-blog/api-data-xml (title content &optional terms draft)
  (let* ((date (hatena-blog/api-iso-date))
         (title (hatena-blog/api-escape title))
         (content (hatena-blog/api-escape content))
	 (term-string (hatena-blog/api-create-term-request terms))
	 (is_draft (if draft "yes" "no"))
	 )
    (format
     "<?xml version='1.0' encoding='utf-8'?>
<entry xmlns='http://www.w3.org/2005/Atom' xmlns:app='http://www.w3.org/2007/app'>
  <title>%s</title>
  <author><name>%s</name></author>
  <content type='text/plain'>%s</content>
  <updated>%s</updated>
  %s
  <app:control>
    <app:draft>%s</app:draft>
  </app:control>
</entry>" title "yasuharu519" content date term-string is_draft)))


(provide 'hatena-blog-api)
