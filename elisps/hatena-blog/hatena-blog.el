(require 'tabulated-list)
(require 'hatena-blog-api)
(eval-when-compile (require 'cl))

;; define group 

(defgroup hatena-blog nil
  "Hatena blog."
  :group 'applications)

;; define custom

(defcustom hatena-blog/username nil
  "User name for hatena blog."
  :type '(choice string (const nil :tag "none")) ;; string もしくは nil
  :group 'hatena-blog)

(defcustom hatena-blog/auto-pager t
  "t means auto load"
  :type 'boolean
  :group 'hatena-blog)

(defcustom hatena-blog/major-mode 'html-mode
  "Major mode"
  :type 'function
  :group 'hatena-blog)

(defcustom hatena-blog/no-auto-save t
  "t means to disable auto-saving"
  :type 'boolean
  :group 'hatena-blog)

(defcustom hatena-blog/working-directory
  (concat (file-name-as-directory user-emacs-directory) "hatena/")
  "Directory used by hatena-blog"
  :type 'directory
  :group 'hatena-blog)

;; define face

(defface hatena-blog/list-delete
  '((t (:inherit error)))
  "Face for list item marked as to delete."
  :group 'hatena-blog)

(defface hatea-blog/list-publish
  '((t (:inherit font-lock-constant-face)))
  "Face for list item marked as to publish."
  :group 'hatena-blog)

;; define constant

(defconst hatena-blog--entries-buffer "*Hatena-blog Entries%s*")
(defconst hatena-blog--drafts-buffer "*Hatena-blog Drafts%s*")
(defconst hatena-blog--preview-buffer "*Hatena-blog Preview%s*")
(defconst hatena-blog--title-width 56)
(defconst hatena-blog--date-width 20)
(defconst hatena-blog--category-width 20)
(defconst hatena-blog--list-message
  "Commands: N, v, V, c, C; d, P, u, x; q to quit; ? for help.")

;; define variable

(defvar hatena-blog--entry nil)
(make-variable-buffer-local 'hatena-blog--entry)
(put 'hatena-blog--entry 'permanent-local t)
(defvar hatena-blog--file-name nil)
(make-variable-buffer-local 'hatena-blog--file-name)
(put 'hatena-blog--file-name 'permanent-local t)
(defvar hatena-blog--type nil)
(make-variable-buffer-local 'hatena-blog--type)
(defvar hatena-blog--current-page nil)
(make-variable-buffer-local 'hatena-blog--current-page)

(defvar hatena-blog/list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'hatena-blog/list-select)
    (define-key map (kbd "v") #'hatena-blog/list-preview)
    (define-key map (kbd "V") #'hatena-blog/list-view)
    (define-key map (kbd "d") #'hatena-blog/list-delete)
    (define-key map (kbd "D") #'hatena-blog/list-delete)
    (define-key map (kbd "P") #'hatena-blog/list-publish)
    (define-key map (kbd "u") #'hatena-blog/list-unmark)
    (define-key map (kbd "x") #'hatena-blog/list-execute)
    (define-key map (kbd "N") #'hatena-blog/list-retrieve-next)
    (define-key map (kbd "c") #'hatena-blog/new)
    (define-key map (kbd "C") #'hatena-blog/new-draft)
    (define-key map (kbd "q") #'kill-this-buffer)
    map))

(defvar hatena-blog/list-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'hatena-blog/list-mouse-select)
    map))

(defsubst hatena-blog/user ()
  (or hatena-blog/username (hatena/username)))

(defsubst hatena-blog--entry-file (entry)
  (concat (file-name-as-directory hatena-blog/working-directory)
          (hatena-blog--entry-unique-name entry)))

(defsubst hatena-blog--format-time (time)
  (format-time-string "%Y-%m-%d %T" time))

(defsubst hatena-blog--buffer-name (base user)
  (format base (or (and user (format " [%s]" user)) "")))

(defsubst hatena-blog--entries-buffer (&optional user)
  (hatena-blog--buffer-name hatena-blog--entries-buffer user))

(defsubst hatena-blog--drafts-buffer (&optional user)
  (hatena-blog--buffer-name hatena-blog--drafts-buffer user))

(defsubst hatena-blog--preview-buffer (&optional user)
  (hatena-blog--buffer-name hatena-blog--preview-buffer user))

(defsubst hatena-blog--list-get-cols ()
  (or (get-text-property (point) 'tabulated-list-entry)
      (car-safe (cdr (assq (tabulated-list-get-id) tabulated-list-entries)))))

;; commands

;;;###autoload
(defun hatena-blog/list (&optional arg)
  "List Hatena::Diary blog entries in a buffer."
  (interactive "P")
  (switch-to-buffer (hatena-blog/list-noselect arg))
  (message hatena-blog--list-message))

;;;###autoload
(defun hatena-blog/list-noselect (&optional arg)
  "List Hatena::Diary blog entries in a buffer without selecting it."
  (interactive "P")
  (let* ((user (and arg (hatena-blog--list-ask-user)))
         (buffer (get-buffer-create (hatena-blog--entries-buffer user))))
    (with-current-buffer buffer
      (hatena-blog/list-mode)
      (hatena-blog--list-set-user user)
      (hatena-blog--list-refresh)
      (tabulated-list-print))
    buffer))

;;;###autoload
(defun hatena-blog/list-draft (&optional arg)
  "List Hatena::Diary draft entries in a buffer."
  (interactive "P")
  (switch-to-buffer (hatena-blog/list-draft-noselect arg))
  (message hatena-blog--list-message))

;;;###autoload
(defun hatena-blog/list-draft-noselect (&optional arg)
  "List Hatena::Diary draft entries in a buffer without selecting it."
  (interactive "P")
  (let* ((user (and arg (hatena-blog--list-ask-user)))
         (buffer (get-buffer-create (hatena-blog--drafts-buffer user))))
    (with-current-buffer buffer
      (hatena-blog/list-mode)
      (hatena-blog--list-set-user user)
      (hatena-blog--list-refresh 'draft)
      (tabulated-list-print))
    buffer))

;;;###autoload
(defun hatena-blog/new-noselect (&optional entry buf)
  "Open a buffer for a Hatena::blog entry without selecting the buffer.
If ENTRY is specified, open a buffer for the entry. Otherwise,
open a buffer for a new entry."
  (interactive)
  (unless entry
    (setq entry (hatena-blog/make-entry :type 'blog :user (hatena-blog/user))))
  (unless (hatena-blog/entry-p entry) (error "Non entry object specified"))
  (let* ((title (or (hatena-blog/entry-title entry) " Title"))
         (content (or (hatena-blog/api-get-source entry) ""))
         (name (hatena-blog--entry-unique-name entry))
         (buf (or buf (get-buffer-create name))))
    (with-current-buffer buf
      (when (or (hatena-blog/entry-id entry) (= (point-min) (point-max)))
        ;; initialize
        (erase-buffer)
        (insert (format "# %s\n\n" title)) ;; タイトルを挿入
        (insert content)
        (unless (= (char-before (point)) ?\n) (insert "\n"))
        (when (fboundp hatena-blog/major-mode) (funcall hatena-blog/major-mode))
        (goto-char (point-min))
        (when hatena-blog/no-auto-save
          (auto-save-mode -1)
          (set (make-local-variable 'auto-save-default) nil))
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil))
      (setq hatena-blog/entry entry)
      (hatena-blog/edit-mode 1)
      buf)))

;;;###autoload
(defun hatena-blog/new (&optional entry)
  "Open a buffer for a Hatena::blog entry.
If ENTRY is specified, open a buffer for the entry. Otherwise,
open a buffer for a new entry."
  (interactive)
  (switch-to-buffer (hatena-blog/new-noselect entry)))

;;;###autoload
(defun hatena-blog/new-draft-noselect ()
  "Open a buffer for a new Hatena::Diary draft without selecting the buffer."
  (interactive)
  (hatena-blog/new (hatena-blog/make-entry :type 'draft :user (hatena-blog/user))))

;;;###autoload
(defun hatena-blog/new-draft ()
  "Open a buffer for a new Hatena::Diary draft."
  (interactive)
  (switch-to-buffer (hatena-blog/new-draft-noselect)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode hatena-blog/edit-mode
  "Minor mode for editing Hatena::Diary entry."
  :group 'hatena-diary
  (if hatena-blog/edit-mode
      ;; on
      (let ((name (hatena-blog--entry-file hatena-blog/entry)))
        (hatena-blog--set-file-name name)
        (set-buffer-modified-p nil)
        (hatena-blog--revert-function)
        (add-hook 'multi-indirect-buffer-hook
                  #'hatena-blog--revert-function nil t)
        (when (boundp 'multi-indirect-buffers-alist)
          (dolist (elt multi-indirect-buffers-alist)
            (with-current-buffer (cdr elt)
              (unless (null (buffer-base-buffer))
                (hatena-blog--revert-function)))))
        (add-hook 'write-file-functions #'hatena-blog/save nil t)
        (add-hook 'after-change-major-mode-hook
                  #'hatena-blog/restore-edit-mode nil t))
    ;; off
    (kill-local-variable 'revert-buffer-function)
    (remove-hook 'after-change-major-mode-hook #'hatena-blog/restore-edit-mode t)
    (remove-hook 'write-contents-functions #'hatena-blog/save t)))

(define-derived-mode hatena-blog/list-mode tabulated-list-mode
  "Hatena::Blog"
  "Major mode for listing Hatena::Diary entries.
The entry list is invoked by the commands \\[hatena-blog/list] and
\\[hatena-blog/list-draft].
In Hatena::Diary entry list mode, the following commands are defined:
\\<hatena-blog/list-mode-map>
\\[quit-window]    Remove the entry list from the display.
\\[hatena-blog/list-select]  Select this line's entry and open its source text.
\\[hatena-blog/list-preview]    Toggle preview mode.
\\[hatena-blog/list-view]    Select this line's entry and open its source text in `view-mode'.
\\[hatena-blog/list-delete]    Mark the buffer on this entry line for deletion.
\\[hatena-blog/list-publish]    Mark the buffer on this draft entry line for publish.
\\[hatena-blog/list-unmark]    Cancel all requested operations on buffer on this line.
\\[hatena-blog/list-execute]    Do marked operations.
\\[hatena-blog/list-retrieve-next]    Retrieve the next page of entries and add them to the list.
\\[hatena-blog/new]    Open a buffer for a new blog entry.
\\[hatena-blog/new-draft]    Open a buffer for a new draft entry."
  (add-hook 'tabulated-list-revert-hook #'hatena-blog--list-refresh nil t)
  (add-hook 'post-command-hook #'hatena-blog--list-auto-pager nil t)
  (setq show-trailing-whitespace nil)
  (hl-line-mode 1))

(define-minor-mode hatena-blog/list-preview-mode
  "Minor mode for Hatena::Diary list preview."
  :group 'hatena-blog
  (if hatena-blog/list-preview-mode
      ;; on
      (add-hook 'post-command-hook #'hatena-blog/list-preview1 nil t)
    ;; off
    (remove-hook 'post-command-hook #'hatena-blog/list-preview1 t)
    (hatena-blog/list-close-preview)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hatena-blog/list-select ()
  "Select this line's entry and open its source text."
  (interactive)
  (hatena-blog/list-close-preview)
  (let ((entry (tabulated-list-get-id)))
    (when (hatena-blog/entry-p entry) (hatena-blog/new entry))))

(defun hatena-blog/list-mouse-select (event)
  "Select the entry whose line you click on and open its source text."
  (interactive "e")
  (hatena-blog/list-close-preview)
  (select-window (posn-window (event-end event)))
  (let ((entry (tabulated-list-get-id (posn-point (event-end event)))))
    (when (hatena-blog/entry-p entry) (hatena-blog/new entry))))

(defun hatena-blog/list-view ()
  "Select this line's entry and open its source text in `view-mode'."
  (interactive)
  (hatena-blog/list-close-preview)
  (let ((entry (tabulated-list-get-id)))
    (when (hatena-blog/entry-p entry)
      (let ((buf (hatena-blog/new-noselect entry)))
        (with-current-buffer buf
          (view-mode 1))
        (switch-to-buffer buf)))))

(defun hatena-blog/list-preview1 ()
  (let ((entry (tabulated-list-get-id))
        (win (selected-window))
        (buf (get-buffer-create (hatena-blog--preview-buffer hatena-blog/username))))
    (if (not (hatena-blog/entry-p entry))
        (hatena-blog/list-close-preview)
      (with-current-buffer buf ;; バッファにコンテンツを挿入
        (setq show-trailing-whitespace nil)
        (erase-buffer)
        (insert (hatena-blog/entry-content entry))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (cond
         ((eq (hatena-blog/entry-type entry) 'blog) ;; ブログだった場合
          (if (fboundp 'w3m-buffer) (w3m-buffer) (html-mode)))
         ((eq (hatena-blog/entry-type entry) 'draft) ;; ドラフトだった場合
          (when (fboundp hatena-blog/major-mode) (funcall hatena-blog/major-mode)))))
      (switch-to-buffer-other-window buf t)
      (select-window win))))

(defun hatena-blog/list-preview ()
  "Toggle preview mode.
A preview of this line's entry shows up on the other buffer.
For blog entries, the preview is either formatted text generated
by `w3m' or `html-mode'. For draft entries, the preview is a
source text with `hatena-blog/major-mode'."
  (interactive)
  (if hatena-blog/list-preview-mode ; toggle
      (hatena-blog/list-preview-mode 0)
    (hatena-blog/list-preview-mode 1)
    (hatena-blog/list-preview1)))

(defun hatena-blog/list-close-preview ()
  (let ((win (get-buffer-window (hatena-blog--preview-buffer hatena-blog/username))))
    (when win (delete-window win))))

(defun hatena-blog/list-delete ()
  "Mark the buffer on this entry line for deletion."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry (error "No entry at point")))
  (hatena-blog--list-put-mark "D")
  (hatena-blog--list-apply-face 'hatena-blog/list-delete)
  (forward-line 1))

(defun hatena-blog/list-publish ()
  "Mark the buffer on this draft entry line for publish."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry (error "No entry at point"))
    (unless (and (hatena-blog/entry-p entry)
                 (eq (hatena-blog/entry-type entry) 'draft))
      (error "No draft entry at point")))
  (hatena-blog--list-put-mark "P")
  (hatena-blog--list-apply-face 'hatena-blog/list-publish)
  (forward-line 1))

(defun hatena-blog/list-unmark (&optional noforward)
  "Cancel all requested operations on buffer on this line."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry (error "No entry at point")))
  (hatena-blog--list-put-mark " ")
  (hatena-blog--list-apply-face 'default)
  (unless noforward (forward-line 1)))

(defun hatena-blog/list-execute1 (entry what)
  (let (status)
    (cond
     ((= what ?D)
      (setq status (hatena-blog/api-delete entry)))
     ((= what ?P)
      (setq status (hatena-blog/api-publish entry))))
    (when status
      (message status)
      (string-match-p "200 OK$" status))))

(defun hatena-blog/list-execute ()
  "Do marked operations."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((entry (tabulated-list-get-id)))
        (cond
         ((hatena-blog/entry-p entry)
          (if (prog1 (hatena-blog/list-execute1 entry (char-after))
                (hatena-blog/list-unmark t))
              (hatena-blog--list-delete)
            (forward-line 1)))
         (t (forward-line 1)))))))

(defun hatena-blog/list-retrieve-next ()
  "Retrieve the next page of entries and add them to the list."
  (interactive)
  (let* ((type (or hatena-blog--type 'blog))
         (user (hatena-blog/user))
         (page (1+ (or hatena-blog--current-page 0)))
         (es (hatena-blog/api:entries type user page)))
    (setq hatena-blog--current-page page)
    (hatena-blog--list-append es)
    (tabulated-list-print t)))

(defun hatena-blog/save (&optional entry)
  "Save the Hatena::Diary entry.
If the entry is a blog entry, it is immediately reflected to the
public Web page of Hatena::Diary. If the entry is a draft entry,
it is saved to the draft collection."
  (interactive)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let ((entry (or entry hatena-blog/entry)) status)
      (unless (hatena-blog/entry-p entry)
        (error "This buffer is not a Hatena::Diary entry."))
      (destructuring-bind (title content) (hatena-blog--parse-content)
        (setf (hatena-blog/entry-title entry) title)
        (setf (hatena-blog/entry-source entry) content)
        (setq status (hatena-blog/api-save entry))
        (message status)
        (when (or (string-match-p "200 OK$" status)
                  (string-match-p "201 Created$" status))
          (setq hatena-blog/entry entry)
          (setq last-coding-system-used 'utf-8)
          (delete-auto-save-file-if-necessary)
          (hatena-blog/edit-mode 1)
          t)))))

(defun hatena-blog/save-as-draft ()
  "Save current entry as a draft.
If the current entry is already a draft, then it is saved as
usual. Otherwise, if the current entry is a blog entry, then the
contents of the entry is saved as a new draft entry."
  (interactive)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let ((entry hatena-blog/entry))
      (if (eq (hatena-blog/entry-type entry) 'draft)
          (hatena-blog/save)
        (let* ((user (hatena-blog/entry-user entry))
               (entry (hatena-blog/make-entry :type 'draft :user user)))
          (hatena-blog/save entry))))))

(defun hatena-blog/revert (ignore1 ignore2)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let ((mod (buffer-modified-p))) ;; もし変更済なら
      (set-buffer-modified-p nil) ;; 変更済フラッグを消す
      (when (fboundp 'multi-mode-quit) (multi-mode-quit)) ;; マルチモードを使っている場合(一部にメジャーモードを混ぜる?)
      (set-buffer-modified-p mod)) ;; 変更済フラッグを元に戻す
    (hatena-blog/new-noselect hatena-blog/entry (current-buffer))))

(defun hatena-blog/restore-edit-mode (&rest ignore)
  "Re-enable `hatena-blog/edit-mode'."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (when (and hatena-blog--file-name
               (string= (expand-file-name (buffer-file-name))
                        (expand-file-name hatena-blog--file-name))
               (or (and (numberp hatena-blog/edit-mode)
                        (<= hatena-blog/edit-mode 0))
                   (null hatena-blog/edit-mode)))
      (hatena-blog/edit-mode 1))))
(put 'hatena-blog/restore-edit-mode 'permanent-local-hook t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hatena-blog--list-ask-user ()
  (read-string "User: " (hatena-blog/user)))

(defun hatena-blog--list-set-user (user)
  (when user (set (make-local-variable 'hatena-blog/username) user)))

(defun hatena-blog--list-title (entry)
  (let ((title (hatena-blog/entry-title entry)))
    (propertize
     (replace-regexp-in-string "^\\(\\[.*?\\]\\)*[ \t\r\n]*" "" title)
     'mouse-face 'highlight
     'local-map hatena-blog/list-mode-map)))

(defun hatena-blog--list-created (entry)
  (hatena-blog--format-time (hatena-blog/entry-created entry)))

(defun hatena-blog--list-category (entry)
  (let ((title (hatena-blog/entry-title entry))
        (regexp "^\\[\\(.*?\\)\\]")
        categories)
    (while (string-match regexp title)
      (push (match-string 1 title) categories)
      (setq title (replace-regexp-in-string regexp "" title)))
    (mapconcat #'identity (sort categories #'string<) ",")))

(defun hatena-blog--list-append (entries &optional initialize)
  (when initialize (setq tabulated-list-entries nil))
  (setq tabulated-list-entries
        (append tabulated-list-entries
                (mapcar
                 #'(lambda (entry)
                     (let ((title (hatena-blog--list-title entry))
                           (created (hatena-blog--list-created entry))
                           (categories (hatena-blog--list-category entry)))
                       (list entry (vector "" title created categories))))
                 entries))))

(defun hatena-blog--list-refresh (&optional type)
  (setq tabulated-list-use-header-line t
        tabulated-list-format
        (vector
         `(" " 1 t)
         `("Title" ,hatena-blog--title-width t)
         `("Date" ,hatena-blog--date-width t :right-align t)
         `("Categories" ,hatena-blog--category-width t)))
  (let* ((type (or type hatena-blog--type 'blog))
         (user (hatena-blog/user))
         (entries (hatena-blog/api-entries type)))
    (setq hatena-blog--type type
          hatena-blog--current-page 1)
    (hatena-blog--list-append entries t))
  (tabulated-list-init-header))

(defun hatena-blog--list-auto-pager ()
  (when (and hatena-blog/auto-pager (eobp))
    (forward-line -1)
    (hatena-blog/list-retrieve-next)))

(defun hatena-blog--list-put-mark (mark)
  (let* ((point (point))
         (entry (tabulated-list-get-id))
         (cols (hatena-blog--list-get-cols))
         (pos (line-beginning-position))
         (inhibit-read-only t))
    (when entry
      (aset cols 0 mark)
      (beginning-of-line)
      (delete-region pos (1+ pos))
      (insert mark)
      (hatena-blog--list-add-line-properties
       'tabulated-list-id entry 'tabulated-list-entry cols)
      (goto-char point))))

(defun hatena-blog--list-delete ()
  (let ((entry (tabulated-list-get-id))
        (inhibit-read-only t))
    (setq tabulated-list-entries
          (loop for e in tabulated-list-entries
                unless (eq entry (car e))
                collect e))
    (when entry
      (delete-region (line-beginning-position) (1+ (line-end-position))))))

(defun hatena-blog--list-apply-face (face)
  (let ((cols (hatena-blog--list-get-cols))
        (prop 'font-lock-face)
        (inhibit-read-only t))
    (loop for i below (length cols)
          for col = (propertize (aref cols i) prop face)
          do (aset cols i col))
    (hatena-blog--list-add-line-properties prop face)))

(defun hatena-blog--list-add-line-properties (&rest props)
  (add-text-properties (line-beginning-position) (line-end-position) props))

(defun hatena-blog--set-file-name (file-name)
  (setq hatena-blog--file-name file-name)
  (set-visited-file-name file-name))

(defun hatena-blog--entry-unique-name (entry)
  (let ((id (hatena-blog/entry-id entry))
        (type (hatena-blog/entry-type entry)))
    (if id
        (car-safe (last (split-string id ":")))
      (format "new-hatena-%s-entry" type))))

(defun hatena-blog--parse-content ()
  (let (pos title)
    (save-excursion
      (goto-char (point-min))
      (if (not (re-search-forward "^\\*" nil t))
          (setq title (read-string "Title: "))
        (setq pos (point))
        (re-search-forward "$" nil t)
        (setq title (buffer-substring-no-properties pos (point))))
      (skip-chars-forward " \t\r\n")
      (list title (buffer-substring-no-properties (point) (point-max))))))

(defun hatena-blog--revert-function ()
  (set (make-local-variable 'revert-buffer-function) 'hatena-blog/revert))

;;;;;;; evil のための設定
(push 'hatena-blog/list-mode evil-motion-state-modes)
(evil-make-overriding-map hatena-blog/list-mode-map)
(evil-add-hjkl-bindings hatena-blog/list-mode-map 'motion)

(provide 'hatena-blog)
