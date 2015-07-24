;; Org-mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(custom-set-variables
  '(org-hide-leading-stars t) ;; 見出しの余分な*を消す
  '(org-indent-mode t) ;; インデントモード
  '(org-startup-indented t) ;; インデント
  '(org-directory user-org-memo-directory) ;; org-default-notes-filesのディレクトリ
  '(org-agenda-files (list user-org-memo-directory)) ;; アジェンダ表示対象ファイル
  '(org-default-notes-file "gtd.org") ;; org-default-notes-fileのファイル名
  '(org-tag-alist
	'(("OFFICE" . ?o) ("HOME" . ?h) ("COMPUTER" . ?c) ("READING" . ?r) ("PROJECT" . ?p) ("WEB" . ?w)))
  '(org-capture-templates
	'(("t" "ToDo 項目の追加" entry (file+headline nil "INBOX")
	   "** TODO %^{ToDoのタイトル}%? %^g\n %T" :prepend)
	  ("j" "ジャーナル" entry (file+datetree (concat user-org-memo-directory "journal.org"))
	   "* %?\nEntered on %U")
	  ("w" "Webメモ" entry (file+headline (concat user-org-memo-directory "web.org") "Web")
	   "* %?\nEntered on %U")
	  ))
  '(org-todo-keywords
	'((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d!)" "SOMEDAY(s)" "CANCEL(c)")))
  ;; ここに設定できるのは
  ;; agenda, todo, search, tags, alltodo, tags-todo, todo-tree
  ;; tags-tree, occur-tree, or a user-defined function.
  '(org-agenda-custom-commands
	'(
	  ("r" "Review" tags "+LEVEL=2+CATEGORY=\"INBOX\"")
	  ("p" "Projects"
	   ((tags "PROJECT+LEVEL=3")))
	  ("h" "Office and Home lists"
	   ((agenda)
	    (tags-todo "OFFICE")
	    (tags-todo "HOME")
	    (tags-todo "COMPUTER")
	    (tags "+CATEGORY=\"WEB\"")))
	  ("d" "Daily Action List"
	   (
	    (agenda "" ((org-agenda-ndays 1)
			(org-agenda-sorting-strategy
			 (quote ((agenda time-up priority-down tag-up))))
			(org-deadline-warning-days 0)
			))))))
  '(org-refile-targets
	(quote (("gtd.org" :level . 1)
		("projects.org" :level . 2))))
  '(org-log-done 'time)
  '(hl-line-face 'underline)
  '(calender-holidays nil) ;; 標準の祝日を表示しない
  '(org-agenda-time-grid
	'((daily today required-timed)
	  "--------------------"
	  (900 1000 1100 1200 1300 1400 1500 1600 1700)))
  )

(defun baymacs/show-org-buffer (file)
  "Show an org-file on the current buffer"
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
	(switch-to-buffer buffer)
	(message "%s" file))
    (find-file (concat user-org-memo-directory file))))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'turn-on-font-lock) ;; orgモードでの強調表示
  (add-hook 'org-mode-hook 'baymacs/org-mode-hook)
  (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1))) ;; アジェンダ表示で下線を用いる

  (global-set-key (kbd "C-S-c") '(lambda () (interactive) (baymacs/show-org-buffer "gtd.org")))

  )


(defun baymacs/org-mode-hook ()
  )

