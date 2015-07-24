;;; emacs-aoj
;;; -- A client of AOJ in emacs environment

(require 'url)

(defvar aoj-problem-volumes '(100 0 1 2 3 5 6 10 11 12 13 15 20 21 22 23 24 25 26)) 

(defconst aoj-api-base-uri "http://judge.u-aizu.ac.jp/onlinejudge/")

(defconst aoj-api-endpoints-alist
  '((description . "description.jsp")
    (submit . "webservice/submit")
    (user-search . "webservice/user")
    (problem-search . "webservice/problem")
    (problem-list-search . "webservice/problem_list")
    (user-list-search . "webservice/user_list")
    (solved-record-search . "webesrvice/solved_record")
    (status-log-search . "webservice/status_log")
    (judge-detail-search . "webservice/judge")
    (problem-category-search . "webservice/problem_category")
    (source-search . "webservice/source")
    (content-list . "webservice/content_list")
    (content-info . "webservice/content_info")
    (content-standing . "webservice/content_standing")
    (contest-problem . "webservice/contest_problem")
    (contest-status-log . "webservice/contest_status_log")
    ))

(defun emacs-aoj-api--user-search(user-id)
  )

(defun emacs-aoj-api--problem-search(problem-id)
  )

(defun emacs-aoj-api--problem-list(volume)
  )

(defun emacs-aoj-api--get-problem-description(id)
  )



(provide 'emacs-aoj)
