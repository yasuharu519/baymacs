(defmacro dotpersonal|call-func (func &optional msg)
  "関数が定義されている場合は関数呼び出し。
  MSGがnilでない場合は `*Messages' に表示する."
  `(progn
     (when ,msg (personal-buffer/message ,msg))
     (if (fboundp ',func) (,func))))

(provide 'core-dotpersonal)
