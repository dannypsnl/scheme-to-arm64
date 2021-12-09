#lang racket

(define hooks
  (filter (lambda (path)
            (parameterize ([current-directory ".hooks"])
              (member 'execute (file-or-directory-permissions path))))
          (directory-list ".hooks")))

(for ([hook hooks])
  (system (format "ln -sf ~a/~a .git/hooks/~a" ".hooks" hook hook)))
