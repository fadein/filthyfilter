#!/usr/local/bin/csi -s

(use srfi-1)
(use regex)
(use posix)

(define filthyfilter
  (let ((blank? (lambda (l) (eq? 0 (string-length l))))
		(digit-rx (regexp "^\\d+$"))
		(time-rx (regexp "(\\d{2}):(\\d{2}):(\\d{2},\\d{3}) --> (\\d{2}):(\\d{2}):(\\d{2},\\d{3})")))
		

  (lambda (filename)
  (with-input-from-file filename
						(lambda ()
						  (let loop ((line (read-line))
									 (state 'digit))
							(cond
							  ((eof-object? line)
							   'done)
							  (
							   
							(if (not )


;10.632000 11.240000 1

; vim:ft=chicken:
