#!/usr/local/bin/csi -s
;if hitting backspace produces  chars, do :set t_kb=

(use srfi-1)
(use regex)
(use posix)
(use fmt)
(use getopt-long)

(define *action-mute* 0)
(define *action-skip* 1)

(define cusses (regexp "that|talking|parkour|kid"))

(define option-spec
  `((cusses	"list of cusswords to filter out"
			(single-char #\c) (value (optional WORDS)))
	(help	"show help text" (single-char #\h))
	(file	"name of subtitle file to process"
			(single-char #\f)
			(value (required FILE)
				   (predicate ,file-exists?)))))

;srt parser
(define filthyfilter
  (let ((blank? (lambda (l) (eq? 0 (string-length l))))
		(digit-rx (regexp "^\\d+$"))
		(time-rx (regexp "(\\d{2}):(\\d{2}):(\\d{2}),(\\d{3}) --> (\\d{2}):(\\d{2}):(\\d{2}),(\\d{3})")))


	;open the original .srt file and emit an edl file with the
	;cusses muted out.
	;next phase of project: edit the .srt to make cusses look like
	;#$%@&! in the subtitles in addition to emitting an edl
	(lambda (filename cusses)
	  ;todo - turn string of cusses into a regex with alternations, or 
	  ;something
	  (with-input-from-file
		filename
		(lambda ()
		  (let loop ((line (read-line))
					 (state 'digit)
					 (timestamps #f))

			;(printf "line:~a~n" line)
			(cond

			  ;end of input file - done
			  ((eof-object? line)
			   #t)

			  ;sequence marker
			  ((and (eq? state 'digit)
					(string-match digit-rx line))
			   (loop (read-line) 'time #f))

			  ;time range for subtitle - keep it
			  ((and (eq? state 'time)
					(string-match time-rx line))
			   (loop (read-line)
					   'text
					   (string-match time-rx line)))

			  ;text for subtitle - if it has a cuss write
			  ;a line of edl
			  ((eq? state 'text)
			   (and (string-search cusses line)
					(output-edl timestamps))
			   (loop (read-line) 'blank #f))

			  ;on the blank line between subs
			  ((and (eq? state 'blank)
					(blank? line))
			   (loop (read-line) 'digit #f))

			  (else
				(error "Parse error in input file")))))))))

(define output-edl
  (lambda (timestamps)
	;(printf "edl line for ~a~n" timestamps)
	(let ((h1 (string->number (list-ref timestamps 1)))
		  (m1 (string->number (list-ref timestamps 2)))
		  (s1 (string->number (list-ref timestamps 3)))
		  (u1 (string->number (list-ref timestamps 4)))
		  (h2 (string->number (list-ref timestamps 5)))
		  (m2 (string->number (list-ref timestamps 6)))
		  (s2 (string->number (list-ref timestamps 7)))
		  (u2 (string->number (list-ref timestamps 8))))
	  ;(printf "h1:~a m1:~a s1:~a u1:~a~n" h1 m1 s1 u1)
	  ;(printf "h2:~a m2:~a s2:~a u2:~a~n" h2 m2 s2 u2)
	  (let ((start (+ (* 3600 h1) (* 60 m1) s1))
			(end   (+ (* 3600 h2) (* 60 m2) s2)))
		;(printf "start:~a end:~a~n" start end)
		(printf "~a ~a ~a~n"
				(fmt #f start "." (pad-char #\0 (pad/right 6 u1)))
				(fmt #f end "." (pad-char #\0 (pad/right 6 u2)))
				*action-mute*)))))

(define opts (getopt-long  `(,(program-name) ,@(command-line-arguments)) option-spec))

(if (assoc 'help opts)
  (begin
	(print (usage option-spec))
	(exit 0)))

(print opts)
(exit 0)

(filthyfilter "ElderBrianFalorParKour.srt" cusses)

;10.632000 11.240000 1

; vim:ft=chicken:
