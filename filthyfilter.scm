#!/usr/local/bin/csi -s
;if hitting backspace produces  chars, do :set t_kb=

(use srfi-1)
(use regex)
(use posix)
(use fmt)
(use getopt-long)
(use base64)

(define *version* "0.1")
(define *action-mute* 0)
(define *action-skip* 1)

;; default list of cusses to mute, base64 encrypted so your eyes don't melt
;; by just reading the source
(define *cusses* (base64-decode "XGIoKG1vdGhlcik/Zit1K2M/aytcdyp8c2hpdFx3KnxkYW0obXxuKWl0fChkdW1iKT9hc3MoaG9sZSk/fGN1bnR8Yml0Y2h8Z29kZGFtXHcqfHBlbmlzfHZhZ2luYSlcYg=="))

;subtitles file parser - .srt format
(define srt-parser
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
					 (line-num 1)
					 (state 'digit)
					 (timestamps #f))

			(printf "line #~a state:~a ~a~n" line-num (symbol->string state) line)

			(cond

			  ;end of input file - done
			  ((eof-object? line)
			   #t)

			  ;sequence marker
			  ((and (eq? state 'digit)
					(string-match digit-rx line))
			   (loop (read-line) (add1 line-num) 'time #f))

			  ;time range for subtitle - keep it
			  ((and (eq? state 'time)
					(string-match time-rx line))
			   (loop (read-line)
					 (add1 line-num)
					   'text
					   (string-match time-rx line)))

			  ;on text for subtitle - if it has a cuss write
			  ;a line of edl
			  ((eq? state 'text)
			   (cond
				 ;if this line is blank, go into blank state
				 ((blank? line)
				  (loop line line-num 'blank #f))

				 ;if this line has a cuss, output EDL line, goto cuss
				 ((string-search cusses line)
				  (output-edl timestamps)
				  (loop (read-line) (add1 line-num) 'cuss #f))

				 ;otherwise, do this again on the next line
				 (else
				   (loop (read-line) (add1 line-num) 'text timestamps))))

			  ;on text for subtitle - but we already wrote an EDL for it
			  ((eq? state 'cuss)
			   (cond
				 ;if this line is blank, go into blank state
				 ((blank? line)
				  (loop line line-num 'blank #f))

				 ;otherwise, do this again on the next line
				 (else
				   (loop (read-line) (add1 line-num) 'cuss #f))))

			  ;on the blank line between subs
			  ((and (eq? state 'blank)
					(blank? line))
			   (loop (read-line) (add1 line-num) 'digit #f))

			  (else
				(error (fmt #f "Parse error in input file at line "
							line-num nl
							"Expected state: " (symbol->string state) nl
							"Line:" nl (space-to 2) line))))))))))

(define filthyfilter srt-parser)

;; output a line of EDL
;; accepts list of timestamp elements
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

;;specify options to accept for getopt
(define option-spec
		 `((cusses	"list of cusswords to filter out"
					(single-char #\c) (value (optional WORDS)))
		   (help	"show help text" (single-char #\h))
		   (file	"name of subtitle file to process"
					(single-char #\f)
					(value (required FILE)
						   (predicate ,file-exists?)))
		   (encrypt "encrypt plaintext cusses so source isn't offensive"
					(single-char #\e) (value (required REGEXP)))
		   (dump    "print encrypted cusses in plaintext. OFFENSIVE"
					(single-char #\d))
		   (version	,(string-append "show program version (" *version* ")")
					(single-char #\v))))

;parse command-line options
(define opts
  (getopt-long
	(cons (program-name) (command-line-arguments))
	option-spec))

;print usage info for --help option
(and (assoc 'help opts)
  (begin
	(print (usage option-spec))
	(exit 0)))

;display program version
(and (assoc 'version opts)
  (begin
	(print (program-name) " v" *version*)
	(exit 0)))

(and (assoc 'dump opts)
	 (begin
	   (print *cusses*)
	   (exit 0)))

;let user specify cusses to mute on cmdline
(let ((cuss-spec (assoc 'cusses opts)))
  (if cuss-spec
	(set! *cusses* (regexp
					 (string-join (string-tokenize (cdr cuss-spec)) "|")
					 #t))
	(set! *cusses* (regexp *cusses* #t))))

(and-let* ((cuss-spec (assoc 'encrypt opts))
		   (pre (cdr cuss-spec))
		   (crypted (base64-encode pre)))
  (print "I hope you doubled up on the backslashes!")
  (print "Paste this back into the top of this program:")
  (printf "(define *cusses* (regexp (base64-decode \"~a\") #t))~n" crypted)
  (exit 0))

;get the name of the file to filter
(define filename
  (and-let* ((file-spec (assoc 'file opts))
			 (name (cdr file-spec)))
	name))

(if filename
  (filthyfilter filename *cusses*)
  (begin
	(print "Please supply a filename via the --file option")
	(print (usage option-spec))
	(exit 1)))

;10.632000 11.240000 1

; vim:ft=chicken:
