#!/usr/local/bin/csi -s
;if hitting backspace produces  chars, do :set t_kb=

(use srfi-1)
(use srfi-13)
(use regex)
(use posix)
(use fmt)
(use getopt-long)
(use base64)
(use files)

(define *version* "0.1")
(define *action-mute* 1)
(define *action-skip* 0)

;; default list of cusses to mute, base64 encrypted so your eyes don't melt
;; by just reading the source
(define *cusses* (base64-decode "XGIoYmFzdGFyZHwobW90aGVyKT9mK3UrYz9rK1x3KnxzaGl0XHcqfChnb2Q/KT9kYW0obXxuKWl0fChkdW1iKT9hc3MoaG9sZSk/fGN1bnR8Yml0Y2h8cGVuaXN8dmFnaW5hfCgob2gsP3xteSlccyspZ29kfGplc3VzfGNocmlzdClcYg=="))

;subtitles file parser - .srt format
(define srt-parser
  (let ((blank? (lambda (l) (eq? 0 (string-length l))))
        (seq-rx (regexp "^\\d+$"))
        (time-rx (regexp "(\\d{2}):(\\d{2}):(\\d{2}),(\\d{3}) --> (\\d{2}):(\\d{2}):(\\d{2}),(\\d{3})")))

    ;open the original .srt file and emit an edl file with the
    ;cusses muted out.
    ;next phase of project: edit the .srt to make cusses look like
    ;#$%@&! in the subtitles in addition to emitting an edl
    (lambda (temp-subfile edl-file)
      (let ((cuss-count 0)
            (subs-out (open-output-file temp-subfile))
            (edl-out (open-output-file edl-file)))
        (with-input-from-file
          *filename*
          (lambda ()
            (let loop ((line (read-line))
                       (line-num 1)
                       (state 'seq)
                       (timestamps #f))

              ;(printf "line #~a state:~a ~a~n" line-num (symbol->string state) line)

              (cond

                ;end of input file - done
                ((eof-object? line)
                 #t)

                ;sequence marker
                ((and (eq? state 'seq)
                      (string-match seq-rx line))
                 (fprintf subs-out "~a~n" line)
                 (loop (read-line) (add1 line-num) 'time #f))

                ;time range for subtitle - keep it
                ((and (eq? state 'time)
                      (string-match time-rx line))
                 (fprintf subs-out "~a~n" line)
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
                   ((string-search *cusses* line)
                    (output-edl timestamps edl-out)
                    (set! cuss-count (add1 cuss-count))
                    (fprintf subs-out "~a~n" (replace-all-cusses line))
                    (loop (read-line) (add1 line-num) 'cuss #f))

                   ;otherwise, do this again on the next line
                   (else
                     (fprintf subs-out "~a~n" line)
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
                 (fprintf subs-out "~a~n" line)
                 (loop (read-line) (add1 line-num) 'seq #f))

                (else
                  (error (fmt #f "Parse error in input file at line "
                              line-num nl
                              "Expected state: " (symbol->string state) nl
                              "Line:" nl (space-to 2) line)))))))
        ;close the output ports
        (close-output-port subs-out)
        (close-output-port edl-out)
        cuss-count))))

;return a new string of punctuation characters of the given length
(define make-random-cuss-string
  (let* ((cuss-chars (list->vector (list #\! #\@ #\# #\$ #\% #\& #\*)))
		 (num-chars (vector-length cuss-chars))
		 (shuffle-vector
		   (lambda (v)
			 (do (( i 0 (add1 i)))
			   ((= i (vector-length v)))
			   (let*
				 ((j (random (vector-length v)))
				  (t (vector-ref v j)))
				 (vector-set! v j (vector-ref v i))
				 (vector-set! v i t))))))
	(lambda (len)
	  (let shuffle-loop ((len len) (accum '()))
		(if (< len 0)
		  (list->string accum)
		  (begin
			(shuffle-vector cuss-chars)
			;select len of them, accumulate into accum
			(do
			  ((i 0 (add1 i)))
			  ((or (= i len) (= i num-chars)))
			  (set! accum (append accum (list (vector-ref cuss-chars i)))))
			(shuffle-loop (- len num-chars) accum)))))))

;return a sanitized line of dialog
(define replace-all-cusses
  (lambda (input-string)
    (fold
      ;kons
      (lambda (span work-in-progress)
        (let ((start (car span)) (end (cadr span)))
          (string-replace work-in-progress
                          (make-random-cuss-string (- end start))
                          start end)))
      ;knil
      input-string
      ;clist1
      (let loop ((start 0) (extents '()))
        (let ((result (string-search-positions *cusses* input-string start)))
          (if result
            (loop (cadar result) (cons (car result) extents))
            extents))))))

;; output a line of EDL to a port
;; accepts list of timestamp elements
;; and a port to write them out to
(define output-edl
  (lambda (timestamps port)
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
		(fprintf port "~a ~a ~a~n"
				(fmt #f start "." (pad-char #\0 (pad/right 6 u1)))
				(fmt #f end "." (pad-char #\0 (pad/right 6 u2)))
				*action-mute*)))))

;;specify options to accept for getopt
(define option-spec
		 `((cusses	"regex of cusswords to filter out"
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
  (printf "(define *cusses* (base64-decode \"~a\"))~n" crypted)
  (exit 0))

;get the name of the file to filter
(define *filename*
  (and-let* ((file-spec (assoc 'file opts))
			 (name (cdr file-spec)))
	name))

(define parse-dispatch
  (lambda ()
	(let-values (((filename-root extension) (split-extension *filename*)))

	  (let* ((parser (choose-parser extension))
			 (edl-file (make-edl-filename filename-root))
			 (temp-subfile (make-temp-filename *filename*)))
		(if (not (procedure? parser))
		  (error (fmt #f "Could not identify parse procedure for file " *filename*)))

        (let ((cuss-count (parser temp-subfile edl-file)))
          (cond
            ((> cuss-count 0)
              (delete-file* *filename*)
              (file-move temp-subfile *filename*)
              (printf "Filtered ~a cusses from ~a~nCreated ~a~n"
                      cuss-count *filename* edl-file))
            (else
              (delete-file* temp-subfile)
              (delete-file* edl-file)
              (printf "No cusses in this movie!~n"))))))))

;split a file's extension from it's name
;multiple return values
(define split-extension
  (lambda (filename)
	(let* ((extension-index (string-index-right filename #\.))
		   (head (string-copy filename 0 extension-index))
		   (ext  (string-copy filename (+ 1 extension-index))))
	  (values head ext))))

;add the .edl extension to the input
(define make-edl-filename
  (lambda (name)
	(string-concatenate `(,name ".edl"))))

;generate a temporary filename by prepending a . and appending .tmp
(define make-temp-filename
  (lambda (name)
	(string-concatenate `("." ,name ".tmp"))))

;decide which parser function to invoke based solely on the extension of the input file
(define choose-parser
  (lambda (type)
	(let ((res (assoc type `(("srt" . ,srt-parser)))))
	  (if (not res)
		(error (string-concatenate `("Could not find a parser for " ,type " files"))))
	  (cdr res))))


(if *filename*
  (parse-dispatch)
  (begin
	(print "Please supply a filename via the --file option")
	(print (usage option-spec))
	(exit 1)))

; vim:filetype=chicken expandtab tabstop=4 textwidth=74:

