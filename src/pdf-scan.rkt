#lang racket

;;; uuid and yaml libraries require installation from package manager
(require racket/system
         racket/draw
         file/sha1
         net/base64
         uuid  
         yaml)

;;;
;;; PDF manipulation functions
;;;

;; Poppler paths
(define pdftocairo-exe-path (if (eq? (system-type 'os) 'windows)
                                "../bin/pdftocairo.exe"
                                "../bin/pdftocairo"))
(define pdfinfo-exe-path (if (eq? (system-type 'os) 'windows)
                             "../bin/pdfinfo.exe"
                             "../bin/pdfinfo"))
(define pdftext-exe-path (if (eq? (system-type 'os) 'windows)
                             "../bin/pdftext.exe"
                             "../bin/pdftext"))

;; rasterise pdf file into png image file
(define (pdf-to-png resolution-dpi input-bytes)
  (let-values ([(subproc stdout stdin stderr)
                (subprocess #f #f #f pdftocairo-exe-path "-png" "-r" (~a resolution-dpi) "-singlefile" "-" "-")]
               [(line-terminator) (if (eq? (system-type 'os) 'windows) 'return-linefeed 'linefeed)])
    (write-bytes input-bytes stdin) ; pump the pdf file into the subprocess
    (close-output-port stdin)
    ; nasty bug on windows port of pdftocairo which means all output sent to stdout is in text mode, adding #13 in front of every #10 byte!
    ; so we read it in line by line stripping out CRLF (13 10) byte combinations
    (do ([bs (read-bytes-line stdout line-terminator) (read-bytes-line stdout line-terminator)]
         ; on each iteration, re-insert the line terminator character (10) that was stripped by 'read-bytes-line'
         [output-bytes (make-bytes 0) (bytes-append output-bytes bs (bytes 10))])
      ; when we get to the end of file, return the accumulated byte string, chopping off the last #10 byte
      ((eof-object? bs) (close-input-port stdout)
                        (close-input-port stderr)
                        (subbytes output-bytes 0 (sub1 (bytes-length output-bytes)))))))

;; rasterise pdf file into thumbnail png image file
(define (pdf-to-png-thumbnail pixels input-bytes)
  (let-values ([(subproc stdout stdin stderr)
                (subprocess #f #f #f pdftocairo-exe-path "-png" "-scale-to" (~a pixels) "-singlefile" "-" "-")]
               [(line-terminator) (if (eq? (system-type 'os) 'windows) 'return-linefeed 'linefeed)])
    (write-bytes input-bytes stdin) ; pump the pdf file into the subprocess
    (close-output-port stdin)
    ; nasty bug on windows port of pdftocairo which means all output sent to stdout is in text mode, adding #13 in front of every #10 byte!
    ; so we read it in line by line stripping out CRLF (13 10) byte combinations
    (do ([bs (read-bytes-line stdout line-terminator) (read-bytes-line stdout line-terminator)]
         ; on each iteration, re-insert the line terminator character (10) that was stripped by 'read-bytes-line'
         [output-bytes (make-bytes 0) (bytes-append output-bytes bs (bytes 10))])
      ; when we get to the end of file, return the accumulated byte string, chopping off the last #10 byte
      ((eof-object? bs) (close-input-port stdout)
                        (close-input-port stderr)
                        (subbytes output-bytes 0 (sub1 (bytes-length output-bytes)))))))

;; retrieve metadata from pdf file
(define (pdf-to-info input-bytes)
  (let-values ([(subproc stdout stdin stderr) (subprocess #f #f #f pdfinfo-exe-path "-")]
               [(line-terminator) (if (eq? (system-type 'os) 'windows) 'return-linefeed 'linefeed)])
    (write-bytes input-bytes stdin)
    (close-output-port stdin)
    (do ([line (read-line stdout line-terminator) (read-line stdout line-terminator)]
         [output-strings '() (cons line output-strings)])
      ((eof-object? line) (close-input-port stdout)
                          (close-input-port stderr)
                          (reverse output-strings)))))


;;;
;;; Exceptions and structures
;;;


;;;; exception handling wrapper
;(define simple (with-handlers ([exn:fail? (lambda (v) 'io-failed)])
;                 (println "Hello.")))
;
;
;;;; make a struct with getter handlers
;(struct pdf-meta (author paper-size orientation creation-date software pdf-version encryption))
;
;(define my-pdf (pdf-meta "Adam Jaworski" "A3" "landscape" "03-04-2020" "Word" 1.4 'no))
;
;;;; now access the fields
;(println (pdf-meta-author my-pdf))
;(println (pdf-meta-paper-size my-pdf))

;;;
;;; Image transformation
;;;

(define (png-to-bitmap bstr)
  (let-values ([(in out) (make-pipe)])
    (write-bytes bstr out)
    (let ([image (read-bitmap in 'png)])
      (close-output-port out)
      (close-input-port in)
      image)))


;;;
;;; High level operations
;;;

(define (see-pdf filename)
  (print (png-to-bitmap (pdf-to-png-thumbnail 500 (read-bytes-from-file filename)))))

(define (see-pdfs filenames)
  (map see-pdf filenames))

(define (convert-pdf-to-png filename)
  (write-bytes-to-file (string-append filename ".png") (pdf-to-png 300 (read-bytes-from-file filename))))

(define (info-pdf filename)
  (pdf-to-info (read-bytes-from-file filename)))


(define (info-pdf-1 filename)
  (let-values ([(subproc stdout stdin stderr) (subprocess #f #f #f pdfinfo-exe-path filename)]
               [(line-terminator) (if (eq? (system-type 'os) 'windows) 'return-linefeed 'linefeed)])
    (close-output-port stdin)
    (do ([line (read-line stdout line-terminator) (read-line stdout line-terminator)]
         [output-strings '() (cons line output-strings)])
      ((eof-object? line) (close-input-port stdout)
                          (close-input-port stderr)
                          (reverse output-strings)))))

;;;
;;; File handling and hashing
;;;

;;; folder picker
(require racket/gui/base)

(define (pick-folder)
  (path->string (get-directory "Choose a root folder for the search space.")))



;;; generate sha-256 hash
(define (sha-256-as-string bstr)
  (bytes->hex-string (sha256-bytes bstr)))

;;; read/write file in/out of memory as bytes
;; need to catch file reading exceptions!
(define (read-bytes-from-file filename)
  (call-with-input-file filename port->bytes #:mode 'binary))

;;; this needs code to check if the file is already open/blocking
;;; in which case pause and try a couple more times before giving up and throwing an exception.
(define (write-bytes-to-file filename bytes)
  (call-with-output-file filename (lambda (port) (write-bytes bytes port)) #:mode 'binary #:exists 'replace))


;;; make a random file name
;;; using base64 encoded uuid
(define (random-name)
  (string-trim (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 (uuid-string)))) #px"[\\s]+"))


;;; directory path to file list resolution


;; ** doesn't work on windows
;(define (get-list-of-pdf-files d)
;  (glob (build-path d "**" "*.pdf")))
;
;; crashes with deep search
;(define (get-list-of-pdf-files d)
;  (for/list ([f (in-directory d)] #:when (regexp-match? "\\.pdf$" f))
;    f))
; why do these crash?
; (for ([f (in-directory "C:\\adam")] #:when (regexp-match? "\\.pdf$" f)) (display f))
; (for ([f (in-directory "C:\\adam\\personal")] #:when (regexp-match? "\\.pdf$" f)) (display (string-append (path->string f) "\n")))
; (for/list ([f (in-directory "C:\\adam\\personal")] #:when (regexp-match? #rx"\\.pdf$" (path->string f))) f)
; (for/list ([f (in-directory "C:\\adam\\personal")] #:when (regexp-match? #rx"\\.pdf$" (path->string f))) f)
; (for ([f (in-directory "C:\\adam\\personal")] #:when (regexp-match? #rx"\\.pdf$" (path->string f))) (displayln (path->string f))
; (for ([f (in-directory "C:\\adam\\personal")] #:when (regexp-match? #rx"\\.pdf$" (path->string f))) (displayln (path->string f))
; (for ([f (in-directory "C:\\adam\\personal")] #:when (regexp-match? #rx"\\.pdf$" (path->string f))) (displayln (path->string f))
; (for ([f (in-directory "C:\\adam\\personal")]) (cond [(regexp-match? #rx"\\.pdf$" (path->string f)) (displayln (path->string f))] [else (display ".")]))
; (for ([f (in-directory "C:\\adam\\personal")]) (cond [(regexp-match? #rx"\\.pdf$" (path->string f)) (begin (display "\n") (display (path->string f)))] [else (display ".")]))
; (with-output-to-file "results.txt" (lambda () (for ([f (in-directory "C:\\adam\\personal")]) (cond [(regexp-match? #rx"\\.pdf$" (path->string f)) (begin (display "\n") (displayln (path->string f)))] [else (display ".")]))) #:exists 'replace)


;; get list of pdf file candidates from source directory
;; this one works, accelerated by using shell commands
(define (get-file-list-metadata d)
  (let*-values ([(command) (if (eq? (system-type 'os) 'windows)
                               ; output strings are relatively robust, could break if filenames contain \n characters
                               (string-append "powershell.exe "
                                              "-noprofile "
                                              "-command \""
                                              "gci "
                                              "\\\"" (path->string (simplify-path d)) "\\\" "
                                              "-Include *.pdf "
                                              "-Recurse "
                                              "| "
                                              "% { $_.FullName, $_.length, (Get-Date -UFormat \\\"%Y-%m-%d %T\\\" $_.LastWriteTime) }"
                                              "\"")
                               (string-append "find "
                                              "-L "
                                              "\"" (path->string (simplify-path d)) "\" "
                                              "-iname \"*.pdf\" "
                                              "-ignore_readdir_race "
                                              "-printf \"%p\n%s\n%TY-%Tm-%Td %TH:%TM:%TS %TZ\n\""))]
                [(stdout stdin id stderr ctrl) (apply values (process command))]
                [(line-terminator) (if (eq? (system-type 'os) 'windows) 'return-linefeed 'linefeed)])
    (do ([line (read-line stdout line-terminator) (read-line stdout line-terminator)]
         [output-strings '() (cons line output-strings)])
      ((eof-object? line) (close-input-port stdout)
                          (close-input-port stderr)
                          (reverse output-strings))
      (displayln line))))

; probably need to change this so that it is a for loop that fires events rather than builds a list
; to avoid long blocking.
(define (get-file-list-metadata-portable d)
  (for/list ([f (in-directory d)] #:when (regexp-match? #rx"\\.(?i:pdf)$" (path->string f)))
    (list (path->string f) (file-size f) (file-or-directory-modify-seconds f))))
  
; (find-files (lambda (x) (regexp-match? #rx"\\.(?i:pdf)$" x)) (string->path "../test"))                                  

(define (walker d)
  (with-output-to-file "results.txt"
    (lambda ()
      (get-file-list-metadata-portable d)) #:exists 'replace))

(define (external-walker d)
  (with-output-to-file "results.txt"
    (lambda ()
      (get-file-list-metadata d)) #:exists 'replace))

;(walker "\\\\?\\J:\\200000\\229000\\229222-00 Macallan Distillery")
;(external-walker "J:\\200000\\229000\\229222-00 Macallan Distillery")


(define deep-path "\\\\?\\J:\\200000\\229000\\229222-00 Macallan Distillery\\4 Internal Data\\04 Calculations\\05 Mechanical\\05 Loads, Energy Source\\SS\\IES Model For Review\\IES Modelling Folder Structure\\2_Evidence\\2_As_Designed\\3_Systems\\150603 TENDER T2 issue - Specs & Non Issued Drawings")
(define shallow-path "\\\\?\\J:\\200000\\229000\\229222-00 Macallan Distillery\\4 Internal Data\\04 Calculations\\05 Mechanical\\05 Loads, Energy Source\\SS\\IES Model For Review\\IES Modelling Folder Structure")



;(define (get-file-list-metadata-names fm-list)
;  (for [(xxxxxxxxxxxxxxxx '() fm-list (first list)

    
; (close-output-port stdin)
; (close-input-port stderr)   
; (port->lines stdout #:line-mode line-terminator)))

;(define proc1 (process "dir c:\\adam\\code\\*.pdf /s /b"))
;(define-values (stdout stdin id stderr info-proc) (process "dir c:\\adam\\code\\*.pdf /s /b"))
; (define-values (subproc stdout stdin stderr) (subprocess #f #f #f "dir" "c:\\adam\\code\\*.pdf" "/s" "/b"))
;                                   (string-append "dir \"" (path->string (build-path (simplify-path d) "*.pdf\"")) " /s /b")
;                                   (string-append "find -L " (path->string (simplify-path d)) " -iname *.pdf"))]
; gci .. -Include *.pdf -Recurse -FollowSymlink | % { $_.FullName, $_.length, (Get-Date -UFormat "%Y-%m-%d %T" $_.LastWriteTime) }
; find -L ~ -iname "*.pdf" -ignore_readdir_race -printf "%p\n%s\n%TY-%Tm-%Td %TH:%TM:%TS %TZ\n"

;;; Conditional filter to be added
;;; Multi-thread read and processing of pdf files
;;; Exception handling of bad IO, file names etc.
;;; Exception handling of bad pdf processing.
;;; UI development with stick selector, drawing selector, viewing pane and message pane
