;; Open an empty buffer, type openscad shorthand, and call (escad-current-buffer)
;; to write complete openscad syntax to ~/test.scad.
;; REQUIRES PACKAGE cl-lib !
;; might need this for larger escad files: this is an UNexcessive limit (setq max-lisp-eval-depth 10000)
;; may also need variable: (setq max-specpdl-size 18000)
;; May need to rewrite this with a dolist or something later

;; Example:

;; @ $fn=20;
;; y 4 2
;; t 2 4 0
;;  r 90 0 0
;;   #c 2 2 2
;;  x
;; x

;; Generates:

;;  $fn=20;
;; cylinder(d=4, h=2);
;; translate([2,4,0]){
;; rotate([90,0,0]){
;; #cube([2,2,2]);
;; };
;; };

;; SYNTAX

;; @
;; openscad syntax passthrough 
;; Do not process as escad shorthand
;; EX: @ $fn=20;
;; EX: @ overall_height = 36;
;; Trailing space not necessary, but recommended for clarity
;; variables, once defined, may be used in place of numerical values
;; or evaluations without spaces EX: c 1 overall_height+10 2
;; 
;; c
;; cube width depth height
;; EX: c 1 2 3 = cube([1,2,3]);
;; 
;; y
;; cylinder diameter height
;; EX: y 4 2 = cylinder(d=4, h=2);
;; 
;; t
;; translate X Y Z
;; NOTE: Opens braces, must be closed with x
;; EX: t 1 2 3 = translate([2,4,0]){
;; 
;; r
;; rotate X Y Z
;; NOTE: Opens braces, must be closed with x
;; EX: r 90 180 0 = rotate([90,180,0]){
;; 
;; u
;; union
;; EX: u = union() {
;; 
;; d
;; difference
;; EX: d = difference() {
;; 
;; x
;; Close braces
;; Used to close braces previously opened by t, r, u, d
;; EX: x = };
;; 

;; BUGS:
;; [ ] if blank line with leading space, will parse error (do not know that command)

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
   White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  ) ;; Thanks Xah Lee!

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun read-lines-from-file (filePath)
  "Return a list of lines of a file at filePath as list of strings. format is ~/file.txt 
   surrounded by double quotes"
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun read-lines-from-buffer ()
  "Return a list of all lines of the current buffer as list of strings"
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))

;; (read-lines-from-file "~/tmp")

(defun line-read (line)
  "reads string as elisp code. yay homoiconicity!
   1st arg is the function, and the rest are quoted
   with lisp-specific symbols auto escaped"
  (let ((cmd
	 (car (read-from-string
	       (concat
		"("
		(if (string= (car (split-string line " ")) "p")
		    ;; don't escape paraentheses
		    (mapconcat 'identity (split-string
			line ;; for line
			"\\.") "\\\.") ;; swap . with \.
		  ;; if it's NOT a polygon, escape the parentheses
		  (mapconcat 'identity (split-string
			(mapconcat 'identity (split-string
				(mapconcat 'identity (split-string
					line ;; for line
				"\\.") "\\\.") ;; swap . with \.
			"\(") "\\\(") ;; swap ( with \(
		"\)") "\\\)") ;; swap ) with \)
		)
		")")))
	 ))
    (cons
     (car cmd)
     (mapcar (lambda (x) (list 'quote x))
	     (cdr cmd)))
    ))

(defun stringer (value)
  "return the appropraite stringifying function for the value"
  (if (symbolp value)
      'symbol-name
    (if (numberp value)
	'number-to-string)))

;; Approved commands (letters) to process
(setq allowed-commands '(s c u t r y n d i m h re x p))

(defun escad-eval (sexp)
  "Evaluate escad language. Check flet for syntax"
  ;; technically flet is not ideal for this, but lambda-ing this balks at the rede
  ;; finition of t so whatever. Maybe I'll adapt it to cl-flet
  ;; nevermind, cl-letf allows the dynamic scoping required 
  (cl-letf (
	 ((symbol-function 's)(lambda (x y &optional center)
	    (concat
	     "square(["
	     (funcall (stringer x) x) ","
	     (funcall (stringer y) y) "]"
	     (if center
		 ", center=true")
	     ");")))
	 ((symbol-function 'c)(lambda (rd x)
	    (concat
	     "circle("
	     (if (eq 'r rd)
		 (concat "r=" (funcall (stringer x) x))
	       (concat "d=" (funcall (stringer x) x))
	       )
	     ");")))
	 ((symbol-function 'u)(lambda (w d h &optional center)
	    (concat
	     "cube(["
	     (funcall (stringer w) w) ","
	     (funcall (stringer d) d) ","
	     (funcall (stringer h) h) "]"
	     (if center
		 ", center=true")
	     ");")))
	 ((symbol-function 't)(lambda (x y z)
	    (concat
	     "translate(["
	     (funcall (stringer x) x) ","
	     (funcall (stringer y) y) ","
	     (funcall (stringer z) z) "]){")))
	 ((symbol-function 'r)(lambda (x y z)
	    (concat
	     "rotate(["
	     (funcall (stringer x) x) ","
	     (funcall (stringer y) y) ","
	     (funcall (stringer z) z) "]){")))
	 ((symbol-function 'y)(lambda (d h &optional center)
	    (concat
	     "cylinder("
	     "d=" (funcall (stringer d) d) ", "
	     "h=" (funcall (stringer h) h)
	     (if center
		 ", center=true")
	     ");")))
	 ((symbol-function 'n)(lambda () "union() {"))
	 ((symbol-function 'd)(lambda () "difference() {"))
	 ((symbol-function 'i)(lambda () "intersection() {"))
	 ((symbol-function 'm)(lambda () "minkowski() {"))
	 ((symbol-function 'h)(lambda () "hull() {"))
	 ((symbol-function 're)(lambda (a c x y z)
	    (concat
	     "rotate_extrude("
	     "angle=" (funcall (stringer a) a) ","
	     "convexity=" (funcall (stringer c) c) ")"
	     "translate(["
	     (funcall (stringer x) x) ","
	     (funcall (stringer y) y) ","
	     (funcall (stringer z) z) "])"
	     )))
	 ((symbol-function 'x)(lambda  (&rest xs)
	     (if (car xs )
	         (mapconcat 'identity (cons "};" (mapcar '(lambda (a) "};") xs)) "")
	       "};" 
	       )
	     ))
	 ((symbol-function 'p)(lambda (points &optional paths)
	    (concat
	     "polygon(points=["
	     (enclose_points points)
	     (if paths
		 (concat
		  "],paths=[["
		  (mapconcat 'identity (mapcar 'number-to-string paths) ", ")
		  "]"))
	     "]);")))
	 ((symbol-function 'enclose_points)(lambda (points)
			(if points
			    (concat
			     "[" (funcall (stringer (car points))(car points)) ","
			     (funcall (stringer (cadr points))(cadr points)) "],"
			     (enclose_points(cddr points))))))
	 )
  (if (member (car sexp) allowed-commands)
      (eval sexp)
    "I do not know that command.\n")))

(defun escad-print (string newline)
  "where to send the output"
  ;; presently outputting to temprory buffer defined in (escad-current-buffer)
  (insert (concat string))
  (if newline
      (insert "\n")))

(defun escad-process-file (lines)
  "input is lines of instruction file as list of strings"
  (if lines
      (progn 
	(if (string= "@" (first-char (car lines)))
	    (escad-print (eval (cons 'concat (cddr (split-string (trim-string (car lines)) "" t)))) t)
	  (if (string= "#" (first-char (car lines)))
	      (progn
		(escad-print "#" nil)
		(escad-print (escad-eval (line-read (eval (cons 'concat (cdr (split-string (trim-string (car lines)) "" t)))))) t)
		)
	    (if (string= "%" (first-char (car lines)))
		(progn
		  (escad-print "%" nil)
		  (escad-print (escad-eval (line-read (eval (cons 'concat (cdr (split-string (trim-string (car lines)) "" t)))))) t)
		  )
	      (escad-print (escad-eval (line-read (car lines))) t)
	      )
	    )
	  )
	(escad-process-file (cdr lines))
	)
    nil))

(defun first-char (line)
  " returns first NON WHITESPACE character of a string"
  (car (split-string (trim-string line) "" t)))

(defun escad-current-buffer ()
  "Interactive function to call when your buffer is full of openscad shorthand"
  (interactive)
  (let ((current-buffer-lines (read-lines-from-buffer)))
  (with-temp-buffer
    (escad-process-file current-buffer-lines)
    (write-file "~/test.scad"))))
