;; Open an empty buffer, type openscad shorthand, and call (escad-current-buffer)
;; to write complete openscad syntax to ~/test.scad.
;; REQUIRES PACKAGE cl-lib !

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
   1st arg is the function, and the rest are quoted"
  (let ((cmd
	 (car (read-from-string (concat "(" line ")")))))
    (cons
     (car cmd)
     (mapcar (lambda (x) (list 'quote x))
	     (cdr cmd)))
    ))

(defun stringer (value)
  "return the appropraite stringifying function for the value"
  (if (typep value 'symbol)
      'symbol-name
    (if (typep value 'integer)
	'number-to-string
      )
    )
  )

;; Approved commands (letters) to process
(setq allowed-commands '(s c u t r n d x y p m h))

(defun escad-eval (sexp)
  "Evaluage escad language. Check flet for syntax"
  ;; technically flet is not ideal for this, but lambda-ing this balks at the rede
  ;; finition of t so whatever. Maybe I'll adapt it to cl-flet
  (flet (
	 (s (x y &optional center)
	    (concat
	     "square(["
	     (funcall (stringer x) x) ","
	     (funcall (stringer y) y) "]"
	     (if center
		 ", center=true")
	     ");"))
	 (c (rd x)
	    (concat
	     "circle("
	     (if (eq 'r rd)
		 (concat "r=" (funcall (stringer x) x))
	       (concat "d=" (funcall (stringer x) x))
	       )
	     ");"))
	 (u (w d h &optional center)
	    (concat
	     "cube(["
	     (funcall (stringer w) w) ","
	     (funcall (stringer d) d) ","
	     (funcall (stringer h) h) "]"
	     (if center
		 ", center=true")
	     ");"))
	 (t (x y z)
	    (concat
	     "translate(["
	     (funcall (stringer x) x) ","
	     (funcall (stringer y) y) ","
	     (funcall (stringer z) z) "]){"))
	 (r (x y z)
	    (concat
	     "rotate(["
	     (funcall (stringer x) x) ","
	     (funcall (stringer y) y) ","
	     (funcall (stringer z) z) "]){"))
	 (y (d h)
	    (concat
	     "cylinder("
	     "d=" (funcall (stringer d) d) ", "
	     "h=" (funcall (stringer h) h) ");"))
	 (n () "union() {")
	 (d () "difference() {")
	 (i () "intersection() {")
	 (m () "minkowski() {")
	 (h () "hull() {")
	 (re (a c)
	    (concat
	     "rotate_extrude(["
	     (funcall (stringer a) a) ","
	     (funcall (stringer c) c) "]){"))
	 (x () "};")
	 (p (points &optional paths)
	    (concat
	     "polygon(points=["
	     (enclose_points points)
	     (if paths
		 (concat
		  "],paths=[["
		  (mapconcat 'identity (mapcar (funcall 'stringer pa p)ths) ", ")
		  "]"))
	     "]);"))
	 (enclose_points(points)
			(if points
			    (concat
			     "[" (funcall (stringer (car points))(car points)) ","
			     (funcall (stringer (cadr points))(cadr points)) "],"
			     (enclose_points(cddr points)))))
	 )
  (if (member (car sexp) allowed-commands)
      (eval sexp)
    "I do not know that command.\n")))

(defun escad-print (string newline)
  "where to send the output"
  ;; presently outputting to temprory buffer defined in (escad-current-buffer)
  (insert (concat string))
  (if newline
      (insert "\n")
    ))

(defun escad-process-file (lines)
  "input is lines of instruction file as list of strings"
  (if lines
      (progn 
	(if (string= "@" (first-char (car lines)))
	    (escad-print (eval (cons 'concat (cddr (split-string (car lines) "" t)))) t)
	  (if (string= "#" (first-char (car lines)))
	      (progn
		(escad-print "#" nil)
		(escad-print (escad-eval (line-read (eval (cons 'concat (cdr (split-string (trim-string (car lines)) "" t)))))) t)
		)
	    (escad-print (escad-eval (line-read (car lines))) t)
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
    (write-file "~/test.scad")
    )))

;; (escad-process-file (read-lines-from-file "~/tmp"))

;; (setq lines (read-lines-from-file "~/tmp"))
;; (setq line (car lines))

;; (concat (apply number-to-string 9) "!")
;; (stringer 9)
;; (concat (funcall (stringer 'aa) 'aaa) "!")
