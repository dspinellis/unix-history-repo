;; Copyright (C) 1986 Free Software Foundation, Inc.
;; Author Bill Rosenblatt

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Floating point arithmetic package.
;;
;; Floating point numbers are represented by dot-pairs (mant . exp)
;; where mant is the 24-bit signed integral mantissa and exp is the
;; base 2 exponent.
;;
;; Emacs LISP supports a 24-bit signed integer data type, which has a
;; range of -(2**23) to +(2**23)-1, or -8388608 to 8388607 decimal.
;; This gives six significant decimal digit accuracy.  Exponents can
;; be anything in the range -(2**23) to +(2**23)-1.
;;
;; User interface:
;; function f converts from integer to floating point
;; function string-to-float converts from string to floating point
;; function fint converts a floating point to integer (with truncation)
;; function float-to-string converts from floating point to string
;;                   
;; Caveats:
;; -  Exponents outside of the range of +/-100 or so will cause certain 
;;    functions (especially conversion routines) to take forever.
;; -  Very little checking is done for fixed point overflow/underflow.
;; -  No checking is done for over/underflow of the exponent
;;    (hardly necessary when exponent can be 2**23).
;; 
;;
;; Bill Rosenblatt
;; June 20, 1986
;;

;; fundamental implementation constants
(defconst exp-base 2
  "Base of exponent in this floating point representation.")

(defconst mantissa-bits 24
  "Number of significant bits in this floating point representation.")

(defconst decimal-digits 6
  "Number of decimal digits expected to be accurate.")

(defconst expt-digits 2
  "Maximum permitted digits in a scientific notation exponent.")

;; other constants
(defconst maxbit (1- mantissa-bits)
  "Number of highest bit")

(defconst mantissa-maxval (1- (ash 1 maxbit))
  "Maximum permissable value of mantissa")

(defconst mantissa-minval (ash 1 maxbit)
  "Minimum permissable value of mantissa")

(defconst floating-point-regexp
  "^[ \t]*\\(-?\\)\\([0-9]*\\)\
\\(\\.\\([0-9]*\\)\\|\\)\
\\(\\(\\([Ee]\\)\\(-?\\)\\([0-9][0-9]*\\)\\)\\|\\)[ \t]*$"
  "Regular expression to match floating point numbers.  Extract matches:
1 - minus sign
2 - integer part
4 - fractional part
8 - minus sign for power of ten
9 - power of ten
")

(defconst high-bit-mask (ash 1 maxbit)
  "Masks all bits except the high-order (sign) bit.")

(defconst second-bit-mask (ash 1 (1- maxbit))
  "Masks all bits except the highest-order magnitude bit")

;; various useful floating point constants
(setq _f0 '(0 . 1))

(setq _f1/2 '(4194304 . -23))

(setq _f1 '(4194304 . -22))

(setq _f10 '(5242880 . -19))

;; support for decimal conversion routines
(setq powers-of-10 (make-vector (1+ decimal-digits) _f1))
(aset powers-of-10 1 _f10)
(aset powers-of-10 2 '(6553600 . -16))
(aset powers-of-10 3 '(8192000 . -13))
(aset powers-of-10 4 '(5120000 . -9))
(aset powers-of-10 5 '(6400000 . -6))
(aset powers-of-10 6 '(8000000 . -3))

(setq all-decimal-digs-minval (aref powers-of-10 (1- decimal-digits))
      highest-power-of-10 (aref powers-of-10 decimal-digits))

(defun fashl (fnum)			; floating-point arithmetic shift left
  (cons (ash (car fnum) 1) (1- (cdr fnum))))

(defun fashr (fnum)			; floating point arithmetic shift right
  (cons (ash (car fnum) -1) (1+ (cdr fnum))))

(defun normalize (fnum)
  (if (> (car fnum) 0)			; make sure next-to-highest bit is set
      (while (zerop (logand (car fnum) second-bit-mask))
	(setq fnum (fashl fnum)))
    (if (< (car fnum) 0)		; make sure highest bit is set
	(while (zerop (logand (car fnum) high-bit-mask))
	  (setq fnum (fashl fnum)))
      (setq fnum _f0)))			; "standard 0"
  fnum)
      
(defun abs (n)				; integer absolute value
  (if (natnump n) n (- n)))

(defun fabs (fnum)			; re-normalize after taking abs value
  (normalize (cons (abs (car fnum)) (cdr fnum))))

(defun xor (a b)			; logical exclusive or
  (and (or a b) (not (and a b))))

(defun same-sign (a b)			; two f-p numbers have same sign?
  (not (xor (natnump (car a)) (natnump (car b)))))

(defun extract-match (str i)		; used after string-match
  (condition-case ()
      (substring str (match-beginning i) (match-end i))
    (error "")))

;; support for the multiplication function
(setq halfword-bits (/ mantissa-bits 2)	; bits in a halfword
      masklo (1- (ash 1 halfword-bits)) ; isolate the lower halfword
      maskhi (lognot masklo)		; isolate the upper halfword
      round-limit (ash 1 (/ halfword-bits 2)))

(defun hihalf (n)			; return high halfword, shifted down
  (ash (logand n maskhi) (- halfword-bits)))

(defun lohalf (n)			; return low halfword
  (logand n masklo))

;; Visible functions

;; Arithmetic functions
(defun f+ (a1 a2)
  "Returns the sum of two floating point numbers."
  (let ((f1 (fmax a1 a2))
	(f2 (fmin a1 a2)))
    (if (same-sign a1 a2)
	(setq f1 (fashr f1)		; shift right to avoid overflow
	      f2 (fashr f2)))
    (normalize
     (cons (+ (car f1) (ash (car f2) (- (cdr f2) (cdr f1))))
	   (cdr f1)))))

(defun f- (a1 &optional a2)		; unary or binary minus
  "Returns the difference of two floating point numbers."
  (if a2
      (f+ a1 (f- a2))
    (normalize (cons (- (car a1)) (cdr a1)))))

(defun f* (a1 a2)			; multiply in halfword chunks
  "Returns the product of two floating point numbers."
  (let* ((i1 (car (fabs a1)))
	 (i2 (car (fabs a2)))
	 (sign (not (same-sign a1 a2)))
	 (prodlo (+ (hihalf (* (lohalf i1) (lohalf i2)))
		    (lohalf (* (hihalf i1) (lohalf i2)))
		    (lohalf (* (lohalf i1) (hihalf i2)))))
	 (prodhi (+ (* (hihalf i1) (hihalf i2))
		    (hihalf (* (hihalf i1) (lohalf i2)))
		    (hihalf (* (lohalf i1) (hihalf i2)))
		    (hihalf prodlo))))
    (if (> (lohalf prodlo) round-limit)
	(setq prodhi (1+ prodhi)))	; round off truncated bits
    (normalize
     (cons (if sign (- prodhi) prodhi)
	   (+ (cdr (fabs a1)) (cdr (fabs a2)) mantissa-bits)))))

(defun f/ (a1 a2)			; SLOW subtract-and-shift algorithm
  "Returns the quotient of two floating point numbers."
  (if (zerop (car a2))			; if divide by 0
      (signal 'arith-error (list "attempt to divide by zero" a1 a2))
    (let ((bits (1- maxbit))
	  (quotient 0) 
	  (dividend (car (fabs a1)))
	  (divisor (car (fabs a2)))
	  (sign (not (same-sign a1 a2))))
      (while (natnump bits)
	(if (< (- dividend divisor) 0)
	    (setq quotient (ash quotient 1))
	  (setq quotient (1+ (ash quotient 1))
		dividend (- dividend divisor)))
	(setq dividend (ash dividend 1)
	      bits (1- bits)))
      (normalize
       (cons (if sign (- quotient) quotient)
	     (- (cdr (fabs a1)) (cdr (fabs a2)) (1- maxbit)))))))
  
(defun f% (a1 a2)
  "Returns the remainder of first floating point number divided by second."
  (f- a1 (f* (ftrunc (f/ a1 a2)) a2)))
	  

;; Comparison functions
(defun f= (a1 a2)
  "Returns t if two floating point numbers are equal, nil otherwise."
  (equal a1 a2))

(defun f> (a1 a2)
  "Returns t if first floating point number is greater than second,
nil otherwise."
  (cond ((and (natnump (car a1)) (< (car a2) 0)) 
	 t)				; a1 nonnegative, a2 negative
	((and (> (car a1) 0) (<= (car a2) 0))
	 t)				; a1 positive, a2 nonpositive
	((and (<= (car a1) 0) (natnump (car a2)))
	 nil)				; a1 nonpos, a2 nonneg
	((/= (cdr a1) (cdr a2))		; same signs.  exponents differ
	 (> (cdr a1) (cdr a2)))		; compare the mantissas.
	(t
	 (> (car a1) (car a2)))))	; same exponents.

(defun f>= (a1 a2)
  "Returns t if first floating point number is greater than or equal to 
second, nil otherwise."
  (or (f> a1 a2) (f= a1 a2)))

(defun f< (a1 a2)
  "Returns t if first floating point number is less than second,
nil otherwise."
  (not (f>= a1 a2)))

(defun f<= (a1 a2)
  "Returns t if first floating point number is less than or equal to
second, nil otherwise."
  (not (f> a1 a2)))

(defun f/= (a1 a2)
  "Returns t if first floating point number is not equal to second,
nil otherwise."
  (not (f= a1 a2)))

(defun fmin (a1 a2)
  "Returns the minimum of two floating point numbers."
  (if (f< a1 a2) a1 a2))

(defun fmax (a1 a2)
  "Returns the maximum of two floating point numbers."
  (if (f> a1 a2) a1 a2))
      
(defun fzerop (fnum)
  "Returns t if the floating point number is zero, nil otherwise."
  (= (car fnum) 0))

(defun floatp (fnum)
  "Returns t if the arg is a floating point number, nil otherwise."
  (and (consp fnum) (integerp (car fnum)) (integerp (cdr fnum))))

;; Conversion routines
(defun f (int)
  "Convert the integer argument to floating point, like a C cast operator."
  (normalize (cons int '0)))

(defun int-to-hex-string (int)
  "Convert the integer argument to a C-style hexadecimal string."
  (let ((shiftval -20)
	(str "0x")
	(hex-chars "0123456789ABCDEF"))
    (while (<= shiftval 0)
      (setq str (concat str (char-to-string 
			(aref hex-chars
			      (logand (lsh int shiftval) 15))))
	    shiftval (+ shiftval 4)))
    str))

(defun ftrunc (fnum)			; truncate fractional part
  "Truncate the fractional part of a floating point number."
  (cond ((natnump (cdr fnum))		; it's all integer, return number as is
	 fnum)
	((<= (cdr fnum) (- maxbit))	; it's all fractional, return 0
	 '(0 . 1))
	(t				; otherwise mask out fractional bits
	 (let ((mant (car fnum)) (exp (cdr fnum)))
	   (normalize 
	    (cons (if (natnump mant)	; if negative, use absolute value
		      (ash (ash mant exp) (- exp))
		    (- (ash (ash (- mant) exp) (- exp))))
		  exp))))))

(defun fint (fnum)			; truncate and convert to integer
  "Convert the floating point number to integer, with truncation, 
like a C cast operator."
  (let* ((tf (ftrunc fnum)) (tint (car tf)) (texp (cdr tf)))
    (cond ((>= texp mantissa-bits)	; too high, return "maxint"
	   mantissa-maxval)
	  ((<= texp (- mantissa-bits))	; too low, return "minint"
	   mantissa-minval)
	  (t				; in range
	   (ash tint texp)))))		; shift so that exponent is 0

(defun float-to-string (fnum &optional sci)
  "Convert the floating point number to a decimal string.
Optional second argument non-nil means use scientific notation."
  (let* ((value (fabs fnum)) (sign (< (car fnum) 0))
	 (power 0) (result 0) (str "") 
	 (temp 0) (pow10 _f1))

    (if (f= fnum _f0)
	"0"
      (if (f>= value _f1)			; find largest power of 10 <= value
	  (progn				; value >= 1, power is positive
	    (while (f<= (setq temp (f* pow10 highest-power-of-10)) value)
	      (setq pow10 temp
		    power (+ power decimal-digits)))
	    (while (f<= (setq temp (f* pow10 _f10)) value)
	      (setq pow10 temp
		    power (1+ power))))
	(progn				; value < 1, power is negative
	  (while (f> (setq temp (f/ pow10 highest-power-of-10)) value)
	    (setq pow10 temp
		  power (- power decimal-digits)))
	  (while (f> pow10 value)
	    (setq pow10 (f/ pow10 _f10)
		  power (1- power)))))
					  ; get value in range 100000 to 999999
      (setq value (f* (f/ value pow10) all-decimal-digs-minval)
	    result (ftrunc value))
      (if (f> (f- value result) _f1/2)	; round up if remainder > 0.5
	  (setq str (int-to-string (1+ (fint result))))
	(setq str (int-to-string (fint result))))

      (if sci				; scientific notation
	  (setq str (concat (substring str 0 1) "." (substring str 1)
			    "E" (int-to-string power)))

					  ; regular decimal string
	(cond ((>= power (1- decimal-digits))
					  ; large power, append zeroes
	       (let ((zeroes (- power decimal-digits)))
		 (while (natnump zeroes)
		   (setq str (concat str "0")
			 zeroes (1- zeroes)))))

					  ; negative power, prepend decimal
	      ((< power 0)		; point and zeroes
	       (let ((zeroes (- (- power) 2)))
		 (while (natnump zeroes)
		   (setq str (concat "0" str)
			 zeroes (1- zeroes)))
		 (setq str (concat "0." str))))

	      (t				; in range, insert decimal point
	       (setq str (concat
			  (substring str 0 (1+ power))
			  "."
			  (substring str (1+ power)))))))

      (if sign				; if negative, prepend minus sign
	  (concat "-" str)
	str))))

    
;; string to float conversion.
;; accepts scientific notation, but ignores anything after the first two
;; digits of the exponent.
(defun string-to-float (str)
  "Convert the string to a floating point number.
Accepts a decimal string in scientific notation, 
with exponent preceded by either E or e.
Only the 6 most significant digits of the integer and fractional parts
are used; only the first two digits of the exponent are used.
Negative signs preceding both the decimal number and the exponent
are recognized."

  (if (string-match floating-point-regexp str 0)
      (let (power)
	(f*
	 ; calculate the mantissa
	 (let* ((int-subst (extract-match str 2))
		(fract-subst (extract-match str 4))
		(digit-string (concat int-subst fract-subst))
		(mant-sign (equal (extract-match str 1) "-"))
		(leading-0s 0) (round-up nil))

	   ; get rid of leading 0's
	   (setq power (- (length int-subst) decimal-digits))
	   (while (and (< leading-0s (length digit-string))
		       (= (aref digit-string leading-0s) ?0))
	     (setq leading-0s (1+ leading-0s)))
	   (setq power (- power leading-0s)
		 digit-string (substring digit-string leading-0s))
	   
	   ; if more than 6 digits, round off
	   (if (> (length digit-string) decimal-digits)
	       (setq round-up (>= (aref digit-string decimal-digits) ?5)
		     digit-string (substring digit-string 0 decimal-digits))
	     (setq power (+ power (- decimal-digits (length digit-string)))))

	   ; round up and add minus sign, if necessary
	   (f (* (+ (string-to-int digit-string)
		    (if round-up 1 0))
		 (if mant-sign -1 1))))
	   
	 ; calculate the exponent (power of ten)
	 (let* ((expt-subst (extract-match str 9))
		(expt-sign (equal (extract-match str 8) "-"))
		(expt 0) (chunks 0) (tens 0) (exponent _f1)
		(func 'f*))
 
	   (setq expt (+ (* (string-to-int
			     (substring expt-subst 0
					(min expt-digits (length expt-subst))))
			    (if expt-sign -1 1))
			 power))
	   (if (< expt 0)		; if power of 10 negative
	       (setq expt (- expt)	; take abs val of exponent
		     func 'f/))		; and set up to divide, not multiply

	   (setq chunks (/ expt decimal-digits)
		 tens (% expt decimal-digits))
	   ; divide or multiply by "chunks" of 10**6
	   (while (> chunks 0)	
	     (setq exponent (funcall func exponent highest-power-of-10)
		   chunks (1- chunks)))
	   ; divide or multiply by remaining power of ten
	   (funcall func exponent (aref powers-of-10 tens)))))
		  
    _f0))				; if invalid, return 0


