;;; math-fns.el --- various mathematic functions for emacs

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Public domain.

;; $Id: math-fns.el,v 1.10 2011/01/26 17:50:57 friedman Exp $

;;; Commentary:
;;; Code:

(defun average (&rest x)
  "Return the average of all the arguments.
If a single argument is passed and it is a list, return the average of the
values in that list."
  (and (consp x)
       (null (cdr x))
       (setq x (car x)))
  (/ (apply '+ x) (length x)))

(defun next-power-of-two (x)
  "Returns the smallest power of two greater than X."
  (lsh 1 (1+ (logb x))))

;; Not sure how useful this is when emacs's integers overflow at 12!...
;; On 64-bit v22 emacs, this overflows at 35!.  v24: 36!.
(defun factorial (n)
  "Return integer product 1*2*..*N."
  (let ((i 1))
    (while (not (zerop n))
      (setq i (* i n)
            n (- n 1)))
    i))

;; Neme's approximation to Stirling's approximation.
;; Note that n! == gamma(n+1)
(defun gamma (z)
  "Compute the Gamma function of Z"
  (unless (floatp z) (setq z (float z)))
  (* (sqrt (/ (* 2 float-pi) z))
     (expt (/ (+ z (/ 1 (- (* 12 z)
                           (/ 1 (* 10 z)))))
              float-e)
           z)))

(defun greatest-common-divisor (a b)
  "Return the greatest integer that divides both A and B."
  (let ((result 0)
        min max)
    (cond ((and (zerop a) (zerop b)))
          (t
           (while (zerop result)
             (setq min (min a b))
             (setq max (max a b))
             (if (zerop (% max min))
                 (setq result min)
               (setq max (- max min))
               (setq a max)
               (setq b min)))))
    result))

(defun least-common-multiple (a b)
  "Return the smallest number which has both A and B as factors."
  (* (/ (max a b)
        (greatest-common-divisor a b))
     (min a b)))

;; x & (x-1) == 0 iff x == 2^n
;; if x == 2^n, only nth bit in x is set.
;; subtracting 1 flips all bits via a borrow; the logical AND is zero.
;; If x != 2^n, x-1 will flip all bits up to and including the first 1, but
;; will not negate the entire value and an AND will not produce zero.
(defun power-of-two-p (n)
  "Return true if N is a power of two."
  (zerop (logand n (1- n))))

(defun sumorial (n)
  "Return the sum 1+2+..+N."
  (/ (* n (1+ n)) 2))

(defun temp:ctof (c)
  "Convert degrees Celsius to degrees Fahrenheit."
  (+ (* 1.8 c) 32))

(defun temp:ftoc (f)
  "Convert degress Fahrenheit to degrees Celsius."
  (/ (- f 32) 1.8))

(defun valbits (&optional n)
  "Returns the number of binary bits required to represent n.
If n is not specified, this is effectively the number of valbits emacs uses
to represent ints---including the sign bit.

Negative values of n will always require VALBITS bits, the number of bits
emacs actually uses for its integer values, since the highest bit is used
for the sign; use (abs n) to ignore the sign."
  (or n (setq n -1))
  (let ((b 0))
    (while (not (zerop n))
      (setq n (lsh n -1))
      (setq b (1+ b)))
    b))


;;; combinatoric functions

;; Use actual factorials; will usually overflow anything without bignums
;(defun comb:choose (n k)
;  "Compute nCk."
;  (/ (factorial n)
;     (* (factorial k)
;        (factorial (- n k)))))
(defun comb:choose (n k)
  "Compute nCk."
  (let ((n (float n))
        (i 0)
        (c 1))
    (while (> k i)
      (setq c (* c (/ (- n i)
                      (- k i)))
            i (1+ i)))
    (floor c)))

;; Use actual factorials; will usually overflow anything without bignums
;(defun comb:permute (n r)
;  "Compute nPr."
;  (/ (factorial n)
;     (factorial (- n r))))
(defun comb:permute (n r)
  "Compute nPr."
  (let ((i (- n r))
        (p 1))
    (while (<= (setq i (1+ i)) n)
      (setq p (* p i)))
    p))


;;; Financial formulae

(defun APY (i &optional n)
  "Calculate annual percent yield using effective interest rate formula.

Inputs are:
  I, the nominal interest rate in decimal form (i.e. 0.05 for 5%);
  N, number of compounding periods per year.  Defaults to 12."
  (unless n (setq n 12))
  (- (expt (1+ (/ i n)) n) 1))

(defun interest-earned (principal interest-rate months &optional compounds-per-year)
  "Return the total compounded interest earned on principal.

PRINCIPAL specifies the starting principal.
INTEREST-RATE specifies the nominal interest rate.
MONTHS specifies the duration that interest accrues.
Optional COMPOUNDS-PER-YEAR specifies how often interest payments are made
  annually, and thus how frequently interest-on-interest can be accrued."
  (* principal (/ (APY interest-rate compounds-per-year) 12) months))

(provide 'math-fns)

;;; math-fns.el ends here.
