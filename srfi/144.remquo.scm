;;; Copyright (C) 2026 Peter McGoron, contributed under the MIT license

(define (flremquo x y)
  (check-flonum! 'flremquo x)
  (check-flonum! 'flremquo y)
  (let ((ax (flabs x))
        (ay (flabs y)))
    (cond
      ((or (flzero? y) (not (flfinite? x)) (flnan? y))
       ;; Quotient is unspecified.
       (values (fl/ (fl* x y) (fl* x y)) 0))
      ((fl=? ax ay)
       ;; Simple case 1.
       (values (flcopysign 0.0 ax)
               (exact (fl* (flsgn ax) (flsgn ay)))))
      ((fl<? ax ay)
       ;; Simple case 2.
       ;; If ax/ay is less than 0.5, then return x as the remainder, and
       ;; 0 as the quotient. Otherwise, round up.
       (let ((div (fl/ ax ay)))
         (if (fl<=? div 0.5)
             (values x 0)
             (let ((q (flround div)))
               ;; These operations are exact, as `div` here can only
               ;; become 1.0 or -1.0, whose value is calculated exactly.
               (values (fl- x (fl* q y)) (exact q))))))
      (else (complicated-remquo x y)))))

(define (complicated-remquo x y)
  ;; Two radix-2 floating point numbers are represented as
  ;; 
  ;; x = 1.x_1x_2 ... × 2^e_x
  ;; y = 1.y_1y_2 ... × 2^e_y
  ;; 
  ;; To calculate x/y, we split the numbers into fractional and
  ;; exponential parts. (Slightly different than the representation
  ;; above.)
  (define-values (fx ex) (flnormalized-fraction-exponent x))
  (define-values (fy ey) (flnormalized-fraction-exponent y))
  ;; x = fx*2^{e_x}
  ;; y = fy*2^{e_y}
  ;; 
  ;; x/y = (fx/fy)*2^{e_x - e_y}
  ;; 
  ;; fx = 0.5 + fx_1/4 + fx_2/2^3 ...
  ;; fx = 0.5 + fy_1/4 + fy_2/2^3 ...
  ;; If we multiply this by the mantissa width (53 for doubles), then
  ;; we get (floating point) integers. We can then cast these to exact
  ;; integers.
  (define ix (exact (make-flonum fx precision-bits)))
  (define iy (exact (make-flonum fy precision-bits)))
  ;; Now we have
  ;; x/y = (ix/iy)*2^{e_x - e_y}
  ;; The mantissa exponent is cancelled out (we will need to add it to
  ;; the remainder later). Now we will do the actual integer division.
  ;; We can't do round/ here, because the rounding of the division here
  ;; might not reflect the final rounding. (Example: 5.75/2.0. You can
  ;; work it out on a piece of paper in a base-10 floating-point system.)
  ;; Hence we just use *an* integer division operator here, and truncate
  ;; is symmetric around the origin (like round is).
  (define-values (q r) (truncate/ ix iy))
  ;; x/y = (q + r/iy)*2^{e_x - e_y}
  ;;     = q*2^{e_x - e_y} + (r/iy)*2^{e_x - e_y}
  ;;
  ;; The exponent is still an issue. We can overflow if we multiply
  ;; by it, but more importantly, we can calculate the incorrect
  ;; remainder, because by multiplying r by 2 e_x - e_y times, we
  ;; may get `r` to be larger than `iy`. So we will have to modify
  ;; the quotient over time. Since r/iy is smaller than the quotient,
  ;; it won't cause the quotient to flip sign when we shift numbers
  ;; from the remainder to the quotient.
  ;;
  ;; The returned value has to have the appropriate sign, even
  ;; if the lower bits are all zero. `convert-sign` will return
  ;; the appropriately signed value. (It can be thought of as an
  ;; exact integer, wrapping version of copysign.)
  (define (convert-sign x sign)
    (cond
      ((and (negative? sign) (zero? x))
       -128)
      (else (* sign (abs x)))))
  (define (int-ldexp n exp)
    (if (>= exp 7)
        0
        (* n (expt 2 exp))))
  (define divsign
    (cond
      ((and (negative? r) (negative? iy)) 1)
      ((and (negative? r) (positive? iy)) -1)
      ((and (positive? r) (negative? iy)) -1)
      ((and (positive? r) (positive? iy)) 1)))
  (define (wrap x) (abs (remainder x 128)))
  ;; After the calculations below, the equation becomes (where r' < iy)
  ;;
  ;; x/y = q*2^{e_x - e_y} + I + r'/iy
  ;; x/y = q*2^{e_x - e_y} + I + r'*2^{e_y - 53}/iy*2^{e_y - 53}
  ;; x/y = q*2^{e_x - e_y} + I + r'*2^{e_y - 53}/y
  ;;
  ;; Using iy*2^{-53} = fx, and fx*2^{e_y} = y.
  ;;
  ;; Hence we incur one rounding, the one caused by make-flonum
  ;; (aka ldexp).
  (define q-sign (if (negative? q) -1 1))
  ;; First calculate the new quotient, only keeping 7 bits.
  (letrec ((add-power-of-two
            (lambda (q i)
              (wrap (+ q (* divsign (int-ldexp
                                     1
                                     (- ex ey i 1)))))))
;; Now calculate the remainder by incrementally multiplying it by
;; 2. When the numerator becomes larger in absolute value than the
;; denominator, then we need to shift that number, multiplied by
;; the appropriate power of 2, to the quotient.
;;
;; NOTE: Instead of fixup-remquo and return at the end, the loop
;; could keep the correct rounding at each step. This would make
;; things a little simpler.
           (loop
            (lambda (q r i)
              (if (>= i (- ex ey))
                (fixup-remquo q q-sign r ey)
                (let ((r (* r 2)))
                  (if (>= (abs r) (abs iy))
                      (loop (add-power-of-two q i)
                            (- r (* divsign iy))
                            (+ i 1))
                      (loop q r (+ i 1)))))))
           (fixup-remquo
            (lambda (q q-sign r ey)
              (let ((r*2 (* r 2)))
                (cond
                  ((or (> (abs r*2) (abs iy))
                       (and (= (abs r*2) (abs iy)) (odd? q)))
                   ;; Round up when remainder is above the halfway point,
                   ;; or at the halfway point and rounding up would make
                   ;; the quotient even.
                   (return (wrap (+ q (* divsign 1)))
                           q-sign
                           (- r (* divsign iy))
                           ey))
                  (else (return q q-sign r ey))))))
           (return (lambda (q q-sign r ey)
                     (values (make-flonum (flonum r) (- ey 53))
                             (convert-sign q q-sign)))))
    (let ((q (int-ldexp (abs q) (- ex ey))))
      (if (zero? r)
          (values (flcopysign 0.0 y) q)
          (loop q r 0)))))

(define (fltruncate-remainder x y)
  (let ((ax (flabs x))
        (ay (flabs y)))
    (cond
      ((or (flzero? y) (not (flfinite? x)) (flnan? y))
       (fl/ (fl* x y) (fl* x y)))
      ((fl=? ax ay) (flcopysign 0.0 ax))
      ((fl<? ax ay)
       ;; truncate(|x/y|) = 0, so r = x.
       ax)
      (else (fltruncate-remainder* x y)))))

(define (fltruncate-remainder* x y)
  ;; See flremquo. This code is much simpler as the rounding rule is
  ;; simpler.
  (let*-values (((fx ex) (flnormalized-fraction-exponent x))
                ((fy ey) (flnormalized-fraction-exponent y))
                ((ix) (exact (make-flonum fx precision-bits)))
                ((iy) (exact (make-flonum fy precision-bits)))
                ((r) (truncate-remainder ix iy))
                ((adjustment-sign) (cond
                                     ((and (negative? r) (positive? iy))
                                      (flonum -1.0))
                                     ((and (positive? r) (negative? iy))
                                      (flonum -1.0))
                                     ((and (positive? r) (positive? iy))
                                      (flonum 1.0))
                                     ((and (negative? r) (negative? iy))
                                      (flonum 1.0)))))
    (if (zero? r)
        (flonum r)
        (do ((n 0 (+ n 1))
             (r r (let ((r (* r 2)))
                    (if (>= (abs r) (abs iy))
                        (- r (* adjustment-sign iy))
                        r))))
            ((= n (- ex ey))
             (make-flonum (flonum r) (- ey 53)))))))

(define flremainder
  (flop2 'flremainder fltruncate-remainder))

