(define-library (srfi 144)
  (import (scheme base)
          (only (mit legacy runtime)
                flo:rounding-mode flo:set-rounding-mode!
                flo:flonum flo:flonum?
                flo:adjacent flo:ldexp
                flo:safe= flo:safe<
                flo:safe<= flo:safe> flo:safe>=
                flo:unordered? flo:zero? flo:positive? flo:negative?
                flo:normal? flo:subnormal?
                flo:finite? flo:infinite?
                flo:nan?
                flo:fast-fma?
                flo:+ flo:- flo:* flo:/ flo:+*
                flo:abs flo:copysign
                flo:floor flo:ceiling flo:round flo:truncate
                flo:exp flo:exp2 flo:expm1
                flo:log flo:logp1
                flo:log2 flo:log10
                flo:sin flo:cos flo:tan
                flo:asin flo:acos flo:atan flo:atan2
                flo:sinh flo:cosh flo:tanh
                flo:asinh flo:acosh flo:atanh
                flo:sqrt flo:cbrt
                flo:hypot
                flo:largest-positive-normal
                flo:smallest-positive-subnormal
                flo:error-bound
                flo:logb flo:sign-negative?
                flo:max flo:min))
  (export 
          (rename flo:largest-positive-normal flgreatest)
          (rename flo:smallest-positive-subnormal flleast)
          (rename flo:error-bound flepsilon)
          fl-fast-fl+*
          fl-integer-exponent-zero
          fl-integer-exponent-nan)
  (begin
    (define fl-fast-fl+* (flo:fast-fma?))
    (define fl-integer-exponent-zero
      (- (- (expt 2 24)) 1))   ;; very small exponent
    (define fl-integer-exponent-nan
      fl-integer-exponent-zero))

  (export
          (rename flo:flonum flonum)
          (rename flo:nextafter fladjacent)
          (rename flo:ldexp make-flonum)
          (rename flo:copysign flcopysign))

  (export flinteger-fraction
          flexponent
          flinteger-exponent
          flnormalized-fraction-exponent
          flsign-bit)
  (begin
    (define (flinteger-fraction fl)
      ;; fl = (1 + m)*2^e
      (let ((i (flo:truncate fl)))
        ;; if e < 0, then i = 0, hence the fractional part is just the
        ;; flonum itself.
        ;;
        ;; if e >= 0, then truncate will zero out the parts of the
        ;; mantissa that are below the integer. Then the subtraction will
        ;; remove the upper parts of the mantissa and the 1, leaving
        ;; (after normalization) the fractional part.
        ;;
        ;; Hence this is errorless.
        (values i (flo:- flo i))))
    (define (flinteger-exponent fl)
      (cond
        ((flo:nan? fl) fl-integer-exponent-nan)
        ((flo:zero? fl) fl-integer-exponent-zero)
        ((flo:infinite? fl) (expt 2 24))  ; big fixnum
        (else (flo:logb fl))))
    (define (flexponent fl)
      (cond
        ((flo:nan? fl) fl)
        ((flo:zero? fl) (flo:flonum -inf.0))
        ((flo:infinite? fl) fl)
        (else (flo:flonum (flo:logb fl)))))
    (define (flnormalized-fraction-exponent fl)
      (cond
        ((flo:zero? fl) (values fl 0))
        ((flo:infinite? fl) (values fl 0)) ; unspecified.
        ((flo:nan? fl) (values fl 0))      ; unspecified
        (else
         ;; fl = (1+m)*2^e, 1 <= 1+m < 2
         (let* ((e (flinteger-exponent fl))
                ;; (flexponent fl) = floor(log_2((1 + m)*2^e))
                ;;                 = floor(e + log_2(1 + m))
                ;; because log_2(1+m) < 1,
                ;; (flexponent fl) = e
                (returned-e (+ e 1))        ; abbreviated R
                (frac (make-flonum fl (- returned-e)))
                ;; frac = fl*2^R
                ;;      = fl*2^(-e - 1)
                ;;      = (1+m)*2^e*2^(-e - 1)
                ;;      = (1+m)/2
                ;; Hence frac ∈ [0.5, 1) and
                ;; frac*2^R = (1+m)*2^e = fl
           (values frac returned-e)))))
    (define (flsign-bit fl)
      (if (flo:sign-negative? fl)
          1
          0)))

  (export
        (rename flo:flonum? flonum?)
        fl=? fl<? fl>? fl<=? fl>=?
        (rename flo:unordered? flunordered?)
        flinteger?
        (rename flo:zero? flzero?)
        (rename flo:positive? flpositive?)
        (rename flo:negative? flnegative?)
        flodd? fleven?
        (rename flo:finite? flfinite?)
        (rename flo:infinite? flinfinite?)
        (rename flo:nan? flnan?)
        (rename flo:normal? flnormalized?)
        (rename flo:subnormal? fldenormalized?))

  (begin
    (define-syntax define-nary-predicate
      (syntax-rules ()
        ((_ name ~?)
         (define name
           (case-lambda
             ((x y) (~? x y))
             ((x y . rest)
              (let loop ((x x) (y y) (rest rest))
                (and (~? x y)
                     (or (null? rest)
                         (loop y (car rest) (cdr rest)))))))))))
    (define-nary-predicate fl=? flo:=)
    (define-nary-predicate fl<? flo:<)
    (define-nary-predicate fl>? flo:>)
    (define-nary-predicate fl<=? flo:<=)
    (define-nary-predicate fl>=? flo:>=)
    (define (flinteger? x)
      (flo:= x (flo:round x)))
    (define (fleven? x)
      (unless (flinteger? x)
        (error "not an integer" x))
      (flinteger? (flo:/ x 2)))
    (define (flodd? x)
      (not (fleven? x))))

  (export flmax flmin
          fl+ fl*
          (rename flo:+* fl+*)
          fl- fl/ 
          (rename flo:abs flabs)
          flabsdiff
          flposdiff
          flsgn
          flnumerator
          fldenominator
        (rename flo:floor flfloor)
        (rename flo:ceiling flceiling)
        (rename flo:round flround)
        (rename flo:truncate fltruncate))

  (begin
    (define-syntax define-nary-prop
      (syntax-rules ()
        ((_ name prop default)
         (let-syntax ((on-single (syntax-rules () ((_ x) x))))
           (define-nary-prop name prop default on-single)))
        ((_ name prop default on-single)
         (define name
           (case-lambda
             (() default)
             ((x) (on-single x))
             ((x y) (prop x y))
             ((x y . rest)
              (do ((candidate x (prop candidate y))
                   (y y (car rest))
                   (rest rest (cdr rest)))
                  ((null? rest) (prop candidate y)))))))))

    (define-nary-prop flmax flo:max-num -inf.0)
    (define-nary-prop flmin flo:min-num +inf.0)

    (define fl+
      (case-lambda
        (() 0.0)
        ((x) x)
        ((x y) (flo:+ x y))
        (input
         ;; Neumaier summation
         (let loop ((sum 0.0) (c 0.0) (input input))
           (if (null? input)
               (flo:+ sum c)
               (let* ((x (car input))
                      (t (flo:+ sum x))
                      (c* (if (flo:>= (flo:abs sum) (flo:abs x))
                              (flo:+ (flo:- sum t) x)
                              (flo:+ (flo:- x t) sum))))
                 (loop t (flo:+ c c*) (cdr input))))))))

    (define-nary-prop fl* flo:* 0.0)
    (define-nary-prop fl- flo:- (error "wrong number of arguments to fl-") flo:negate)
    (define-nary-prop fl/ 
                      flo:/ 
                      (error "wrong number of arguments to fl/")
                      (lambda (fl) (flo:/ 1.0 fl)))

    (define (flabsdiff x y)
      (fl:abs (flo:- x y)))
    (define (flposdiff x y)
      (if (flo:<? x y)
          +0.0
          (flo:- x y)))
    (define (flsgn x)
      (fl:copysign 1.0 x))

    (define (flnumerator-denominator x)
      (cond
        ((flo:infinite? x) x)
        ((flo:nan? x) x)
        ((flinteger? x) x)
        (else
         (let*-values
             (((i f) (flinteger-fraction x))
              ;; x = i + f, |f| > 0.
              ;;
              ;; Find the denominator by multiplying the fraction
              ;; until it becomes an integer.
              ((fracnum fracdenom)
               (do ((f f (flo:* f 2.0))
                    (d 1.0 (flo:* d 2.0)))
                   ((flinteger? f) (values f d))))
              ;; f = fracnum/fracdenom.
              ;; i = i*fracdenom/fracdenom
              ;; x = (fracnum + i*fracdenom)/fracdenom
            (values (flo:+ fracnum (flo:* i fracdenom))
                    fracdenom))))))

    (define (flnumerator x)
      (let-values (((n d) (flnumerator-denominator x)))
        n))

    (define (fldenominator x)
      (let-values (((n d) (flnumerator-denominator x)))
        d))
  )

  (export
        (rename flo:exp flexp)
        (rename flo:exp2 flexp2)
        (rename flo:expm1 flexp-1)
        flsquare
        (rename flo:sqrt flsqrt)
        (rename flo:cbrt flcbrt)
        (rename flo:hypot flhypot)
        (rename flo:expt flexpt)
        (rename flo:log fllog)
        (rename flo:logp1 fllog+1)
        (rename flo:log2 fllog2)
        (rename flo:log10 fllog10)
        make-fllog-base)

  (begin
    (define (flsquare x)
      (flo:* x x))
    (define (make-fllog-base x)
      (if (flo:<= x 1.0)
          (error "invalid base" x)
          (lambda (y)
            (flo:/ (flo:log y) (flo:log x)))))
  )

  (export
        (rename flo:sin flsin)
        (rename flo:cos flcos)
        (rename flo:tan fltan)
        (rename flo:asin flasin)
        (rename flo:acos flacos)
        flatan
        (rename flo:sinh flsinh)
        (rename flo:cosh flcosh)
        (rename flo:tanh fltanh)
        (rename flo:asinh flasinh)
        (rename flo:acosh flacosh)
        (rename flo:atanh flatanh))
  (begin
    (define flatan
      (case-lambda
        ((x) (fl:atan x))
        ((y x) (fl:atan2 y x)))))

  (export flquotient flremainder flremquo)

  (begin
    (define (flquotient x y)
      (unless (flo:flonum? x)
        (error "not a flonum" x))
      (unless (flo:flonum? y)
        (error "not a flonum" y))
      ;; This is rare instance where we want a non-standard rounding mode.
      ;; The division may return a number that was rounded up from a
      ;; non-integer into a integer. We don't want that, because then
      ;; the surrounding "truncate" won't truncate to the integer.
      ;; However, if the division truncates the extra bits, then the
      ;; proper integer part is preserved.
      ;;
      ;; Example: x = 13510798882111490.0, y = 3.0.
      ;; (integer? (/ 13510798882111490 3)) ⇒ #f
      ;; (integer? (/ 13510798882111490.0 3.0)) ⇒ #t
      ;; (< (/ 13510798882111490 3) (/ 13510798882111490.0 3.0)) ⇒ #t
      ;; (truncate (/ 13510798882111490 3)) ⇒ 4503599627370496
      ;; (truncate (/ 13510798882111490.0 3.0)) ⇒ 4503599627370497.0
      ;;
      ;; In modern CPUs, rounding modes can be attached to individual
      ;; instructions. So a smarter implementation can just emit that
      ;; instruction, without having to touch fenv.h.
      ;;
      ;; If you don't have the ability to change the rounding mode, then
      ;; this can also be calculated in a similar way to flremquo
      ;; and flremainder. However, it is more complicated. One should
      ;; benchmark to see if this or the manual version is slower.
      (let ((current-rounding-mode (flo:rounding-mode)))
        (flo:set-rounding-mode! 'toward-zero)
        (let ((value (flo:truncate (flo:/ x y))))
          (flo:set-rounding-mode! current-rounding-mode)
          value))))
  (begin
    (define (check-flonum! who x)
      (unless (flo:flonum? x)
        (error "not a flonum" who x)))
    (define (flop2 who proc)
      (lambda (x y)
        (unless (flo:flonum? x)
          (error "not a flonum" who x))
        (unless (flo:flonum? y)
          (error "not a flonum" who y))
        (proc x y))))
  (include "../../srfi/144.remquo.scm")

  (export (rename flo:gamma flgamma)
          flloggamma
          (rename flo:jn flfirst-bessel)
          (rename flo:yn flfirst-bessel)
          (rename flo:erf flerf)
          (rename flo:erfc flerfc))
  (begin
    (define (flloggamma x)
      (let-values (((m s) (flloggamma x)))
        (values m (flo:flonum s))))))

