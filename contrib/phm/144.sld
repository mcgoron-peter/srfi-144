(define-library (srfi 144)
  (import (scheme base) (scheme case-lambda)
          (only (srfi 1) filter-map)
          (only (srfi 143) fx-least fx-greatest))
  (import (only (mit legacy runtime)
                with-restart
                prompt-for-evaluated-expression
                error:wrong-type-argument
                er-macro-transformer
                flo:rounding-mode flo:set-rounding-mode!
                flo:precision
                flo:negate
                flo:safe= flo:safe< flo:safe<= flo:safe> flo:safe>=
                flo:fast-fma?
                flo:+ flo:- flo:* flo:/ flo:+*
                flo:error-bound
                flo:logb flo:sign-negative?
                flo:max flo:min
                ;; Renamed imports
                flo:signed-lgamma
                flo:largest-positive-normal
                flo:smallest-positive-subnormal flo:error-bound
                flo:nextafter flo:ldexp
                flo:copysign flo:flonum? flo:unordered? flo:zero?
                flo:positive? flo:negative? flo:finite?
                flo:infinite? flo:nan? flo:normal? flo:subnormal?
                flo:*+ flo:abs flo:floor flo:ceiling flo:round
                flo:truncate flo:exp flo:exp2 flo:expm1
                flo:sqrt flo:cbrt flo:hypot flo:expt flo:log
                flo:logp1 flo:log2 flo:log10 flo:sin flo:cos
                flo:tan flo:asin flo:acos flo:sinh flo:cosh
                flo:atan flo:atan2 flo:tanh
                flo:asinh flo:acosh flo:atanh
                flo:gamma flo:jn flo:yn flo:erf flo:erfc))
  ;; XXX: Rename imports/exports should bebetter, but doesn't seem
  ;; to work well.
  (begin
    (define-syntax manual-renames
      (syntax-rules ()
        ((_ (input output) ...)
         (begin (define output input) ...))))
    (manual-renames
                  (flo:largest-positive-normal fl-greatest)
                  (flo:smallest-positive-subnormal fl-least)
                  (flo:error-bound fl-epsilon/2)
                  (flo:nextafter fladjacent)
                  (flo:ldexp make-flonum)
                  (flo:copysign flcopysign)
                  (flo:flonum? flonum?)
                  (flo:unordered? flunordered?)
                  (flo:zero? flzero?)
                  (flo:positive? flpositive?)
                  (flo:negative? flnegative?)
                  (flo:finite? flfinite?)
                  (flo:infinite? flinfinite?)
                  (flo:nan? flnan?)
                  (flo:normal? flnormalized?)
                  (flo:subnormal? fldenormalized?)
                  (flo:*+ fl+*)
                  (flo:abs flabs)
                  (flo:floor flfloor)
                  (flo:ceiling flceiling)
                  (flo:round flround)
                  (flo:truncate fltruncate)
                  (flo:exp flexp)
                  (flo:exp2 flexp2)
                  (flo:expm1 flexp-1)
                  (flo:sqrt flsqrt)
                  (flo:cbrt flcbrt)
                  (flo:hypot flhypot)
                  (flo:expt flexpt)
                  (flo:log fllog)
                  (flo:logp1 fllog1+)
                  (flo:log2 fllog2)
                  (flo:log10 fllog10)
                  (flo:sin flsin)
                  (flo:cos flcos)
                  (flo:tan fltan)
                  (flo:asin flasin)
                  (flo:acos flacos)
                  (flo:sinh flsinh)
                  (flo:cosh flcosh)
                  (flo:tanh fltanh)
                  (flo:asinh flasinh)
                  (flo:acosh flacosh)
                  (flo:atanh flatanh)
                  (flo:gamma flgamma)
                  (flo:jn flfirst-bessel)
                  (flo:yn flsecond-bessel)
                  (flo:erf flerf)
                  (flo:erfc flerfc)))

  (begin
    (define (restarter predicate? error-string type-string)
      (letrec
          ((assert
            (lambda (who x)
              (if (predicate? x)
                  x
                  (call-with-current-continuation
                   (lambda (escape)
                     (with-restart
                      'retry-flonum
                      error-string
                      (lambda (x*) (escape (assert who x*)))
                      (lambda () (prompt-for-evaluated-expression "New value"))
                      (lambda ()
                        (if (flonum? x)
                            x
                            (error:wrong-type-argument x type-string who))))))))))
        assert))
    (define assert-flonum
      (restarter flonum? "Prompt for a new flonum value."
                 "flonum")))

  (export fl-greatest fl-least fl-epsilon
          fl-fast-fl+*
          fl-integer-exponent-zero
          fl-integer-exponent-nan)
  (begin
    (define fl-epsilon (flo:* fl-epsilon/2 2.0))
    (define fl-fast-fl+* (flo:fast-fma?))
    (define fl-integer-exponent-zero
      fx-least)
    (define fl-integer-exponent-nan
      fl-integer-exponent-zero))

  (export fl-e fl-1/e fl-e-2 fl-e-pi/4 fl-log2-e fl-log10-e fl-log-2
          fl-1/log-2 fl-log-3 fl-log-pi fl-log-10 fl-1/log-10 fl-pi
          fl-1/pi fl-2pi fl-pi/2 fl-2/pi fl-pi/4 fl-2/sqrt-pi fl-sqrt-pi
          fl-pi-squared fl-degree fl-gamma-1/2 fl-gamma-1/3 fl-gamma-2/3
          fl-sqrt-2 fl-sqrt-3 fl-sqrt-5 fl-sqrt-10 fl-cbrt-2 fl-cbrt-3
          fl-4thrt-2 fl-1/sqrt-2 fl-phi fl-log-phi fl-1/log-phi fl-euler
          fl-e-euler fl-sin-1 fl-cos-1)
  (include "../../srfi/144.constants.scm")

  (export flonum fladjacent make-flonum flcopysign)

  (export flinteger-fraction
          flexponent
          flinteger-exponent
          flnormalized-fraction-exponent
          flsign-bit)
  (begin
    (define-syntax define-checked
      (er-macro-transformer
        ;; (define-checked (name . checks) body ...)
        (lambda (x r c)
          (let* ((desc (cadr x))
                 (body (cddr x))
                 (name (car desc))
                 (formals (map (lambda (form)
                                 (if (pair? form)
                                     (car form)
                                     form))
                               (cdr desc)))
                 (checks (filter-map (lambda (form)
                                       (if (pair? form)
                                           `(,(car form)
                                             (,(cadr form)
                                              (,(r 'quote) ,name)
                                              ,(car form)))
                                           #f))
                                     (cdr desc))))
           `(,(r 'define) (,name . ,formals)
             (,(r 'let) (,@checks)
               ,@body))))))
    (define flonum inexact)
    (define-checked (flinteger-fraction (fl assert-flonum))
      ;; fl = (1 + m)*2^e
      (let ((i (fltruncate fl)))
        ;; if e < 0, then i = 0, hence the fractional part is just the
        ;; flonum itself.
        ;;
        ;; if e >= 0, then truncate will zero out the parts of the
        ;; mantissa that are below the integer. Then the subtraction will
        ;; remove the upper parts of the mantissa and the 1, leaving
        ;; (after normalization) the fractional part.
        ;;
        ;; Hence this is errorless.
        (values i (flo:- fl i))))
    (define-checked (flinteger-exponent (fl assert-flonum))
      (cond
        ((flnan? fl) fl-integer-exponent-nan)
        ((flzero? fl) fl-integer-exponent-zero)
        ((flinfinite? fl) fx-greatest)
        (else (flo:logb fl))))
    (define-checked (flexponent (fl assert-flonum))
      (cond
        ((flnan? fl) fl)
        ((flzero? fl) (flonum -inf.0))
        ((flinfinite? fl) fl)
        (else (flonum (flo:logb fl)))))
    (define-checked (flnormalized-fraction-exponent (fl assert-flonum))
      (cond
        ((flzero? fl) (values fl 0))
        ((flinfinite? fl) (values fl 0)) ; unspecified.
        ((flnan? fl) (values fl 0))      ; unspecified
        (else
         ;; fl = (1+m)*2^e, 1 <= 1+m < 2
         (let* ((e (flinteger-exponent fl))
                ;; (flexponent fl) = floor(log_2((1 + m)*2^e))
                ;;                 = floor(e + log_2(1 + m))
                ;; because log_2(1+m) < 1,
                ;; (flexponent fl) = e
                (returned-e (+ e 1))        ; abbreviated R
                (frac (make-flonum fl (- returned-e))))
                ;; frac = fl*2^R
                ;;      = fl*2^(-e - 1)
                ;;      = (1+m)*2^e*2^(-e - 1)
                ;;      = (1+m)/2
                ;; Hence frac ∈ [0.5, 1) and
                ;; frac*2^R = (1+m)*2^e = fl
           (values frac returned-e)))))
    (define-checked (flsign-bit (fl assert-flonum))
      (if (flo:sign-negative? fl)
          1
          0)))

  (export flonum? flunordered?
          fl=? fl<? fl>? fl<=? fl>=?
          flinteger?
          flodd? fleven?
          flzero? flpositive? flnegative?
          flfinite?
          flinfinite?
          flnan?
          flnormalized?
          fldenormalized?)

  (begin
    (define (nary-predicate who ~?)
      (case-lambda
        ((x y)
         (~? (assert-flonum who x) (assert-flonum who y)))
        ;; MIT-Scheme doesn't like (x y . rest) for some reason
        ((x y w . rest)
         (let loop ((x (assert-flonum who x)) (y y) (rest (cons w rest)))
           (let ((y (assert-flonum who y)))
             (and (~? x y)
                  (or (null? rest)
                      (loop y (car rest) (cdr rest)))))))))
    (define fl=? (nary-predicate 'fl=? flo:safe=))
    (define fl<? (nary-predicate 'fl<? flo:safe<))
    (define fl>? (nary-predicate 'fl>? flo:safe>))
    (define fl<=? (nary-predicate 'fl<=? flo:safe<=))
    (define fl>=? (nary-predicate 'fl>=? flo:safe>=))
    (define-checked (flinteger? (fl assert-flonum))
      (flo:safe= fl (flround fl)))
    (define assert-flinteger
      (restarter flinteger? "Prompt for a new integer flonum value."
                 "integer flonum"))
    (define-checked (fleven? (x assert-flinteger))
      (flinteger? (flo:/ x 2.0)))
    (define-checked (flodd? (x assert-flinteger))
      (not (fleven? x))))

  (export flmax flmin
          fl+ fl* fl+* fl- fl/ flabs
          flabsdiff flposdiff flsgn
          flnumerator fldenominator
          flfloor flceiling flround fltruncate)

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
             ((x) (on-single (assert-flonum 'name x)))
             ((x y) (prop (assert-flonum 'name x) (assert-flonum 'name y)))
             ((x y w . rest)
              (do ((candidate (assert-flonum 'name x)
                              (prop candidate y))
                   (y (assert-flonum 'name y)
                      (assert-flonum 'name (car rest)))
                   (rest (cons w rest) (cdr rest)))
                  ((null? rest)
                   (prop candidate y)))))))))

    (define-nary-prop flmax flo:max -inf.0)
    (define-nary-prop flmin flo:min +inf.0)

    (define fl+
      (case-lambda
        (() 0.0)
        ((x) (assert-flonum 'fl+ x))
        ((x y) (flo:+ (assert-flonum 'fl+ x)
                      (assert-flonum 'fl+ y)))
        ((x y w . input)
         ;; Neumaier summation
         (letrec ((loop
                   (lambda (sum c input)
                     (if (null? input)
                         (flo:+ sum c)
                         (let ((x (assert-flonum 'fl+ (car input))))
                           (let ((t (flo:+ sum x)))
                             (if (flinfinite? t)
                                 (for-infinite t (cdr input))
                                 (let ((c* (if (flo:safe>=
                                                (flo:abs sum)
                                                (flo:abs x))
                                               (flo:+ (flo:- sum t) x)
                                               (flo:+ (flo:- x t) sum))))
                                   (loop t (flo:+ c c*) (cdr input)))))))))
                  (for-infinite
                   (lambda (sum rest)
                     (if (null? rest)
                         sum
                         (for-infinite (flo:+ sum
                                              (assert-flonum 'fl+ (car rest)))
                                       (cdr rest))))))
           (loop (assert-flonum 'fl+ x)
                 0.0
                 (cons (assert-flonum 'fl+ y)
                       (cons (assert-flonum 'fl+ w) input)))))))

    (define-nary-prop fl* flo:* 1.0)
    (define-nary-prop fl- flo:- (error "wrong number of arguments to fl-") flo:negate)
    (define-nary-prop fl/ 
                      flo:/ 
                      (error "wrong number of arguments to fl/")
                      (lambda (fl) (flo:/ 1.0 fl)))

    (define-checked (flabsdiff (x assert-flonum) (y assert-flonum))
      (flabs (flo:- x y)))
    (define-checked (flposdiff (x assert-flonum) (y assert-flonum))
      (if (flo:safe< x y)
          +0.0
          (flo:- x y)))
    (define-checked (flsgn (x assert-flonum))
      (flcopysign 1.0 x))

    (define-checked (flnumerator-denominator (x assert-flonum))
      (cond
        ((flinfinite? x) (values x 1.0))
        ((flnan? x) (values x x))
        ((flinteger? x) (values x 1.0))
        (else
         (let*-values
             (((i f) (flinteger-fraction x))
              ;; x = i + f, |f| > 0.
              ;;
              ;; Find the denominator by multiplying the fraction
              ;; until it becomes an integer.
              ;; This will be in lowest form.
              ((fracnum fracdenom)
               (do ((f f (flo:* f 2.0))
                    (d 1.0 (flo:* d 2.0)))
                   ((flinteger? f) (values f d)))))
              ;; f = fracnum/fracdenom.
              ;; i = i*fracdenom/fracdenom
              ;; x = (fracnum + i*fracdenom)/fracdenom
            (values (flo:+ fracnum (flo:* i fracdenom))
                    fracdenom)))))

    (define-checked (flnumerator (x assert-flonum))
      (let-values (((n d) (flnumerator-denominator x)))
        n))

    (define-checked (fldenominator (x assert-flonum))
      (let-values (((n d) (flnumerator-denominator x)))
        d))
  )

  (export flexp flexp2 flexp-1 flsquare flsqrt flcbrt flhypot flexpt
          fllog fllog1+ fllog2 fllog10 make-fllog-base)

  (begin
    (define-checked (flsquare (x assert-flonum))
      (flo:* x x))
    (define-checked (make-fllog-base (x assert-flonum))
      (if (flo:safe<= x 1.0)
          (error:bad-range-argument x 'make-fllog-base)
          (lambda (y)
            (flo:/ (fllog y) (fllog x)))))
  )

  (export flsin  flcos  fltan   flasin  flacos flatan flsinh
          flcosh fltanh flasinh flacosh flatanh)

  (begin
    (define flatan
      (case-lambda
        ((x) (flo:atan (assert-flonum 'flatan x)))
        ((y x) (flo:atan2 (assert-flonum 'flatan y)
                          (assert-flonum 'flatan x))))))

  (export flquotient flremainder flremquo)

  (begin
    (define-checked (flquotient (x assert-flonum) (y assert-flonum))
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
      ;; Round-towards-zero mode will not return zeroes if the exact
      ;; result is finite, and because of that we have to compute the
      ;; rounded quotient to see if it is infinite. The quantum at
      ;; the largest exponent is so large that if it rounds up to infinity
      ;; then the exact integer part is certainly greater than the
      ;; representable quotients.
      (let ((correct-rounding (flo:/ x y)))
        (if (flo:infinite? correct-rounding)
            correct-rounding
            (let ((current-rounding-mode (flo:rounding-mode)))
              (flo:set-rounding-mode! 'toward-zero)
              (let ((value (flo:truncate (flo:/ x y))))
                (flo:set-rounding-mode! current-rounding-mode)
                value))))))
  (begin
    (define check-flonum!
      (lambda (who x)
        (unless (flonum? x)
          (error:wrong-type-argument x "flonum" who))))
    (define precision-bits flo:precision)
    (define (flop2 who proc)
      (lambda (x y)
        (proc (assert-flonum who x) (assert-flonum who y)))))
  (include "../../srfi/144.remquo.scm")

  (export flgamma flloggamma flfirst-bessel flsecond-bessel flerf
          flerfc)
  (begin
    (define-checked (flloggamma (x assert-flonum))
      (let-values (((m s) (flo:signed-lgamma x)))
        (values m (flonum s))))))
