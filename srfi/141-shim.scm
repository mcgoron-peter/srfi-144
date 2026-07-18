;;; Copyright (c) 2010--2011 Taylor R. Campbell
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(define (divisible? n d)
  ;; This operation admits a faster implementation than the one given
  ;; here.
  (zero? (remainder n d)))

(define (round/ n d)
  (define (divide n d adjust leave)
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (and (not (zero? r))
               (or (and (odd? q) (even? d) (divisible? n (quotient d 2)))
                   (< d (* 2 r))))
          (adjust (+ q 1) (- r d))
          (leave q r))))
  (if (and (exact-integer? n) (exact-integer? d))
      (cond ((and (negative? n) (negative? d))
             (divide (- 0 n) (- 0 d)
               (lambda (q r) (values q (- 0 r)))
               (lambda (q r) (values q (- 0 r)))))
            ((negative? n)
             (divide (- 0 n) d
               (lambda (q r) (values (- 0 q) (- 0 r)))
               (lambda (q r) (values (- 0 q) (- 0 r)))))
            ((negative? d)
             (divide n (- 0 d)
               (lambda (q r) (values (- 0 q) r))
               (lambda (q r) (values (- 0 q) r))))
            (else
             (let ((return (lambda (q r) (values q r))))
               (divide n d return return))))
      (let ((q (round (/ n d))))
        (values q (- n (* d q))))))
