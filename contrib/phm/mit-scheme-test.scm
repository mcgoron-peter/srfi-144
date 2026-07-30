(load "144.sld")
(load "../../tests/scheme/test.sld")
(load "../../tests/scheme/flonum.sld")

;;; This is the contents of flonum.sps

,(import (scheme base)
         (scheme write)
         (tests scheme flonum)
         (tests scheme test))

(display "Running tests for (scheme flonum)")
(newline)
(run-flonum-tests)
(report-test-results)
