(defpackage #:cl-sandbox/tests
  (:use :cl :fiveam)
  (:export run-tests))

(in-package #:cl-sandbox/tests)

(defparameter %%testing-value%% 5)

(defun run-tests ()
  (run! 'cl-sandbox-suite))

(def-suite cl-sandbox-suite
    :description "Tests for cl-sandbox package.")

(in-suite cl-sandbox-suite)

(test in-and-out
  ;; Sanity check
  (is (equal cl:*package* (find-package :cl-sandbox/tests)))
  ;; Try to get in
  (is (eql :success (sandbox:start)))
  ;; Ensure we're in
  (is (not (equal cl:*package* (find-package :cl-sandbox/tests))))
  ;; Try to get out
  (is (eql :success (sandbox:exit))))

(test outer-vars-available?
  ;; Sanity check
  (is (equal cl:*package* (find-package :cl-sandbox/tests)))
  ;; Get in
  (is (eql :success (sandbox:start)))
  ;; See visibility from origin package
  (finishes %%testing-value%%)
  ;; See visibility from imported package
  (finishes nil)
  ;; Get out
  (is (eql :success (sandbox:exit))))

(test test-pausing
  ;; Sanity check
  (is (equal cl:*package* (find-package :cl-sandbox/tests)))
  ;; Check if we can get in there
  (is (eql :success (sandbox:start)))
  (is (not (equal cl:*package* (find-package :cl-sandbox/tests))))
  ;; Pause; see if we got out
  (is (eql :success (sandbox:pause)))
  (is (equal cl:*package* (find-package :cl-sandbox/tests)))
  ;; Try to get back in
  (is (eql :success (sandbox:resume)))
  (is (not (equal cl:*package* (find-package :cl-sandbox/tests))))
  ;; Try to get out
  (is (eql :success (sandbox:exit)))
  (is (equal cl:*package* (find-package :cl-sandbox/tests))))

(test pausing-corner-cases1
  ;; Sanity check
  (is (equal cl:*package* (find-package :cl-sandbox/tests)))
  ;; Can't pause when not in sandbox
  (is (eql :fail (sandbox:pause)))
  ;; Likewise can't resume
  (is (eql :fail (sandbox:resume)))
  ;; Get in
  (is (eql :success (sandbox:start)))
  ;; Can't resume if not paused
  (is (eql :fail (sandbox:resume)))
  ;; Get out
  (is (eql :success (sandbox:exit))))

(test pausing-corner-cases2
  ;; Sanity check
  (is (equal cl:*package* (find-package :cl-sandbox/tests)))
  ;; Get in
  (is (eql :success (sandbox:start)))
  (is (eql :success (sandbox:pause)))
  ;; Can't pause when already paused
  (is (eql :fail (sandbox:pause)))
  ;; Can exit though
  (is (eql :success (sandbox:exit))))

;; TODO: Add tests for keyword arguments to sandbox:start
