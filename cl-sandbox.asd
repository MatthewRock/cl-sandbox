;; Copyright (c) 2015-2017 Mateusz Malisz


;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:


;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.


;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


(asdf:defsystem #:cl-sandbox
  :description "Utility package for creating safe experimental environment."
  :author "Mateusz Malisz <maliszmat@gmail.com>"
  :license "MIT"
  :version "0.0.4"
  :serial t
  :components ((:module
                "src"
                :components
                ((:file "package")
                 (:file "sandbox"))))
  :in-order-to ((asdf:test-op
                 (asdf:test-op #:cl-sandbox/tests))))

(asdf:defsystem #:cl-sandbox/tests
  :depends-on (#:cl-sandbox #:fiveam)
  :author "Mateusz Malisz <maliszmat@gmail.com>"
  :license "MIT"
  :components ((:module
                "t"
                :components
                ((:file "sandbox-tests"))))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :cl-sandbox/tests :run-tests)))
