;;;; sandbox.lisp
;; Copyright (c) 2015 Mateusz Malisz


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

(in-package #:sandbox)

(let ((sandbox nil)
      (previous-package cl:*package*)
      (sandbox-name "sandbox-home"))
  (defun start ()
   (cl:unless sandbox
     (cl:setf sandbox (cl:make-package sandbox-name :use (cl:cons cl:*package* (cl:package-use-list cl:*package*))))
     (cl:setf previous-package cl:*package*)
     (cl:setf cl:*package* sandbox)))
  (defun exit ()
   (cl:when sandbox
     (cl:setf cl:*package* previous-package)
     (cl:delete-package sandbox)
     (cl:setf sandbox nil)
     (cl:setf previous-package nil)
     t))
  (defun name-change (new-name)
   (cl:assert (cl:typep new-name 'string))
   (cl:setf sandbox-name new-name)))
