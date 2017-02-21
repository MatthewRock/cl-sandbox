;; Copyright (c) 2015-2016 Mateusz Malisz

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

;; tl;dr: MIT. Do whatever you want.

(in-package #:sandbox)

(let ((sandbox nil)
      (previous-package cl:*package*)
      (sandbox-name (gensym "sandbox-home")))

  (defun start (&key (import-used-packages (cl:package-use-list cl:*package*)))
    "Start Sandbox session. The Sandbox package will use all packages from
the origin package and this package. If you do not wish for this to happen, either
provide NIL as an argument to import-used-packages to not import anything except
symbols from origin package, or provide your list of packages to import-used-packages;
any packages you specify there will be used."
    (or (cl:unless sandbox ; If we don't have sandbox session already running
          (cl:setf sandbox ; Create sandbox package
                   (cl:make-package
                    sandbox-name ; And use outer package
                    :use (cl:cons cl:*package*
                                  import-used-packages)))
          (cl:setf previous-package cl:*package*) ; Remember previous package
          (cl:setf cl:*package* sandbox) ; Move into new package
          'success) ; Return success if everything went ok.
        'fail)) ; Return fail if we couldn't start sandbox.

  (defun exit ()
    "Close sandbox session, returning to the previous package
and deleting the sandbox package.."
    (or (cl:when sandbox ; If currently in sandbox
          (cl:setf cl:*package* previous-package) ; Move to previous package
          (cl:delete-package sandbox)
          (cl:setf sandbox nil)
          (cl:setf previous-package nil)
          'success)
        'fail))

  (defun pause ()
    "Pause sandbox session, leaving pacakge for future use."
    (or (cl:when (cl:and sandbox (cl:equal cl:*package* sandbox))
          (cl:setf cl:*package* previous-package)
          'success)
        'fail))

  (defun resume ()
    "Resume paused sandbox session."
    (or (cl:when (cl:and sandbox (cl:not (cl:equal cl:*package* sandbox)))
          (cl:setf cl:*package* sandbox)
          'success)
        'fail))

  (defun change-name (new-name)
    "Change name of sandbox package to NEW-NAME."
    (cl:assert (cl:typep new-name 'string))
    (cl:setf sandbox-name new-name)))
