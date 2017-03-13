[![Build Status](https://travis-ci.org/MatthewRock/cl-sandbox.svg?branch=master)](https://travis-ci.org/MatthewRock/cl-sandbox)

Overview
--------

Sandbox is a small package, which allows you to easily create a almost identical
environment like you had in your REPL, where you can experiment and try out different
things without worrying about breaking anything in original place, or polluting
global namespace.

License
--------

Sandobx is licensed with MIT license, which means that you can do pretty much everything with it.


Compatibility
--------

I am trying to build the library on as many compilers that I can. I have encountered some problems with UIOP on CLISP; therefore, CLISP is not supported.

Example
--------

Let's say that you are working inside your own package, which uses [Drakma](http://www.weitz.de/drakma/).

``` common-lisp
CL-USER> (defpackage :my-package
           (:use :cl :drakma))
#<PACKAGE "MY-PACKAGE">
CL-USER> (in-package :my-package)
#<PACKAGE "MY-PACKAGE">
```

Now you want to test something (e.g. querying google translate) and use helper variables, but you don't want to pollute your package. Therefore, you use sandbox package(WARNING: Currently, sandbox is not inside quicklisp; therefore, you would need to add its path where either quicklisp or asdf could find it To find out how to do so, see [this post](http://lispyprogrammer.blogspot.com/2015/11/adding-your-directory-to-quicklisp.html)):

``` common-lisp
MY-PACKAGE> (ql:quickload :sandbox)
To load "sandbox":
  Load 1 ASDF system:
    sandbox
; Loading "sandbox"
(:SANDBOX)
MY-PACKAGE> (sandbox:start)
SANDBOX::SUCCESS
|sandbox-home640|>
```

This will create a unique and fresh package for sandbox. The package will use all the symbols exported from the previous package(here it would be "MY-PACKAGE"), as well as symbols exported by all packages used by your package. If you do not wish to import these packages, use keyword argument `import-used-packages`:

``` common-lisp
MY-PACKAGE> (sandbox:start :import-used-packages nil)
SANDBOX::SUCCESS
|sandbox-home640|> nil ; NIL comes from CL package
; Evaluation aborted on #<UNBOUND-VARIABLE NIL {1001C92773}>.
```

You can also use the same argument to provide your list of packages to use instead:

``` common-lisp
MY-PACKAGE> (sandbox:start :import-used-packages (list (find-package :cl)))
SANDBOX::SUCCESS
|sandbox-home640|> nil
NIL
```

Now let's go back to our example. Since our package "MY-PACKAGE" used drakma, we would like to see if we can access its external symbols:

``` common-lisp
|sandbox-home640|> *drakma-version*
"2.0.2"
```

So far so good! Now let's call the google query:

``` common-lisp
|sandbox-home640|> (defparameter *local-external-format* :utf-8)
*LOCAL-EXTERNAL-FORMAT*
|sandbox-home640|> (defparameter *local-content-types* (cons '("application" . "json")
                                                             *text-content-types*))
*LOCAL-CONTENT-TYPES*
|sandbox-home640|> (ql:quickload :cl-html-parse)
To load "cl-html-parse":
  Load 1 ASDF system:
    cl-html-parse
; Loading "cl-html-parse"
(:CL-HTML-PARSE)
|sandbox-home640|> (defun get-translation (html)
                     (cadr (sixth (third (car (cl-html-parse:parse-html html))))))
GET-TRANSLATION
|sandbox-home640|> (defun translate (query lang-to lang-from)
                     (drakma:http-request "http://translate.google.com/m"
                                          :parameters `(("hl" . ,lang-to)
                                                        ("sl" . ,lang-from)
                                                        ("q" . ,query))))
TRANSLATE
|sandbox-home640|> (get-translation (translate "Hello, world!" "pl" "en"))
"Witaj świecie!"
```

Works fine! Now, you sometimes might want to do something else and might need to change special variables:

``` common-lisp
|sandbox-home640|> (print (drakma:http-request "http://reddit.com/user/cliffwarden/about.json"))

#(123 34 107 105 110 100 34 58 32 34 116 50 34 44 32 34 100 97 116 97 34 58 32
  123 34 110 97 109 101 34 58 32 34 99 108 105 102 102 119 97 114 100 101 110
  34 44 32 34 105 115 95 102 114 105 101 110 100 34 58 32 102 97 108 115 101 44
  32 34 99 114 101 97 116 101 100 34 58 32 49 49 53 49 56 48 53 57 54 51 46 48
  44 32 34 104 105 100 101 95 102 114 111 109 95 114 111 98 111 116 115 34 58
  32 102 97 108 115 101 44 32 34 99 114 101 97 116 101 100 95 117 116 99 34 58
  32 49 49 53 49 55 55 55 49 54 51 46 48 44 32 34 108 105 110 107 95 107 97 114
  109 97 34 58 32 56 51 51 44 32 34 99 111 109 109 101 110 116 95 107 97 114
  109 97 34 58 32 53 54 48 44 32 34 105 115 95 103 111 108 100 34 58 32 102 97
  108 115 101 44 32 34 105 115 95 109 111 100 34 58 32 116 114 117 101 44 32 34
  115 117 98 114 101 100 100 105 116 34 58 32 110 117 108 108 44 32 34 104 97
  115 95 118 101 114 105 102 105 101 100 95 101 109 97 105 108 34 58 32 116 114
  117 101 44 32 34 105 100 34 58 32 34 57 52 52 48 34 125 125)
```

No good! We want text! However, since it depends on special variables, you can shadow them using LET(note that changing any variables that are external will modify them!):

``` common-lisp
|sandbox-home640|> (defparameter *local-external-format* :utf-8)
*LOCAL-EXTERNAL-FORMAT*
|sandbox-home640|> (defparameter *local-content-types* (cons '("application" . "json")
                                                             *text-content-types*))
*LOCAL-CONTENT-TYPES*
(defparameter *my-result* (let ((*drakma-default-external-format* *local-external-format*)
                          (*text-content-types* *local-content-types*))
                      (print (drakma:http-request "http://reddit.com/user/cliffwarden/about.json"))))
"{\"kind\": \"t2\", \"data\": {\"name\": \"cliffwarden\", \"is_friend\": false, \"created\": 1151805963.0, \"hide_from_robots\": false, \"created_utc\": 1151777163.0, \"link_karma\": 833, \"comment_karma\": 560, \"is_gold\": false, \"is_mod\": true, \"subreddit\": null, \"has_verified_email\": true, \"id\": \"9440\"}}"
*MY-RESULT*
```
The code above has been inspired by the blog post that you can find [here](http://thehelpfulhacker.net/2011/07/10/checking-your-reddit-karma-with-common-lisp/).

Finally, let us exit the sandbox and ensure that our environment is clean:

``` common-lisp
|sandbox-home640|> (prog1 *package* (sandbox:exit))
#<PACKAGE (deleted) {100209E5D3}>
MY-PACKAGE> *my-result*
; Evaluation aborted on #<UNBOUND-VARIABLE *MY-RESULT* {1006CBCC63}>.
```

Here we returned `*package*` to see if it is still there. As you can see, it is not. Moreover, variables defined in sandbox are also not present in the origin package(the package we came from; here, it is "MY-PACKAGE"), and we are back to the package that we started sandbox from.


Importing internal symbols
--------
If you want, you can import internal symbols to the sandbox too. Here is an example of how to do so:

``` common-lisp
CL-USER> (defpackage :my-package
           (:use :cl :drakma))
#<PACKAGE "MY-PACKAGE">
CL-USER> (in-package :my-package)
#<PACKAGE "MY-PACKAGE">
MY-PACKAGE> (defparameter *five* 5)
*FIVE*
MY-PACKAGE> *five*
5
MY-PACKAGE> (sandbox:start :import-internal-symbols t)
SANDBOX::SUCCESS
|sandbox-home594|> *five*
5
MY-PACKAGE> *five*
5
```

Testing
--------

You can test the package the following way:

``` common-lisp
CL-USER> (ql:quickload :cl-sandbox)

To load "cl-sandbox":
  Load 1 ASDF system:
    cl-sandbox
; Loading "cl-sandbox"

(:CL-SANDBOX)
CL-USER> (asdf:test-system 'cl-sandbox)
; compiling file "/home/malice/Programming/Lisp/cl-sandbox/t/sandbox-tests.lisp" (written 22 FEB 2017 04:24:42 AM):
; compiling (DEFPACKAGE #:CL-SANDBOX/TESTS ...)
; compiling (IN-PACKAGE #:CL-SANDBOX/TESTS)
; compiling (DEFPARAMETER %%TESTING-VALUE%% ...)
; compiling (DEFUN RUN-TESTS ...)
; compiling (DEF-SUITE CL-SANDBOX-SUITE ...)
; compiling (IN-SUITE CL-SANDBOX-SUITE)
; compiling (TEST IN-AND-OUT ...)
; compiling (TEST OUTER-VARS-AVAILABLE? ...)
; compiling (TEST TEST-PAUSING ...)
; compiling (TEST PAUSING-CORNER-CASES1 ...)
; compiling (TEST PAUSING-CORNER-CASES2 ...)

; /home/malice/.cache/common-lisp/sbcl-1.3.14-linux-x64/home/malice/Programming/Lisp/cl-sandbox/t/sandbox-tests-TMP.fasl written
; compilation finished in 0:00:00.002

Running test suite CL-SANDBOX-SUITE
 Running test IN-AND-OUT ....
 Running test OUTER-VARS-AVAILABLE? .....
 Running test TEST-PAUSING .........
 Running test PAUSING-CORNER-CASES1 ......
 Running test PAUSING-CORNER-CASES2 .....
 Did 29 checks.
    Pass: 29 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
```

If tests failed on your machine and it looks like my fault, please, contact me or file an issue.

Contact
--------

Feel free to contact me by my e-mail (which can be found in sandbox.asd), or by creating an issue.
