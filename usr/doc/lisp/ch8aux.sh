lisp
(cfasl 'ch8auxc.o '_cfoo 'cfoo "integer-function")
(cfasl 'ch8auxp.o '_pfoo 'pfoo "integer-function" "-lpc")
(getaddress '_cmemq 'cmemq "function" '_pmemq 'pmemq "function")
(cmemq 'a '(b c a d e f))
(pmemq 'e '(a d f g a x))
