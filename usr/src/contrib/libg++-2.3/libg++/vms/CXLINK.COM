$ v = 'f$verify(0)
$! THIS COMMAND FILE IS USED TO LINK C++ PROGRAMS TO THE libg++ library and
$! the C RUN TIME LIBRARY.  The shared version of the library is used for
$! the C run time library ONLY.
$ link 'P1'+gnu_cc:[000000]OPTIONS.OPT/OPT
$if v then set verify
