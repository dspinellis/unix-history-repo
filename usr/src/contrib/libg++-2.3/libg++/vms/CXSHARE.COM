$ v = 'f$verify(0)
$! THIS COMMAND FILE IS USED TO LINK C++ PROGRAMS TO THE libg++ library and
$! the C RUN TIME LIBRARY.  The shared version of the library is used for
$! both libraries.  This is the most efficient use of disk space.
$ link 'P1' 'p2' 'p3' 'p4' 'p5' 'p6' 'p7' 'p8' -
	+GNU_CC:[000000]OPTIONS_SHR.OPT/OPT
$if v then set verify
