$! THIS COMMAND FILE IS USED TO LINK C++ PROGRAMS TO THE libg++ library and
$! the C RUN TIME LIBRARY.  The shared version of the library is used for
$! both libraries.  This is the most efficient use of disk space.
$ link 'P1'+GNU_CC:[000000]OPTIONS_SHR.OPT/OPT
