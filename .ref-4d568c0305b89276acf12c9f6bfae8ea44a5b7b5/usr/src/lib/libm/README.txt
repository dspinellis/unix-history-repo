.\" @(#)README.txt	1.2 (ucb.elefunt) %G%
.\" troff -ms README.txt
.de Pi
.if n \
pi
.if t \
\\(*p
..
.ND
.ds CH
.if t \
.KS
.IP \fB...\fR
The machine-independent Version 7 math library found in 4.2BSD
is now \*Q/usr/lib/libom.a\*U.  To compile with those routines use \-lom.
.LP
.nf
K.C. Ng, March 7, 1985, with Z-S. Alex Liu, S. McDonald, P. Tang, W. Kahan.
Revised on 5/10/85, 5/13/85, 6/14/85, 8/20/85, 8/27/85, 9/11/85.
.fi
.if n \{\
.LP
.nf
*******************************************************************************
*  This is a description of the upgraded elementary functions (listed in 1).  *
*  Bessel functions (j0, j1, jn, y0, y1, yn), floor, and fabs passed over     *
*  from 4.2BSD without change except perhaps for the way floating point       *
*  exception is signaled on a VAX.  Three lines that contain "errno" in erf.c *
*  (error function erf, erfc) have been deleted to prevent overriding the     *
*  system "errno".                                                            *
*******************************************************************************
.fi \}
.if t \{\
.sp 0.5
\fB\l'6i'\fR
.QP
\fIThis is a description of the upgraded elementary functions (listed in \fB\(sc1\fP).
Bessel functions (j0, j1, jn, y0, y1, yn), floor, and fabs passed over
from 4.2BSD without change except perhaps for the way floating point
exception is signaled on a VAX.  Three lines that contain \*Qerrno\*U in erf.c
(the error functions erf, erfc) have been deleted to prevent overriding the
system \*Qerrno\*U.
.LP
.sp -0.5
\fB\l'6i'\fR
.sp 0.5 \}
.IP \fB\(sc0.\fR
Total number of files: 40
.sp 0.5
.nf
.ta +\w'IEEE/support.c'u+2n +\w'VAX/Makefile'u+2n +\w'VAX/support.s'u+2n +\w'exp__E.c'u+2n \w'log__L.c'u+2n
IEEE/Makefile	VAX/Makefile	VAX/support.s	erf.c	lgamma.c
IEEE/atan2.c	VAX/argred.s	VAX/tan.s	exp.c	log.c
IEEE/cabs.c	VAX/atan2.s	acosh.c	exp__E.c	log10.c
IEEE/cbrt.c	VAX/cabs.s	asincos.c	expm1.c	log1p.c
IEEE/support.c	VAX/cbrt.s	asinh.c	floor.c	log__L.c
IEEE/trig.c	VAX/infnan.s	atan.c	j0.c	pow.c
Makefile	VAX/sincos.s	atanh.c	j1.c	sinh.c
README	VAX/sqrt.s	cosh.c	jn.c	tanh.c
.ta
.fi
.sp 0.5
.IP \fB\(sc1.\fR
Functions implemented:
.RS
.IP (A).
Standard elementary functions (total 22):
.nf
.ta +\w'expm1(x):=exp(x)\-1'u+4n +\w'... in files'u+1n +\w'\0IEEE/atan2.c\0,'u+1n +\w'\0VAX/sincos.s\0'u+1n
acos(x)	... in file	\*Qasincos.c\*U
asin(x)	... in file	\*Qasincos.c\*U
atan(x)	... in file	\*Qatan.c\*U
atan2(x,y)	... in files	\*QIEEE/atan2.c\*U,	\*QVAX/atan2.s\*U
sin(x)	... in files	\*QIEEE/trig.c\*U,	\*QVAX/sincos.s\*U
cos(x)	... in files	\*QIEEE/trig.c\*U,	\*QVAX/sincos.s\*U
tan(x)	... in files	\*QIEEE/trig.c\*U,	\*QVAX/tan.s\*U
cabs(x,y)	... in files	\*QIEEE/cabs.c\*U,	\*QVAX/cabs.s\*U
hypot(x,y)	... in files	\*QIEEE/cabs.c\*U,	\*QVAX/cabs.s\*U
cbrt(x)	... in files	\*QIEEE/cbrt.c\*U,	\*QVAX/cbrt.s\*U
exp(x)	... in file	\*Qexp.c\*U
expm1(x):=exp(x)\-1	... in file	\*Qexpm1.c\*U
log(x)	... in file	\*Qlog.c\*U
log10(x)	... in file	\*Qlog10.c\*U
log1p(x):=log(1+x)	... in file	\*Qlog1p.c\*U
pow(x,y)	... in file	\*Qpow.c\*U
sinh(x)	... in file	\*Qsinh.c\*U
cosh(x)	... in file	\*Qcosh.c\*U
tanh(x)	... in file	\*Qtanh.c\*U
asinh(x)	... in file	\*Qasinh.c\*U
acosh(x)	... in file	\*Qacosh.c\*U
atanh(x)	... in file	\*Qatanh.c\*U
.ta
.fi
.sp 0.25
.IP (B).
Kernel functions:
.nf
.ta +\w'libm$argred 'u+2n
exp__E(x,c)	... in file \*Qexp__E.c\*U, used by expm1(), exp(), pow() and cosh()
log__L(s)	... in file \*Qlog__L.c\*U, used by log1p(), log() and pow()
libm$argred	... in file \*QVAX/argred.s\*U, used by VAX version of sin(), cos() and tan()
.ta
.fi
.if t \{\
.RE
.KE
.bp
.KS
.RS \}
.if n \
.sp 0.25
.IP (C).
System supported functions:
.nf
.ta +\w'copysign()'u+4n +\w'... in files'u+1n
sqrt()	... in files	\*QIEEE/support.c\*U, \*QVAX/sqrt.s\*U
drem()	... in files	\*QIEEE/support.c\*U, \*QVAX/support.s\*U
finite()	... in files	\*QIEEE/support.c\*U, \*QVAX/support.s\*U
logb()	... in files	\*QIEEE/support.c\*U, \*QVAX/support.s\*U
scalb()	... in files	\*QIEEE/support.c\*U, \*QVAX/support.s\*U
copysign()	... in files	\*QIEEE/support.c\*U, \*QVAX/support.s\*U
rint()	... in file	\*Qfloor.c\*U
.ta
.fi
.sp 0.25
.LP
Notes: 
.IP \fBi\fR.
The codes in files ending with \*Q.s\*U are written in VAX assembly 
language. They are intended for VAX computers.
.IP
Files that end with \*Q.c\*U are written in C. They are intended
for either a VAX or a machine that conforms to the IEEE 
standard 754 for double precision floating-point arithmetic.
.IP \fBii\fR.
On other than VAX or IEEE machines, run the original math 
library, formerly \*Q/usr/lib/libm.a\*U, now \*Q/usr/lib/libom.a\*U,
if nothing better is available.
.IP \fBiii\fR.
The trigonometric functions sin(), cos(), tan() and atan2() in files
\*QVAX/sincos.s\*U, \*QVAX/tan.s\*U and \*QVAX/atan2.s\*U are different
from those in \*QIEEE/trig.c\*U and \*QIEEE/atan2.c\*U.
The VAX assembler code uses the true value of
.Pi
to perform argument reduction, while the C code uses
the machine's value of PI rounded (see \*QIEEE/trig.c\*U).
.RE
.sp 0.5
.IP \fB\(sc2.\fR
A computer system that conforms to IEEE standard 754 should provide 
.LP
.RS
.RS
.nf
sqrt(x),
drem(x,p), (double precision remainder function)
copysign(x,y),
finite(x),
scalb(x,N),
logb(x) and
rint(x).
.fi
.RE
.LP
These functions are either required or recommended by the standard.
.LP
For convenience, a (slow) C implementation of these functions is 
provided in the file \*QIEEE/support.c\*U.
.LP
\fBWarning\fR: The functions in \*QIEEE/support.c\*U are somewhat machine
dependent.
Some modifications may be necessary to run them on a different machine.
Currently, if compiled with a suitable flag, \*QIEEE/support.c\*U will work
on a National 32000, a Zilog 8000, a VAX, and a SUN (cf. the \*QMakefile\*U in
this directory). Invoke the C compiler thus:
.RS
.nf
.ta +\w'cc \-c \-DNATIONAL IEEE/support.c'u+4n +\w'...'u+1n
cc \-c \-DVAX IEEE/support.c	...	on a VAX, D-format
cc \-c \-DNATIONAL IEEE/support.c	...	on a National 32000
cc \-c  IEEE/support.c	...	on other IEEE machines, we hope.
.ta
.fi
.RE
.LP
Notes: 
.IP \fBi\fR.
Faster versions of drem() and sqrt() for IEEE double precision
(coded in C but intended for assembly language) are given at the
end of \*QIEEE/support.c\*U but commented out since they require certain
machine-dependent functions.
.IP \fBii\fR.
A fast VAX assembler version of the system supported functions
copysign(), logb(), scalb(), finite(), and drem() appears in file 
\*QVAX/support.s\*U.  A fast VAX assembler version of sqrt() is in
file \*QVAX/sqrt.s\*U.
.RE
.if n \
.sp
.if t \{\
.RE
.KE
.bp
.KS \}
.IP \fB\(sc3.\fR
Two formats are supported by all the standard elementary functions: 
.RS
.LP
the VAX D-format (56-bit precision), and the IEEE double format
(53-bit precision).  The cbrt() in \*QIEEE/cbrt.c\*U is for IEEE machines 
only. The functions in files that end with \*Q.s\*U are for VAX computers 
only. The functions in files that end with \*Q.c\*U (except \*QIEEE/cbrt.c\*U)
are for VAX and IEEE machines. To use the VAX D-format, compile the code 
with \-DVAX; to use IEEE double format on various IEEE machines, see 
\*QMakefile\*U in this directory). 
.LP
Example:
.RS
.nf
cc \-c \-DVAX sin.c             ... for VAX D-format
.fi
.RE
.sp 0.25
.LP
\fBWarning\fR:
The values of floating-point constants used in the code are
given in both hexadecimal and decimal.  The hexadecimal values
are the intended ones. The decimal values may be used provided 
that the compiler converts from decimal to binary accurately
enough to produce the hexadecimal values shown. If the
conversion is inaccurate, then one must know the exact machine 
representation of the constants and alter the
assembly-language output from the compiler, or play tricks like
the following in a C program.
.sp 0.25
.RS
Example: to store the floating-point constant 
.sp 0.25
.RS
.if n \
p1 = 2**\-6 \(** .F83ABE67E1066A (hexadecimal)
.if t \
p1 = 2\u\s-2\-6\s+2\d \(** .F83ABE67E1066A (hexadecimal)
.RE
on a VAX in C, we use two longwords to store its 
machine value and define p1 to be the double constant 
at the location of these two longwords:
.sp 0.25
.nf
.ta +\w'static long'u+1n +\w'p1x[] ='u+1n
static long	p1x[] = {0x3abe3d78, 0x066a67e1};
#define	p1	(\(**(double\(**)p1x)
.ta
.fi
.RE
.IP Note: \w'Note:'u
\0On a VAX, some functions have two codes. For example, cabs() 
has one implementation in \*QIEEE/cabs.c\*U, and the other in
\*QVAX/cabs.s\*U. 
In this case, the assembly language version is preferred.
.RE
.sp 0.5
.IP \fB\(sc4.\fR
Accuracy. 
.RS
.LP
The errors in expm1(), log1p(), exp(), log(), cabs(), hypot()
and cbrt() are below 1 ULP (Unit in the Last Place).
.LP
The error in pow(x,y) grows with the size of y. Nevertheless,
for integers x and y, pow(x,y) returns the correct integer value 
on all tested machines (VAX, SUN, National 32000, Zilog 8000), provided that 
x to the power of y is representable exactly.
.LP
cosh(), sinh(), acosh(), asinh(), tanh(), atanh() and log10() have errors
below about 3 ULPs. 
.LP
For trigonometric and inverse trigonometric functions,
let [trig(x)] denote the value actually computed for trig(x).
.IP 1)
Those codes using PI, the machine's value of
.Pi
rounded):
.nf
(in files \*QIEEE/trig.c\*U, \*QIEEE/atan2.c\*U, \*Qasincos.c\*U and \*Qatan.c\*U.)
.fi
.IP
The errors in [sin(x)], [cos(x)], and [atan(x)] are below 
1 ULP compared with
.if n \
sin(x\(**pi/PI), cos(x\(**pi/PI), and atan(x)\(**PI/pi
.if t \
sin(x\(**\(*p/PI), cos(x\(**\(*p/PI), and atan(x)\(**PI/\(*p
respectively, where PI is the machine's
value of
.Pi
rounded. [tan(x)] returns
.if n \
tan(x\(**pi/PI)
.if t \
tan(x\(**\(*p/PI)
within about 2 ULPs; [acos(x)], [asin(x)], and [atan2(y,x)] return
.if n \
acos(x)\(**PI/pi, asin(x)\(**PI/pi, and atan2(y,x)\(**PI/pi
.if t \
acos(x)\(**PI/\(*p, asin(x)\(**PI/\(*p, and atan2(y,x)\(**PI/\(*p
respectively to similar accuracy.
.IP 2)
Those using true
.Pi
(for VAX D-format only):
.br
.nf
(in files \*QVAX/sincos.s\*U, \*QVAX/tan.s\*U, \*QVAX/atan2.s\*U, \*Qasincos.c\*U and \*Qatan.c\*U.)
.fi
.IP
The errors in [sin(x)], [cos(x)], and [atan(x)] are below
1 ULP. [tan(x)], [atan2(y,x)], [acos(x)] and [asin(x)] 
have errors below about 2 ULPs. 
.RE
.if t \{\
.KE
.bp
.KS \}
.IP
Here are the results of some test runs to find worst errors on a VAX:
.nf
.ta +\w'expm1'u+1n +\w':'u+2n +\w'2.09'u+1n +\w'ULPs'u+4n +\w'... 1,024,000'uR +1n
.sp 0.25
tan	:	2.09	ULPs	...	1,024,000	random arguments (machine PI)
sin	:	.861	ULPs	...	1,024,000	random arguments (machine PI)
cos	:	.857	ULPs	...	1,024,000	random arguments (machine PI)
.if n \
(compared with tan, sin, cos of (x\(**pi/PI))
.if t \
				(compared with tan, sin, cos of (x\(**\(*p/PI))
.sp 0.25
atan	:	0.86	ULPs	...	1,536,000	random arguments (machine PI)
asin	:	2.06	ULPs	...	200,000	random arguments (machine PI)
acos	:	2.07	ULPs	...	200,000	random arguments (machine PI)
atan2	:	1.41	ULPs	...	356,000	random arguments (machine PI)
.if n \
(compared with (PI/pi)\(**(atan, asin, acos, atan2 of x))
.if t \
				(compared with (PI/\(*p)\(**(atan, asin, acos, atan2 of x))
.sp 0.25
.if n \
tan	:	2.15	ULPs	...	1,024,000	random arguments (true pi)
.if t \
tan	:	2.15	ULPs	...	1,024,000	random arguments (true \(*p)
.if n \
sin	:	.814	ULPs	...	1,024,000	random arguments (true pi)
.if t \
sin	:	.814	ULPs	...	1,024,000	random arguments (true \(*p)
.if n \
cos	:	.792	ULPs	...	1,024,000	random arguments (true pi)
.if t \
cos	:	.792	ULPs	...	1,024,000	random arguments (true \(*p)
.if n \
acos	:	2.15	ULPs	...	1,024,000	random arguments (true pi)
.if t \
acos	:	2.15	ULPs	...	1,024,000	random arguments (true \(*p)
.if n \
asin	:	1.99	ULPs	...	1,024,000	random arguments (true pi)
.if t \
asin	:	1.99	ULPs	...	1,024,000	random arguments (true \(*p)
.if n \
atan2	:	1.48	ULPs	...	1,024,000	random arguments (true pi)
.if t \
atan2	:	1.48	ULPs	...	1,024,000	random arguments (true \(*p)
.if n \
atan	:	.850	ULPs	...	1,024,000	random arguments (true pi)
.if n \
atan	:	.850	ULPs	...	1,024,000	random arguments (true \(*p)
.sp 0.25
acosh	:	3.30	ULPs	...	512,000	random arguments
asinh	:	1.58	ULPs	...	512,000	random arguments
atanh	:	1.71	ULPs	...	512,000	random arguments  
cosh	:	1.23	ULPs	...	768,000	random arguments
sinh	:	1.93	ULPs	...	1,024,000	random arguments
tanh	:	2.22	ULPs	...	1,024,000	random arguments
log10	:	1.74	ULPs	...	1,536,000	random arguments
pow	:	1.79	ULPs	...	100,000	random arguments, 0 < x, y < 20.
.sp 0.25
exp	:	.768	ULPs	...	1,156,000	random arguments
expm1	:	.844	ULPs	...	1,166,000	random arguments
log1p	:	.846	ULPs	...	1,536,000	random arguments
log	:	.826	ULPs	...	1,536,000	random arguments
cabs	:	.959	ULPs	...	500,000	random arguments
cbrt	:	.666	ULPs	...	5,120,000	random arguments
.ta
.fi
.sp 0.5
.IP \fB\(sc5.\fR
Speed.
.IP
Some functions coded in VAX assembly language (cabs(), hypot() and sqrt())
are significantly faster than the corresponding ones in 4.2BSD.
In general, to improve performance, all functions in \*QIEEE/support.c\*U
should be written in assembly language and, whenever possible, should be
called via short subroutine calls.
.sp 0.5
.IP \fB\(sc6.\fR
j0, j1, jn.
.IP
The modifications to these routines were only in how an invalid
floating point operation is signaled on a VAX.
.sp 0.5
.if n \
.KS
.LP
.nf
\fB\(sc7.\fR  Copyright notice, and Disclaimer:
.fi
.if n \{\
.LP
.nf
***************************************************************************
*                                                                         * 
* Copyright (c) 1985 Regents of the University of California.             *
*                                                                         * 
* Use and reproduction of this software are granted  in  accordance  with *
* the terms and conditions specified in  the  Berkeley  Software  License *
* Agreement (in particular, this entails acknowledgement of the programs' *
* source, and inclusion of this notice) with the additional understanding *
* that  all  recipients  should regard themselves as participants  in  an *
* ongoing  research  project and hence should  feel  obligated  to report *
* their  experiences (good or bad) with these elementary function  codes, *
* using "sendbug 4bsd-bugs@BERKELEY", to the authors.                     *
*                                                                         *
***************************************************************************
.fi \}
.if t \{\
.sp 0.25
\fB\l'6i'\fR
.QP
\fICopyright (c) 1985 Regents of the University of California.\fR
.QP
\fIUse and reproduction of this software are granted  in  accordance  with
the terms and conditions specified in  the  Berkeley  Software  License
Agreement (in particular, this entails acknowledgement of the programs'
source, and inclusion of this notice) with the additional understanding
that  all  recipients  should regard themselves as participants  in  an
ongoing  research  project and hence should  feel  obligated  to report
their  experiences (good or bad) with these elementary function  codes,
using \*Qsendbug 4bsd-bugs@BERKELEY\*U, to the authors.\fR
.LP
.sp -0.5
\fB\l'6i'\fR \}
.KE
