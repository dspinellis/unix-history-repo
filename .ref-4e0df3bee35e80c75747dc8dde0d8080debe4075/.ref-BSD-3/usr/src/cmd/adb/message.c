#
/*
 *
 *	UNIX debugger
 *
 */



#include	"mac.h"
SCCSID(@(#)message.c	2.2);
#include	"mode.h"

MSG		VERSION =  "\nVERSION vax2.1	DATE 780917\n";

MSG		BADMOD	=  "bad modifier";
MSG		BADCOM	=  "bad command";
MSG		BADSYM	=  "symbol not found";
MSG		BADLOC	=  "automatic variable not found";
MSG		NOCFN	=  "c routine not found";
MSG		NOMATCH	=  "cannot locate value";
MSG		NOBKPT	=  "no breakpoint set";
MSG		BADKET	=  "unexpected ')'";
MSG		NOADR	=  "address expected";
MSG		NOPCS	=  "no process";
MSG		BADVAR	=  "bad variable";
MSG		BADTXT	=  "text address not found";
MSG		BADDAT	=  "data address not found";
MSG		ODDADR	=  "odd address";
MSG		EXBKPT	=  "too many breakpoints";
MSG		A68BAD	=  "bad a68 frame";
MSG		A68LNK	=  "bad a68 link";
MSG		ADWRAP	=  "address wrap around";
MSG		BADEQ	=  "unexpected `='";
MSG		BADWAIT	=  "wait error: process disappeared!";
MSG		ENDPCS	=  "process terminated";
MSG		NOFORK	=  "try again";
MSG		BADSYN	=  "syntax error";
MSG		NOEOR	=  "newline expected";
MSG		SZBKPT	=  "bkpt: command too long";
MSG		BADFIL	=  "bad file format";
MSG		BADNAM	=  "not enough space for symbols";
MSG		LONGFIL	=  "filename too long";
MSG		NOTOPEN	=  "cannot open";
MSG		BADMAG	=  "bad core magic number";
