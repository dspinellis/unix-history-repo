#ifndef lint
static	char sccsid[] = "@(#)message.c	4.3 %G%";
#endif
/*
 *
 *	UNIX debugger
 *
 */

#include	"mac.h"
#include	"mode.h"

MSG		VERSION =  "\nVERSION VM/VAX4.3	DATE %G%\n";

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
MSG		TOODEEP =  "$<< nesting too deep";
