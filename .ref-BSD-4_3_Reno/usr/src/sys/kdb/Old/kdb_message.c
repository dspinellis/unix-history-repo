/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_message.c	7.3 (Berkeley) 12/15/86
 */

#include "../kdb/redef.h"

char	*BADCOM	=  "bad command";
char	*BADSYM	=  "symbol not found";
char	*BADLOC	=  "automatic variable not found";
char	*NOCFN	=  "c routine not found";
char	*NOMATCH =  "cannot locate value";
char	*BADKET	=  "unexpected ')'";
char	*NOADR	=  "address expected";
char	*BADVAR	=  "bad variable";
char	*ADWRAP	=  "address wrap around";
char	*BADEQ	=  "unexpected `='";
char	*BADSYN	=  "syntax error";
char	*NOEOR	=  "newline expected";
char	*NOBKPT	=  "no breakpoint set";
char	*SZBKPT	=  "bkpt command too long";
char	*EXBKPT	=  "too many breakpoints";
char	*BADMOD	=  "bad modifier";
char	*BADRAD =  "invalid radix";
