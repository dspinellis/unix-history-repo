/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)sysexits.c	5.5 (Berkeley) 6/30/88";
#endif /* not lint */

#include <sysexits.h>

/*
 *  SYSEXITS.C -- error messages corresponding to sysexits.h
 */
char *SysExMsg[] = {
	/* 64 USAGE */		"500 Bad usage",
	/* 65 DATAERR */	"501 Data format error",
	/* 66 NOINPUT */	"550 Cannot open input",
	/* 67 NOUSER */		"550 User unknown",
	/* 68 NOHOST */		"550 Host unknown",
	/* 69 UNAVAILABLE */	"554 Service unavailable",
	/* 70 SOFTWARE */	"554 Internal error",
	/* 71 OSERR */		"451 Operating system error",
	/* 72 OSFILE */		"554 System file missing",
	/* 73 CANTCREAT */	"550 Can't create output",
	/* 74 IOERR */		"451 I/O error",
	/* 75 TEMPFAIL */	"250 Deferred",
	/* 76 PROTOCOL */	"554 Remote protocol error",
	/* 77 NOPERM */		"550 Insufficient permission",
	/* 78 CONFIG */		"554 Local configuration error",
};

int N_SysEx = sizeof(SysExMsg) / sizeof(SysExMsg[0]);

/*
 *  STATSTRING -- return string corresponding to an error status
 *
 *	Parameters:
 *		stat -- the status to decode.
 *
 *	Returns:
 *		The string corresponding to that status
 *
 *	Side Effects:
 *		none.
 */
char *
statstring(stat)
	int stat;
{
	static char ebuf[50];

	stat -= EX__BASE;
	if (stat < 0 || stat >= N_SysEx) {
		(void)sprintf(ebuf, "554 Unknown status %d", stat + EX__BASE);
		return(ebuf);
	}
	return(SysExMsg[stat]);
}
