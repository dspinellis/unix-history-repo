/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)PCSTART.c	1.10 (Berkeley) 5/29/90";
#endif /* not lint */

#include <signal.h>
#include "h00vars.h"
#include "libpc.h"

/*
 * program variables
 */
struct display	_disply[MAXLVL];
int		_argc;
char		**_argv;
long		_stlim = 500000;
long		_stcnt = 0;
long		_seed = 1;
#ifdef ADDR32
char		*_minptr = (char *)0x7fffffff;
#endif ADDR32
#ifdef ADDR16
char		*_minptr = (char *)0xffff;
#endif ADDR16
char		*_maxptr = (char *)0;

/*
 * file record variables
 */
long		_filefre = PREDEF;
struct iorechd	_fchain = {
	0, 0, 0, 0,		/* only use fchain field */
	INPUT			/* fchain  */
};
struct iorec	*_actfile[MAXFILES] = {
	INPUT,
	OUTPUT,
	ERR
};

/*
 * standard files
 */
char		_inwin, _outwin, _errwin;
struct iorechd	input = {
	&_inwin,		/* fileptr */
	0,			/* lcount  */
	0x7fffffff,		/* llimit  */
	stdin,			/* fbuf    */
	OUTPUT,			/* fchain  */
	STDLVL,			/* flev    */
	"standard input",	/* pfname  */
	FTEXT|FREAD|SYNC|EOLN,	/* funit   */
	0,			/* fblk    */
	1			/* fsize   */
};
struct iorechd	output = {
	&_outwin,		/* fileptr */
	0,			/* lcount  */
	0x7fffffff,		/* llimit  */
	stdout,			/* fbuf    */
	ERR,			/* fchain  */
	STDLVL,			/* flev    */
	"standard output",	/* pfname  */
	FTEXT | FWRITE | EOFF,	/* funit   */
	1,			/* fblk    */
	1			/* fsize   */
};
struct iorechd	_err = {
	&_errwin,		/* fileptr */
	0,			/* lcount  */
	0x7fffffff,		/* llimit  */
	stderr,			/* fbuf    */
	FILNIL,			/* fchain  */
	STDLVL,			/* flev    */
	"Message file",		/* pfname  */
	FTEXT | FWRITE | EOFF,	/* funit   */
	2,			/* fblk    */
	1			/* fsize   */
};

PCSTART(mode)
	int mode;
{
	/*
	 * necessary only on systems which do not initialize
	 * memory to zero
	 */

	struct iorec	**ip;

	/*
	 * if running with runtime tests enabled, give more
	 * coherent error messages for FPEs
	 */
	if (mode) {
		signal(SIGFPE, EXCEPT);
	}
	for (ip = &_actfile[3]; ip < &_actfile[MAXFILES]; *ip++ = FILNIL);
}
