/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)PCSTART.c	1.10 (Berkeley) %G%";
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
