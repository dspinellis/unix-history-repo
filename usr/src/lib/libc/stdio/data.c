/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)data.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <sys/param.h>

struct	_iobuf	_iob[_NFILE] ={
	{ 0, NULL, NULL, NULL, _IOREAD, 0},
	{ 0, NULL, NULL, NULL, _IOWRT, 1},
	{ 0, NULL, NULL, NULL, _IOWRT+_IONBF, 2},
};
/*
 * Ptr to end of buffers
 */
struct	_iobuf	*_lastbuf ={ &_iob[_NFILE] };
