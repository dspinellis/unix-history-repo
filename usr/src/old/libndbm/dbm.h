/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dbm.h	5.4 (Berkeley) %G%
 */

#ifndef NULL
/*
 * this is lunacy, we no longer use it (and never should have
 * unconditionally defined it), but, this whole file is for
 * backwards compatability - someone may rely on this.
 */
#define	NULL	((char *) 0)
#endif

/*
 * Pre-define the page block size to be the old dbm size.
 */

#define PBLKSIZ 1024

#include <ndbm.h>

datum	fetch();
datum	firstkey();
datum	nextkey();
