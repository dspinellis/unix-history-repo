/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)names.c	5.1 (Berkeley) %G%
 */

#ifdef hp300
char *defdrives[] = { "sd0", "sd1", "sd2", "rd0", "rd1", "rd2", 0 };
#define	DONE
#endif

#ifdef tahoe
char *defdrives[] = { "dk0", "dk1", "dk2", 0 };
#define	DONE
#endif

#ifdef vax
char *defdrives[] = { "hp0", "hp1", "hp2", 0 };
#define	DONE
#endif

#ifndef DONE
char *defdrives[] = { 0 };
#endif
