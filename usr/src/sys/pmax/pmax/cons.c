/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: cons.c 1.1 90/07/09$
 *
 *	@(#)cons.c	7.4 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/file.h>
#include <sys/conf.h>

#include <machine/machMon.h>

/*
 * Console output may be redirected to another tty
 * (e.g. a window); if so, constty will point to the current
 * virtual console.
 */
struct	tty *constty;		/* virtual console output device */

/*
 * Get character from console.
 */
cngetc()
{
	int (*f)();
#include "dc.h"
#if NDC > 0
#include <machine/dc7085cons.h>
#include <pmax/dev/pdma.h>
	extern struct pdma dcpdma[];

	/* check to be sure device has been initialized */
	if (dcpdma[0].p_addr)
		return (dcKBDGetc());
	f = (int (*)())MACH_MON_GETCHAR;
	return (*f)();
#else
	f = (int (*)())MACH_MON_GETCHAR;
	return (*f)();
#endif
}

/*
 * Print a character on console.
 */
cnputc(c)
	register int c;
{
#include "pm.h"
#if NPM > 0
	pmPutc(c);
#else
#include "cfb.h"
#if NCFB > 0
	cfbPutc(c);
#else
	int s;
	void (*f)() = (void (*)())MACH_MON_PUTCHAR;

	s = splhigh();
	(*f)(c);
	splx(s);
#endif
#endif
}
