/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "tune.h"

#ifdef MSDOS

#include <bios.h>
#include <dos.h>

#include "jove.h"

private void waitfun proto((void));

extern int UpdModLine;
#ifdef IBMPC
static char last = 0;
extern int specialkey;
#endif

getrawinchar()
{
#ifdef RAINBOW
	union REGS regs;
#endif /* RAINBOW */
#ifdef IBMPC
	unsigned scan;

	if (specialkey = last) {
		scan = last;
		last = 0;
		return scan;
	}
#endif /* IBMPC */

	while (!rawkey_ready())
		waitfun();

#ifdef IBMPC
	scan = _bios_keybrd(_KEYBRD_READ);
	if ((scan&0xff) == 0) {
		last = (char) (scan >> 8);
		return 0xff;
	}
	return scan&0xff;
#else /* IBMPC */
#ifdef RAINBOW
waitloop:
	regs.x.di = 2;
	int86(0x18, &regs, &regs);
	if (regs.h.al != 0)	/* should never happen, but who knows */
		return regs.h.al;
	else
		goto waitloop;
#else /* RAINBOW */
	return bdos(0x06, 0x00ff, 0xff) & 0xff;
#endif /* RAINBOW */
#endif /* IBMPC */
}

static int waiting = 0;

rawkey_ready()
{
#ifndef IBMPC
	union REGS regs;
#endif

	if (waiting)
		return 0;
#ifdef IBMPC
	if (last)
		return 1;

	return _bios_keybrd(_KEYBRD_READY);
#else /* IBMPC */
#ifdef RAINBOW
	regs.x.di = 4;
	int86(0x18, &regs, &regs);
	return regs.h.cl != 0;
#else /* RAINBOW */
	regs.h.ah = 0x44;		/* ioctl call */
	regs.x.bx = 0;			/* stdin file handle */
	regs.h.al = 0x06;		/* get input status */
	intdos(&regs, &regs);
	return regs.h.al & 1;
#endif /* RAINBOW */
#endif /* IBMPC */
}

#ifdef IBMPC
static long timecount, lastcount = 0;
#else
static char lastmin = 0;
#endif


private void
waitfun()
{
#ifndef IBMPC
	struct dostime_t tc;
#endif

	if (UpdModLine) {
		waiting = 1;
		redisplay();
		waiting = 0;
		return;
	}
#ifdef IBMPC
	if (_bios_timeofday(_TIME_GETCLOCK, &timecount) ||  /* after midnight */
	    (timecount > lastcount + 0x444) ) {
		lastcount = timecount;
		UpdModLine = 1;
	}
#else
	_dos_gettime(&tc);
	if (tc.minute != lastmin) {
		UpdModLine = 1;
		lastmin = tc.minute;
	}
#endif
}

#endif /* MSDOS */
