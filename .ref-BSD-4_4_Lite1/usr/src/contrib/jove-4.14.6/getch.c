/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "tune.h"

#ifdef	MSDOS

#include <bios.h>
#include <dos.h>

#include "jove.h"
#include "disp.h"

private void waitfun proto((void));

#ifdef	IBMPC
private char last = '\0';
extern int specialkey;
#endif

int
getrawinchar()
{
#ifdef	IBMPC
	unsigned scan;

	if (specialkey = last) {
		scan = last;
		last = '\0';
		return scan;
	}

	while (!rawkey_ready())
		waitfun();

	scan = _bios_keybrd(_KEYBRD_READ);
	if ((scan&0xff) == 0) {
		last = (char) (scan >> 8);
		return 0xff;
	}
	return scan&0xff;

#else	/* !IBMPC */
#ifdef	RAINBOW

	union REGS regs;

	while (!rawkey_ready())
		waitfun();

	for (;;) {
		regs.x.di = 2;
		int86(0x18, &regs, &regs);
		if (regs.h.al != 0)	/* should never happen, but who knows */
			return regs.h.al;
	}
#else	/* !RAINBOW */

	while (!rawkey_ready())
		waitfun();
	return bdos(0x06, 0x00ff, 0xff) & 0xff;
#endif	/* !RAINBOW */
#endif	/* !IBMPC */
}

private bool waiting = NO;

bool
rawkey_ready()
{
#ifdef	IBMPC
	if (waiting)
		return NO;
	if (last)
		return YES;

	return _bios_keybrd(_KEYBRD_READY) != 0;
#else	/* !IBMPC */
	union REGS regs;

	if (waiting)
		return NO;
#ifdef	RAINBOW
	regs.x.di = 4;
	int86(0x18, &regs, &regs);
	return regs.h.cl != 0;
#else	/* !RAINBOW */
	regs.h.ah = 0x44;		/* ioctl call */
	regs.x.bx = 0;			/* stdin file handle */
	regs.h.al = 0x06;		/* get input status */
	intdos(&regs, &regs);
	return regs.h.al & 1;
#endif	/* !RAINBOW */
#endif	/* !IBMPC */
}

#ifdef	IBMPC
private long timecount, lastcount = 0;
#else
private char lastmin = 0;
#endif


private void
waitfun()
{
	if (UpdModLine) {
		waiting = YES;
		redisplay();
		waiting = NO;
		return;
	}
#ifdef	IBMPC
	if (_bios_timeofday(_TIME_GETCLOCK, &timecount) ||  /* after midnight */
	    (timecount > lastcount + 0x444) ) {
		lastcount = timecount;
		UpdModLine = YES;
	}
#else
	{
		struct dostime_t tc;

		_dos_gettime(&tc);
		if (tc.minute != lastmin) {
			UpdModLine = YES;
			lastmin = tc.minute;
		}
	}
#endif
}

#endif	/* MSDOS */
