#ifndef lint
static	char *sccsid = "@(#)mloop.c	3.1 83/09/02";
#endif

#include "defs.h"

mloop()
{
	register n;
	register char *p;
	int imask;

	while (!quit) {
		if (!incmd && selwin->ww_state != WWS_HASPROC) {
			incmd = 1;
			error("Process died.");
		}
		if (incmd) {
			docmd();
			continue;
		}
		/*
		 * Loop until we get some keyboard input.
		 */
		while (ibufc == 0) {
			wwcurtowin(selwin);
			wwupdate();
			wwflush();
			while (imask = 1, wwforce(&imask) < 0)
				;
			if ((imask & 1) == 0)
				continue;
			/* NOTE: ibufc == 0 */
			ibufp = ibuf;
			/* may block */
			if ((ibufc = read(0, ibuf, sizeof ibuf)) < 0) {
				ibufc = 0;
				nreade++;
			} else if (ibufc == 0)
				nreadz++;
			else
				nreadc += ibufc;
			nread++;
		}
		/*
		 * Weird loop.  Copy the buffer to the pty
		 * and stopping on the escape character
		 * in a hopefully efficient way.
		 * Probably a good thing to make ibufc == 1 a special
		 * case.
		 */
		for (p = ibufp, n = ibufc;;) {
			if (--n < 0) {
				(void) write(selwin->ww_pty, ibufp, ibufc);
				ibufp = ibuf;
				ibufc = 0;
				break;
			}
			if (*p++ == escapec) {
				if ((n = p - ibufp) > 1)
					(void) write(selwin->ww_pty,
						ibufp, n - 1);
				ibufp = p;
				ibufc -= n;
				incmd = 1;
				break;
			}
		}
	}
}
