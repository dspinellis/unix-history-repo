#ifndef lint
static	char *sccsid = "@(#)mloop.c	3.2 84/01/16";
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
		while (wwibc == 0) {
			wwcurtowin(selwin);
			wwiomux();
		}
		/*
		 * Weird loop.  Copy the buffer to the pty
		 * and stopping on the escape character
		 * in a hopefully efficient way.
		 * Probably a good thing to make wwibc == 1 a special
		 * case.
		 */
		for (p = wwibp, n = wwibc;;) {
			if (--n < 0) {
				(void) write(selwin->ww_pty, wwibp, wwibc);
				wwibc = 0;
				break;
			}
			if (*p++ == escapec) {
				if ((n = p - wwibp) > 1)
					(void) write(selwin->ww_pty,
						wwibp, n - 1);
				wwibp = p;
				wwibc -= n;
				incmd = 1;
				break;
			}
		}
	}
}
