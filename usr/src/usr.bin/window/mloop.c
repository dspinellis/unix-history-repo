#ifndef lint
static	char *sccsid = "@(#)mloop.c	3.3 84/03/03";
#endif

#include "defs.h"

mloop()
{
	while (!quit) {
		if (wwcurwin == 0) {
			docmd();
		} else if (wwcurwin->ww_state != WWS_HASPROC) {
			wwcurwin = 0;
			if (wwpeekc() == escapec)
				(void) wwgetc();
			error("Process died.");
		} else {
			register char *p;
			register n;

			wwiomux();
			if (wwibp < wwibq) {
				for (p = wwibp; p < wwibq && *p != escapec;
				     p++)
					;
				if ((n = p - wwibp) > 0) {
					(void) write(wwcurwin->ww_pty,
						wwibp, n);
					wwibp = p;
				}
				if (wwpeekc() == escapec) {
					wwcurwin = 0;
					(void) wwgetc();
				}
			}
		}
	}
}
