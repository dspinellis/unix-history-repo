#ifndef lint
static char sccsid[] = "@(#)mloop.c	3.9 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "defs.h"

mloop()
{
	wwrint();		/* catch typeahead before we set ASYNC */
	while (!quit) {
		if (incmd) {
			docmd();
		} else if (wwcurwin->ww_state != WWS_HASPROC) {
			if (!wwcurwin->ww_keepopen)
				closewin(wwcurwin);
			setcmd(1);
			if (wwpeekc() == escapec)
				(void) wwgetc();
			error("Process died.");
		} else {
			register struct ww *w = wwcurwin;
			register char *p;
			register n;

			wwiomux();
			if (wwibp < wwibq) {
				for (p = wwibp; p < wwibq && *p != escapec;
				     p++)
					;
				if ((n = p - wwibp) > 0) {
					if (!w->ww_ispty && w->ww_stopped)
						startwin(w);
					(void) write(w->ww_pty, wwibp, n);
					wwibp = p;
				}
				if (wwpeekc() == escapec) {
					(void) wwgetc();
					setcmd(1);
				}
			}
		}
	}
}
