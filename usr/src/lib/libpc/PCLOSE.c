/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PCLOSE.c 1.4 6/10/81";

#include "h00vars.h"

PCLOSE(level)

	struct iorec		*level;
{
	register struct iorec	*next;

	next = _fchain.fchain;
	while(next != FILNIL && next->flev <= level) {
		if (next->fbuf != 0) {
			if ((next->funit & FDEF) == 0) {
				if (next->fblk > PREDEF) {
					fflush(next->fbuf);
					setbuf(next->fbuf, NULL);
				}
				fclose(next->fbuf);
				if (ferror(next->fbuf)) {
					ERROR("%s: Close failed\n",
						next->pfname);
					return;
				}
			}
			if ((next->funit & TEMP) != 0 &&
			    unlink(next->pfname)) {
				PERROR("Could not remove ", next->pfname);
				return;
			}
		}
		_actfile[next->fblk] = FILNIL;
		next = next->fchain;
	}
	_fchain.fchain = next;
}
