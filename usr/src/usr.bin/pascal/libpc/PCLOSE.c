/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PCLOSE.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

PCLOSE(level)

	struct iorec		*level;
{
	register struct iorec	*next;

	next = _fchain.fchain;
	while(next != FILNIL && next->flev <= level) {
		if (next->fbuf != 0) {
			if ((next->funit & FDEF) == 0) {
				fclose(next->fbuf);
				if (ferror(next->fbuf)) {
					ERROR(ECLOSE, next->pfname);
					return;
				}
			}
			if ((next->funit & TEMP) != 0 &&
			    unlink(next->pfname)) {
				ERROR(EREMOVE, next->pfname);
				return;
			}
		}
		_actfile[next->fblk] = FILNIL;
		next = next->fchain;
	}
	_fchain.fchain = next;
}
