/* Copyright (c) 1982 Regents of the University of California */

static	char sccsid[] = "@(#)PFCLOSE.c	1.3	(Berkeley)	1/21/83";

/*
 * Close a Pascal file deallocating resources as appropriate.
 */

#include "h00vars.h"
#include "libpc.h"

struct iorec *
PFCLOSE(filep, lastuse)
	register struct iorec *filep;
	bool lastuse;
{
	if ((filep->funit & FDEF) == 0 && filep->fbuf != NULL) {
		/*
		 * Have a previous buffer, close associated file.
		 */
		if (filep->fblk > PREDEF) {
			fflush(filep->fbuf);
			setbuf(filep->fbuf, NULL);
		}
		fclose(filep->fbuf);
		if (ferror(filep->fbuf)) {
			ERROR("%s: Close failed\n", filep->pfname);
			return;
		}
		/*
		 * Temporary files are discarded.
		 */
		if ((filep->funit & TEMP) != 0 && lastuse &&
		    unlink(filep->pfname)) {
			PERROR("Could not remove ", filep->pfname);
			return;
		}
	}
	_actfile[filep->fblk] = FILNIL;
	return (filep->fchain);
}
