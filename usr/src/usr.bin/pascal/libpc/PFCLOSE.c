/* Copyright (c) 1982 Regents of the University of California */

static	char sccsid[] = "@(#)PFCLOSE.c	1.1	(Berkeley)	%G%";

/*
 * Close a Pascal file deallocating resources as appropriate.
 */

#include <stdio.h>
#include "h00vars.h"
#include "libpc.h"

struct iorec *
PFCLOSE(filep)
	register struct iorec *filep;
{
	fprintf(stderr, "Closing file %s\n", filep->pfname);
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
		if ((filep->funit & TEMP) != 0 && unlink(filep->pfname)) {
			PERROR("Could not remove ", filep->pfname);
			return;
		}
	}
	_actfile[filep->fblk] = FILNIL;
	return (filep->fchain);
}
