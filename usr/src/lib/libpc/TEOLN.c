/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)TEOLN.c	1.5 (Berkeley) 4/9/90";
#endif /* not lint */

#include "h00vars.h"

bool
TEOLN(filep)

	register struct iorec	*filep;
{
	if (filep->fblk >= MAXFILES || _actfile[filep->fblk] != filep ||
	    (filep->funit & FDEF)) {
		ERROR("Reference to an inactive file\n", 0);
		return;
	}
	if (filep->funit & FWRITE) {
		ERROR("%s: eoln is undefined on files open for writing\n",
		    filep->pfname);
		return;
	}
	IOSYNC(filep);
	if (filep->funit & EOFF) {
		ERROR("%s: eoln is undefined when eof is true\n",
		    filep->pfname);
		return;
	}
	if (filep->funit & EOLN)
		return TRUE;
	return FALSE;
}
