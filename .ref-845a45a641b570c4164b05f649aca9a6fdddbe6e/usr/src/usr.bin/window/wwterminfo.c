/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwterminfo.c	3.1 (Berkeley) %G%";
#endif /* not lint */

#ifdef TERMINFO

#include "ww.h"
#include <stdio.h>
#include <paths.h>
#include "local.h"

/*
 * Terminfo support
 *
 * Written by Brian Buhrow
 *
 * Subsequently modified by Edward Wang
 */

/*
 * Initialize the working terminfo directory
 */
wwterminfoinit()
{
	FILE *fp;
	char buf[2048];

		/* make the directory */
	(void) sprintf(wwterminfopath, "%swwinXXXXXX", _PATH_TMP);
	mktemp(wwterminfopath);
	if (mkdir(wwterminfopath, 0755) < 0 ||
	    chmod(wwterminfopath, 00755) < 0) {
		wwerrno = WWE_SYS;
		return -1;
	}
	(void) setenv("TERMINFO", wwterminfopath, 1);
		/* make a termcap entry and turn it into terminfo */
	(void) sprintf(buf, "%s/cap", wwterminfopath);
	if ((fp = fopen(buf, "w")) == NULL) {
		wwerrno = WWE_SYS;
		return -1;
	}
	(void) fprintf(fp, "%sco#%d:li#%d:%s\n",
		WWT_TERMCAP, wwncol, wwnrow, wwwintermcap);
	(void) fclose(fp);
	(void) sprintf(buf,
		"cd %s; %s cap >info 2>/dev/null; %s info >/dev/null 2>&1",
		wwterminfopath, _PATH_CAPTOINFO, _PATH_TIC);
	(void) system(buf);
	return 0;
}

/*
 * Delete the working terminfo directory at shutdown
 */
wwterminfoend()
{

	switch (vfork()) {
	case -1:
		/* can't really do (or say) anything about errors */
		return -1;
	case 0:
		execl(_PATH_RM, _PATH_RM, "-rf", wwterminfopath, 0);
		return -1;
	default:
		return 0;
	}
}

#endif /* TERMINFO */
