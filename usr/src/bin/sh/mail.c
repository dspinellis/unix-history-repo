/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mail.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * Routines to check for mail.  (Perhaps make part of main.c?)
 */

#include "shell.h"
#include "exec.h"	/* defines padvance() */
#include "var.h"
#include "output.h"
#include "memalloc.h"
#include "error.h"
#include <sys/types.h>
#include <sys/stat.h>


#define MAXMBOXES 10


STATIC int nmboxes;			/* number of mailboxes */
STATIC time_t mailtime[MAXMBOXES];	/* times of mailboxes */



/*
 * Print appropriate message(s) if mail has arrived.  If the argument is
 * nozero, then the value of MAIL has changed, so we just update the
 * values.
 */

void
chkmail(silent) {
	register int i;
	char *mpath;
	char *p;
	register char *q;
	struct stackmark smark;
	struct stat statb;

	if (silent)
		nmboxes = 10;
	if (nmboxes == 0)
		return;
	setstackmark(&smark);
	mpath = mpathset()? mpathval() : mailval();
	for (i = 0 ; i < nmboxes ; i++) {
		p = padvance(&mpath, nullstr);
		if (p == NULL)
			break;
		if (*p == '\0')
			continue;
		for (q = p ; *q ; q++);
		if (q[-1] != '/')
			abort();
		q[-1] = '\0';			/* delete trailing '/' */
#ifdef notdef /* this is what the System V shell claims to do (it lies) */
		if (stat(p, &statb) < 0)
			statb.st_mtime = 0;
		if (statb.st_mtime > mailtime[i] && ! silent) {
			out2str(pathopt? pathopt : "you have mail");
			out2c('\n');
		}
		mailtime[i] = statb.st_mtime;
#else /* this is what it should do */
		if (stat(p, &statb) < 0)
			statb.st_size = 0;
		if (statb.st_size > mailtime[i] && ! silent) {
			out2str(pathopt? pathopt : "you have mail");
			out2c('\n');
		}
		mailtime[i] = statb.st_size;
#endif
	}
	nmboxes = i;
	popstackmark(&smark);
}
