/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Hugh Smith at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)contents.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>
#include <tzfile.h>
#include <dirent.h>
#include <stdio.h>
#include <ar.h>
#include "archive.h"

extern CHDR chdr;			/* converted header */
extern char *archive;			/* archive name */

/*
 * contents --
 *	Handles t[v] option - opens the archive and then reads headers,
 *	skipping member contents.
 */
contents(argv)
	register char **argv;
{
	register int afd, all;
	int eval;
	struct tm *tp;
	char *file, buf[25];
	
	afd = open_archive(O_RDONLY);

	for (all = !*argv; get_header(afd);) {
		if (all)
			file = chdr.name;
		else {
			file = *argv;
			if (!files(argv))
				goto next;
		}
		if (options & AR_V) {
			(void)strmode(chdr.mode, buf);
			(void)printf("%s %6d/%-6d %8ld ",
			    buf + 1, chdr.uid, chdr.gid, chdr.size);
			tp = localtime(&chdr.date);
			(void)strftime(buf, sizeof(buf), "%b %e %H:%M %Y", tp);
			(void)printf("%s %s\n", buf, file);
		} else
			(void)printf("%s\n", file);
		if (!all && !*argv)
			break;
next:		skipobj(afd);
	} 
	eval = 0;
	ORPHANS;
	close_archive(afd);
	return(eval);
}
