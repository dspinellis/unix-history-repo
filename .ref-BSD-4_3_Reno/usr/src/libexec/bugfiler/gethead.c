/*
 * Copyright (c) 1986, 1987 Regents of the University of California.
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
static char sccsid[] = "@(#)gethead.c	5.8 (Berkeley) 6/1/90";
#endif /* not lint */

#include <bug.h>
#include <sys/stat.h>
#include <stdio.h>
#include "pathnames.h"

static int	chk1(), pbuf();

#define ENT(X)	sizeof(X) - 1, X
HEADER	mailhead[] = {				/* mail headers */
	{ NO, YES,  NULL, ENT("Date:"), },
	{ NO,  NO,  NULL, ENT("From "), },
	{ NO, YES,  NULL, ENT("From:"), },
	{ NO,  NO,  chk1, ENT("Index:"), },
	{ NO, YES,  NULL, ENT("Message-Id:"), },
	{ NO, YES,  NULL, ENT("Reply-To:"), },
	{ NO, YES,  NULL, ENT("Return-Path:"), },
	{ NO,  NO,  pbuf, ENT("Subject:"), },
	{ NO, YES,  NULL, ENT("To:"), },
	{ NO,  NO,  NULL, ENT("Apparently-To:"), },
	{ ERR, }
};

FILE	*dfp;				/* distf file pointer */
char	dir[MAXNAMLEN],			/* subject and folder */
	folder[MAXNAMLEN];

/*
 * gethead --
 *	read mail and bug headers from bug report, construct redist headers
 */
gethead(redist)
	int	redist;
{
	register HEADER	*hp;		/* mail header pointer */
	char	*strcpy(), *malloc();

	if (redist) {
		int	fd;
		char	*distf;

		distf = _PATH_TMP;
		if (!(fd = mkstemp(distf)) || !(dfp = fdopen(fd, "w+")))
			error("can't create redistribution file %s.", distf);
		/* disappear after last reference is closed */
		(void)unlink(distf);
	}
	if (!freopen(tmpname, "r", stdin))
		error("can't read temporary bug file %s.", tmpname);

	while (fgets(bfr, sizeof(bfr), stdin)) {
		for (hp = mailhead; hp->found != ERR; ++hp)
			if (!hp->found)
				if (!strncmp(hp->tag, bfr, hp->len)) {
					if (hp->valid && !((*(hp->valid))(bfr)))
						break;
					if (!(hp->line = malloc((u_int)(strlen(bfr) + 1))))
						error("malloc failed.", CHN);
					(void)strcpy(hp->line, bfr);
					hp->found = YES;
					break;
				}
		if ((hp->found == ERR || hp->redist) && redist)
			fputs(bfr, dfp);
	}

	if (!mailhead[INDX_TAG].found)
		error("no readable \"Index:\" header in bug report.", CHN);
}

/*
 * chk1 --
 *	parse the "Index:" line into folder and directory
 */
static
chk1(line)
	char	*line;
{
	register char	*C;		/* tmp pointer */
	struct stat	sbuf;		/* existence check */
	char	*index();

	if (sscanf(line, " Index: %s %s ", folder, dir) != 2)
		return(NO);
	if (C = index(folder, '/')) {	/* deal with "bin/from.c" */
		if (C == folder)
			return(NO);
		*C = EOS;
	}
	if (stat(dir, &sbuf) || (sbuf.st_mode & S_IFMT) != S_IFDIR)
		return(NO);
	(void)pbuf(line);
	return(YES);
}

/*
 * pbuf --
 *	kludge so that summary file looks pretty
 */
static
pbuf(line)
	char	*line;
{
	register char	*rp,			/* tmp pointers */
			*wp;

	for (rp = line; *rp == ' ' || *rp == '\t'; ++rp);
	for (wp = line; *rp; ++wp) {
		if ((*wp = *rp++) != ' ' && *wp != '\t')
			continue;
		*wp = ' ';
		while (*rp == ' ' || *rp == '\t')
			++rp;
	}
	if (wp[-1] == ' ')			/* wp can't == line */
		--wp;
	*wp = EOS;
	return(YES);
}
