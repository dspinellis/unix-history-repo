/*
 * Copyright (c) 1986, 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)gethead.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "pathnames.h"
#include "bug.h"
#include "extern.h"

static int chk1 __P((char *));
static int pbuf __P((char *));

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
void
gethead(redist)
	int	redist;
{
	register HEADER	*hp;		/* mail header pointer */

	if (redist) {
		int	fd;
		char	*distf;

		distf = strdup(_PATH_TMP);
		if (!(fd = mkstemp(distf)) || !(dfp = fdopen(fd, "w+")))
			error("can't create redistribution file %s.", distf);
		/* disappear after last reference is closed */
		(void)unlink(distf);
		free(distf);
	}
	if (!freopen(tmpname, "r", stdin))
		error("can't read temporary bug file %s.", tmpname);

	while (fgets(bfr, sizeof(bfr), stdin)) {
		for (hp = mailhead; hp->found != ERR; ++hp)
			if (!hp->found)
				if (!strncmp(hp->tag, bfr, hp->len)) {
					if (hp->valid && !((*(hp->valid))(bfr)))
						break;
					if (!(hp->line =
					    malloc((u_int)(strlen(bfr) + 1))))
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
static int
chk1(line)
	char	*line;
{
	register char	*C;		/* tmp pointer */
	struct stat	sbuf;		/* existence check */

	if (sscanf(line, " Index: %s %s ", folder, dir) != 2)
		return(NO);
	if (C = strchr(folder, '/')) {	/* deal with "bin/from.c" */
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
static int
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
