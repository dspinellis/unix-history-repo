/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)gethead.c	5.1 (Berkeley) 86/11/25";
#endif not lint

#include <bug.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <stdio.h>

static int	chk1();

#define ENT(X)	sizeof(X) - 1, X
HEADER	mailhead[] = {				/* mail headers */
	{ NO, YES,  NULL, ENT("Date:"), },
	{ NO,  NO,  NULL, ENT("From "), },
	{ NO, YES,  NULL, ENT("From:"), },
	{ NO,  NO,  chk1, ENT("Index:"), },
	{ NO, YES,  NULL, ENT("Message-Id:"), },
	{ NO,  NO,  NULL, ENT("Reply-To:"), },
	{ NO,  NO,  NULL, ENT("Return-Path:"), },
	{ NO,  NO,  NULL, ENT("Subject:"), },
	{ NO,  NO,  NULL, ENT("To:"), },
	{ ERR, }
};

extern short	do_redist,		/* if redistributing report */
		made_dist;		/* if dist file needs removing */
extern char	tmpname[];		/* temp bug report file */

char	*distf = TMP_FILE,		/* redist temp file */
	dir[MAXNAMLEN],			/* subject and folder */
	folder[MAXNAMLEN];

/*
 * gethead --
 *	read mail and bug headers from bug report, construct redist headers
 */
gethead()
{
	register HEADER	*hp;		/* mail header pointer */
	register FILE	*dfp;		/* distf file pointer */
	char	*strcpy(), *malloc(), *mktemp();

	if (do_redist && (!mktemp(distf) || !(dfp = fopen(distf,"w"))))
		error("unable to create redistribution file %s.",distf);
	made_dist = YES;
	if (!freopen(tmpname,"r",stdin))
		error("unable to read temporary bug file %s.",tmpname);

	while (fgets(bfr,sizeof(bfr),stdin)) {
		for (hp = mailhead;hp->found != ERR;++hp)
			if (!hp->found)
				if (!strncmp(hp->tag,bfr,hp->len)) {
					if (hp->valid && !((*(hp->valid))(bfr)))
						break;
					if (!(hp->line = malloc((u_int)(strlen(bfr) + 1))))
						error("unable to allocate space for header search.",CHN);
					strcpy(hp->line,bfr);
					hp->found = YES;
					break;
				}
		if ((hp->found == ERR || hp->redist) && do_redist)
			fputs(bfr,dfp);
	}

	if (!mailhead[INDX_TAG].found)
		error("no readable \"Index:\" header in bug report.",CHN);
	if (do_redist)
		fclose(dfp);
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

	if (sscanf(line," Index: %s %s ",folder,dir) != 2)
		return(NO);

	/* backward compatible, deal with "bin/from.c" */
	if (C = index(folder,'/')) {
		if (C == folder)
			return(NO);
		*C = EOS;
	}

	if (stat(dir,&sbuf) || (sbuf.st_mode & S_IFMT) != S_IFDIR)
		return(NO);
	return(YES);
}
