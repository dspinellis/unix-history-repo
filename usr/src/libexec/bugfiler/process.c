/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)process.c	5.2 (Berkeley) 87/01/28";
#endif not lint

#include <bug.h>
#include <sys/file.h>
#include <sys/dir.h>
#include <sys/time.h>
#include <stdio.h>

extern HEADER	mailhead[];			/* mail headers */
extern int	lfd;				/* lock file descriptor */
extern char	dir[],				/* directory */
		folder[];			/* sub-directory */

char	pfile[MAXPATHLEN];			/* permanent file name */

/*
 * process --
 *	process a bug report
 */
process()
{
	register int	rval;			/* read return value */
	struct timeval	tp;			/* time of day */
	struct timezone	tzp;
	char	*ctime();

	/* copy report to permanent file */
	sprintf(pfile,"%s/%s/%d",dir,folder,getnext());
	fprintf(stderr,"\t%s\n",pfile);
	if (!(freopen(pfile,"w",stdout)))
		error("unable to create permanent bug file %s.",pfile);
	rewind(stdin);
	while ((rval = read(fileno(stdin),bfr,sizeof(bfr))) != ERR && rval)
		write(fileno(stdout),bfr,rval);
	REL_LOCK;

	/* append information to the summary file */
	sprintf(bfr,"%s/%s",dir,SUMMARY_FILE);
	GET_LOCK;
	if (!(freopen(bfr,"a",stdout)))
		error("unable to append to summary file %s.",bfr);
	else {
		if (gettimeofday(&tp,&tzp))
			error("unable to get time of day.",CHN);
		printf("\n%s\t\t%s\t%s\t%s\tOwner: Bugs Bunny\n\tComment: Received\n",pfile,ctime(&tp.tv_sec),mailhead[INDX_TAG].line,mailhead[SUBJ_TAG].found ? mailhead[SUBJ_TAG].line : "Subject:\n");
	}
	REL_LOCK;
	fclose(stdout);
}

/*
 * getnext --
 *	get next file name (number)
 */
static
getnext()
{
	register struct direct	*d;		/* directory structure */
	register DIR	*dirp;			/* directory pointer */
	register int	n;			/* number values */

	GET_LOCK;
	sprintf(bfr,"%s/%s",dir,folder);
	if (!(dirp = opendir(bfr))) {
		REL_LOCK;
		error("unable to read folder directory %s.",bfr);
	}
	for (n = 0;d = readdir(dirp);)
		n = MAX(n,atoi(d->d_name));
	closedir(dirp);
	return(++n);
}
