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
static char sccsid[] = "@(#)process.c	5.8 (Berkeley) 6/1/90";
#endif /* not lint */

#include <bug.h>
#include <sys/file.h>
#include <sys/time.h>
#include <stdio.h>
#include <ctype.h>

char	pfile[MAXPATHLEN];			/* permanent file name */

/*
 * process --
 *	copy report to permanent file,
 *	update summary file.
 */
process()
{
	register int	rval;			/* read return value */
	struct timeval	tp;			/* time of day */
	int	lfd;				/* lock file descriptor */
	char	*ctime();

	if (access(LOCK_FILE, R_OK) || (lfd = open(LOCK_FILE, O_RDONLY, 0)) < 0)
		error("can't find lock file %s.", LOCK_FILE);
	if (flock(lfd, LOCK_EX))
		error("can't get lock.", CHN);
	sprintf(pfile, "%s/%s/%d", dir, folder, getnext());
	fprintf(stderr, "\t%s\n", pfile);
	if (!(freopen(pfile, "w", stdout)))
		error("can't create %s.", pfile);
	rewind(stdin);
	while ((rval = read(fileno(stdin), bfr, sizeof(bfr))) != ERR && rval)
		if (write(fileno(stdout), bfr, rval) != rval)
			error("write to %s failed.", pfile);

	/* append information to the summary file */
	sprintf(bfr, "%s/%s", dir, SUMMARY_FILE);
	if (!(freopen(bfr, "a", stdout)))
		error("can't append to summary file %s.", bfr);
	if (gettimeofday(&tp, (struct timezone *)NULL))
		error("can't get time of day.", CHN);
	printf("\n%s\t\t%s\t%s\t%s\tOwner: Bugs Bunny\n\tStatus: Received\n", pfile, ctime(&tp.tv_sec), mailhead[INDX_TAG].line, mailhead[SUBJ_TAG].found ? mailhead[SUBJ_TAG].line : "Subject:\n");
	(void)flock(lfd, LOCK_UN);
	(void)fclose(stdout);
}

/*
 * getnext --
 *	get next file name (number)
 */
static
getnext()
{
	register struct direct *d;		/* directory structure */
	register DIR *dirp;			/* directory pointer */
	register int highval, newval;
	register char *p;

	(void)sprintf(bfr, "%s/%s", dir, folder);
	if (!(dirp = opendir(bfr)))
		error("can't read folder directory %s.", bfr);
	for (highval = -1; d = readdir(dirp);) {
		for (p = d->d_name; *p && isdigit(*p); ++p);
		if (!*p && (newval = atoi(d->d_name)) > highval)
			highval = newval;
	}
	closedir(dirp);
	return(++highval);
}
