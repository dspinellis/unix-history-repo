/*
 * Copyright (c) 1983, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)bugfiler.c	5.6 (Berkeley) 86/11/25";
#endif not lint

/*
 * Bug report processing program, designed to be invoked
 * through aliases(5).
 */
#include <bug.h>
#include <sys/time.h>
#include <stdio.h>
#include <pwd.h>

extern char	*optarg;		/* getopt arguments */
extern int	optind;

int	lfd;				/* lock file descriptor */
short	do_redist = YES;		/* redistribut BR */
char	bfr[MAXBSIZE],			/* general I/O buffer */
	tmpname[sizeof(TMP_BUG) + 5];	/* temp bug file */

main(argc,argv)
int	argc;
char	**argv;
{
	register struct passwd	*pwd;	/* bugs password entry */
	register int	ch;		/* getopts char */
	register short	do_ack = YES;	/* acknowledge bug report */
	struct passwd	*getpwnam();

	while ((ch = getopt(argc,argv,"ar")) != EOF)
		switch((char)ch) {
			case 'a':
				do_ack = NO;
				break;
			case 'r':
				do_redist = NO;
				break;
			case '?':
			default:
				error("usage: bugfiler [-ar] [maildir]",CHN);
		}

	if (!(pwd = getpwnam(BUGS_ID)))
		error("bugs person %s is unknown",BUGS_ID);

	argv += optind;
	if (*argv) {		/* change to argument directory */
		if (chdir(*argv))
			error("can't move to %s.",*argv);
	}			/* change to bugs home directory */
	else if (chdir(pwd->pw_dir))
		error("can't move to %s.",pwd->pw_dir);

	if (setreuid(0,pwd->pw_uid))
		error("can't set id to %s.",BUGS_ID);

	umask(2);		/* everything is 664 */
	seterr();
	logit();
	make_copy();

	if (access(LOCK_FILE,R_OK) || (lfd = open(LOCK_FILE,O_RDONLY,0)) < 0)
		error("can't read lock file %s.",LOCK_FILE);

	gethead();
	process();

	if (setuid(0,0))
		error("can't set id to root.",CHN);

	if (do_ack)
		reply();
	if (do_redist)
		redist();

	unlink(tmpname);
	exit(OK);
}

/*
 * make_copy --
 *	make a copy of the bug report
 */
static
make_copy()
{
	register int	cnt,			/* read return value */
			tfd;			/* temp file descriptor */
	char	*mktemp(), *strcpy();

	/* use O_EXCL, since may not be able to get a lock file */
	for (cnt = 0;cnt < 20;++cnt)
		if ((tfd = open(mktemp(strcpy(tmpname,TMP_BUG)),O_WRONLY | O_CREAT | O_EXCL,0664)) >= 0) {
			while ((cnt = read(fileno(stdin),bfr,sizeof(bfr))) != ERR && cnt)
				write(tfd,bfr,cnt);
			close(tfd);
			return;
		}
	error("unable to make copy using %s.\n",tmpname);
}

/*
 * logit --
 *	log this run of the bugfiler
 */
static
logit()
{
	struct timeval	tp;
	struct timezone	tzp;
	char	*ctime();

	if (gettimeofday(&tp,&tzp))
		error("unable to get time of day.",CHN);
	fprintf(stderr,"\n>>> BUGFILER <<<\n\t%s",ctime(&tp.tv_sec));
}
