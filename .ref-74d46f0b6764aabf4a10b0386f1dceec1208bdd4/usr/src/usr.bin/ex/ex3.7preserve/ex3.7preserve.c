/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ex3.7preserve.c	7.18 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include <time.h>
#include <pwd.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "pathnames.h"

/*
 * Expreserve - preserve a file in _PATH_PRESERVE.
 * Bill Joy UCB November 13, 1977
 *
 * This routine is very naive - it doesn't remove anything from
 * _PATH_PRESERVE... this may mean that we leave stuff there...
 * the danger in doing anything with _PATH_PRESERVE is that the
 * clock may be screwed up and we may get confused.
 *
 * We are called in two ways - first from the editor with no arguments
 * and the standard input open on the temp file.  Second with an
 * argument to preserve the entire contents of _PATH_VARTMP (root only).
 *
 * BUG: should do something about preserving Rx... (register contents)
 *      temporaries.
 */

#ifdef VMUNIX
#define	HBLKS	2
#define	LBLKS	900
#else
#define HBLKS	1
#define	LBLKS	125
#endif
#define	FNSIZE	128

struct 	header {
	time_t	Time;			/* Time temp file last updated */
	int	Uid;			/* This users identity */
#ifdef VMUNIX
	int	Flines;
#else
	short	Flines;			/* Number of lines in file */
#endif
	char	Savedfile[FNSIZE];	/* The current file name */
	short	Blocks[LBLKS];		/* Blocks where line pointers stashed */
} H;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register DIR *tf;
	struct dirent *dirent;
	struct stat stbuf;
	int aflag, ch, err;
	char path[MAXPATHLEN];

	aflag = 0;
	while ((ch = getopt(argc, argv, "a")) != EOF)
		switch(ch) {
		case 'a':
			aflag = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (chdir(_PATH_PRESERVE) < 0)
		error(_PATH_PRESERVE);

	/*
	 * If only one argument, then preserve the standard input.
	 */
	if (!aflag)
		exit(copyout(NULL) ? 1 : 0);

	/*
	 * If not super user, then can only preserve standard input.
	 */
	if (getuid()) {
		errno = EPERM;
		error(NULL);
	}

	/*
	 * ... else preserve all the stuff in /var/tmp, removing
	 * it as we go.
	 */
	if (!(tf = opendir(_PATH_VARTMP)))
		error(_PATH_VARTMP);

	err = 0;
	while ((dirent = readdir(tf)) != NULL) {
		/* Ex temporaries must begin with Ex. */
		if (dirent->d_name[0] != 'E' || dirent->d_name[1] != 'x')
			continue;
		(void)sprintf(path, "%s/%s", _PATH_VARTMP, dirent->d_name);
		if (stat(path, &stbuf) || !S_ISREG(stbuf.st_mode))
			continue;
		/*
		 * Save the bastard.
		 */
		err |= copyout(path);
	}
	(void)closedir(tf);
	exit(err);
}

copyout(name)
	char *name;
{
	struct stat sb;
	register int ifd, ofd, nr, nw, off, rval;
	char buf[8*1024], fname[20];

	/* Open any given file name. */
	if (name) {
		if ((ifd = open(name, O_RDWR)) < 0)
			return(1);
		(void)fstat(ifd, &sb);
		if (!sb.st_size) {
			(void)unlink(name);
			return(0);
		}
	} else {
		ifd = STDIN_FILENO;
		/* vi hands us an fd, it's not necessarily at the beginning. */
		(void)lseek(ifd, 0l, SEEK_SET);
	}

	if (read(ifd, &H, sizeof(H)) != sizeof(H))
		goto format;

	/*
	 * Consistency checks so we don't copy out garbage.
	 */
	if (H.Flines < 0) {
		(void)fprintf(stderr,
		    "ex3.7preserve: negative number of lines\n");
		goto format;
	}

	if (H.Blocks[0] != HBLKS || H.Blocks[1] != HBLKS+1) {
		(void)fprintf(stderr,
		    "ex3.7preserve: blocks %d %d\n", H.Blocks[0], H.Blocks[1]);
		goto format;
	}

	if (!name && H.Uid != getuid()) {
		(void)fprintf(stderr, "ex3.7preserve: wrong user-id\n");
		goto format;
	}

	if (lseek(ifd, 0l, SEEK_SET)) {
		(void)fprintf(stderr,
		    "ex3.7preserve: negative number of lines\n");
format:		(void)fprintf(stderr, "ex3.7preserve: %s\n", strerror(EFTYPE));
		return (1);
	}

	/*
	 * If no name was assigned to the file, then give it the name
	 * LOST, by putting this in the header.  This involves overwriting
	 * the "input" file.
	 */
	if (H.Savedfile[0] == 0) {
		(void)strcpy(H.Savedfile, "LOST");
		(void)write(ifd, &H, sizeof(H));
		H.Savedfile[0] = 0;
		(void)lseek(ifd, 0l, SEEK_SET);
	}

	/* File is good.  Get a name and create a file for the copy. */
	(void)strcpy(fname, "ExXXXXXX");
	if ((ofd = mkstemp(fname)) == -1)
		return(1);

	/* Copy the file. */
	rval = 0;
	while ((nr = read(ifd, buf, sizeof(buf))) > 0)
		for (off = 0; off < nr; nr -= nw, off += nw)
			if ((nw = write(ofd, buf + off, nr)) < 0) {
				(void)fprintf(stderr,
				    "ex3.7preserve: tmp file: %s\n",
				    strerror(errno));
				rval = 1;
				break;
			}
	if (nr < 0) {
		(void)fprintf(stderr, "ex3.7preserve: %s: %s\n",
		    name ? name : "stdin", strerror(errno));
		rval = 1;
	}
	    
	if (rval)
		(void)unlink(fname);
	else {
		(void)fchown(ofd, H.Uid, 0);
		notify(H.Uid, H.Savedfile, (int)name, H.Time);
		if (name)
			(void)unlink(name);
	}
	(void)close(ifd);
	(void)close(ofd);
	return(rval);
}

/* Notify user uid that his file fname has been saved. */
notify(uid, fname, flag, time)
	int uid;
	char *fname;
	time_t	time;
{
	struct passwd *pp;
	register FILE *mf;
	static int reenter;
	static char hostname[MAXHOSTNAMELEN], *timestamp;
	char cmd[MAXPATHLEN + 50], croak[50];

	pp = getpwuid(uid);
	if (pp == NULL)
		return;

	if (!reenter) {
		reenter = 1;
		(void)gethostname(hostname, sizeof(hostname));
		timestamp = ctime(&time);
		timestamp[16] = 0;	/* blast from seconds on */
	}

	(void)snprintf(cmd, sizeof(cmd), 
	    "%s -i -t -F \"The Editor\" -f root", _PATH_SENDMAIL);
	mf = popen(cmd, "w");
	if (mf == NULL)
		return;
	(void)fprintf(mf,
	    "Reply-To: root@%s\nFrom: root@%s (The Editor)\nTo: %s\n",
	    hostname, hostname, pp->pw_name);

	/*
	 * flag says how the editor croaked: "the editor was killed" is
	 * perhaps still not an ideal error message.  Usually, either it
	 * was forcably terminated or the phone was hung up, but we don't
	 * know which.
	 */
	(void)snprintf(croak, sizeof(croak), 
	    flag ? "the system went down"
		 : "the editor was killed");
	if (!fname  || !fname[0]) {
		fname = "LOST";
		fprintf(mf, "Subject: editor saved \"LOST\"\n\n");
		fprintf(mf, "You were editing a file without a name\n");
		fprintf(mf, "at %s on the machine %s when %s.\n",
		    timestamp, hostname, croak);
		fprintf(mf,
		   "Since the file had no name, it has been named \"LOST\".\n");
	} else {
		fprintf(mf, "Subject: editor saved \"%s\"\n\n", fname);
		fprintf(mf, "You were editing the file %s\n", fname);
		fprintf(mf, "at %s on the machine %s\n", timestamp, hostname);
		fprintf(mf, "when %s.\n", croak);
	}
	fprintf(mf, "\nYou can retrieve most of your changes to this file\n");
	fprintf(mf, "using the \"recover\" command of the editor.\n");
	fprintf(mf,
	    "An easy way to do this is to give the command \"vi -r %s\".\n",
	    fname);
	fprintf(mf, "This method also works using \"ex\" and \"edit\".\n\n");
	pclose(mf);
}

/*
 *	people making love
 *	never exactly the same
 *	just like a snowflake 
 */
error(msg)
	char *msg;
{
	(void)fprintf(stderr, "ex3.7preserve: ");
	if (msg)
		(void)fprintf(stderr, "%s: ", msg);
	(void)fprintf(stderr, "%s\n", strerror(errno));
	exit(1);
}

usage()
{
	(void)fprintf(stderr, "usage: ex3.7preserve [-a]\n");
	exit(1);
}
