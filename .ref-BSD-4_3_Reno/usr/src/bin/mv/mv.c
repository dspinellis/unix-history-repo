/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ken Smith of The State University of New York at Buffalo.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mv.c	5.9 (Berkeley) 5/31/90";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/errno.h>
#include <stdio.h>
#include <string.h>
#include "pathnames.h"

extern int errno;
int fflg, iflg;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register int baselen, exitval, len;
	register char *p, *endp;
	struct stat sbuf;
	int ch;
	char path[MAXPATHLEN + 1];

	while (((ch = getopt(argc, argv, "-if")) != EOF))
		switch((char)ch) {
		case 'i':
			++iflg;
			break;
		case 'f':
			++fflg;
			break;
		case '-':		/* undocumented; for compatibility */
			goto endarg;
		case '?':
		default:
			usage();
		}
endarg:	argc -= optind;
	argv += optind;

	if (argc < 2)
		usage();

	/*
	 * if stat fails on target, it doesn't exist (or can't be accessed
	 * by the user, doesn't matter which) try the move.  If target exists,
	 * and isn't a directory, try the move.  More than 2 arguments is an
	 * error.
	 */
	if (stat(argv[argc - 1], &sbuf) || !S_ISDIR(sbuf.st_mode)) {
		if (argc > 2)
			usage();
		exit(do_move(argv[0], argv[1]));
	}

	/* got a directory, move each file into it */
	(void)strcpy(path, argv[argc - 1]);
	baselen = strlen(path);
	endp = &path[baselen];
	*endp++ = '/';
	++baselen;
	for (exitval = 0; --argc; ++argv) {
		if ((p = rindex(*argv, '/')) == NULL)
			p = *argv;
		else
			++p;
		if ((baselen + (len = strlen(p))) >= MAXPATHLEN)
			(void)fprintf(stderr,
			    "mv: %s: destination pathname too long\n", *argv);
		else {
			bcopy(p, endp, len + 1);
			exitval |= do_move(*argv, path);
		}
	}
	exit(exitval);
}

do_move(from, to)
	char *from, *to;
{
	struct stat sbuf;
	int ask, ch;

	/*
	 * Check access.  If interactive and file exists ask user if it
	 * should be replaced.  Otherwise if file exists but isn't writable
	 * make sure the user wants to clobber it.
	 */
	if (!fflg && !access(to, F_OK)) {
		ask = 0;
		if (iflg) {
			(void)fprintf(stderr, "overwrite %s? ", to);
			ask = 1;
		}
		else if (access(to, W_OK) && !stat(to, &sbuf)) {
			(void)fprintf(stderr, "override mode %o on %s? ",
			    sbuf.st_mode & 07777, to);
			ask = 1;
		}
		if (ask) {
			if ((ch = getchar()) != EOF && ch != '\n')
				while (getchar() != '\n');
			if (ch != 'y')
				return(0);
		}
	}
	if (!rename(from, to))
		return(0);
	if (errno != EXDEV) {
		(void)fprintf(stderr,
		    "mv: rename %s to %s: %s\n", from, to, strerror(errno));
		return(1);
	}
	/*
	 * if rename fails, and it's a regular file, do the copy
	 * internally; otherwise, use cp and rm.
	 */
	if (stat(from, &sbuf)) {
		(void)fprintf(stderr,
		    "mv: %s: %s\n", from, strerror(errno));
		return(1);
	}
	return(S_ISREG(sbuf.st_mode) ?
	    fastcopy(from, to, &sbuf) : copy(from, to));
}

fastcopy(from, to, sbp)
	char *from, *to;
	struct stat *sbp;
{
	struct timeval tval[2];
	static u_int blen;
	static char *bp;
	register int nread, from_fd, to_fd;
	char *malloc();

	if ((from_fd = open(from, O_RDONLY, 0)) < 0) {
		(void)fprintf(stderr,
		    "mv: %s: %s\n", from, strerror(errno));
		return(1);
	}
	if ((to_fd = open(to, O_WRONLY|O_CREAT|O_TRUNC, sbp->st_mode)) < 0) {
		(void)fprintf(stderr,
		    "mv: %s: %s\n", to, strerror(errno));
		(void)close(from_fd);
		return(1);
	}
	if (!blen && !(bp = malloc(blen = sbp->st_blksize))) {
		(void)fprintf(stderr, "mv: %s: out of memory.\n", from);
		return(1);
	}
	while ((nread = read(from_fd, bp, blen)) > 0)
		if (write(to_fd, bp, nread) != nread) {
			(void)fprintf(stderr, "mv: %s: %s\n",
			    to, strerror(errno));
			goto err;
		}
	if (nread < 0) {
		(void)fprintf(stderr, "mv: %s: %s\n", from, strerror(errno));
err:		(void)unlink(to);
		(void)close(from_fd);
		(void)close(to_fd);
		return(1);
	}
	(void)fchown(to_fd, sbp->st_uid, sbp->st_gid);
	(void)fchmod(to_fd, sbp->st_mode);

	(void)close(from_fd);
	(void)close(to_fd);

	tval[0].tv_sec = sbp->st_atime;
	tval[1].tv_sec = sbp->st_mtime;
	tval[0].tv_usec = tval[1].tv_usec = 0;
	(void)utimes(to, tval);
	(void)unlink(from);
	return(0);
}

copy(from, to)
	char *from, *to;
{
	int pid, status;

	if (!(pid = vfork())) {
		execlp(_PATH_CP, "mv", "-pr", from, to);
		(void)fprintf(stderr, "mv: can't exec %s.\n", _PATH_CP);
		_exit(1);
	}
	(void)waitpid(pid, &status, 0);
	if (!WIFEXITED(status) || WEXITSTATUS(status))
		return(1);
	if (!(pid = vfork())) {
		execlp(_PATH_RM, "mv", "-rf", from);
		(void)fprintf(stderr, "mv: can't exec %s.\n", _PATH_RM);
		_exit(1);
	}
	(void)waitpid(pid, &status, 0);
	return(!WIFEXITED(status) || WEXITSTATUS(status));
}

usage()
{
	(void)fprintf(stderr,
"usage: mv [-if] src target;\n   or: mv [-if] src1 ... srcN directory\n");
	exit(1);
}
