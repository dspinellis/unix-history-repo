/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ken Smith of The State University of New York at Buffalo.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mv.c	5.12 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "pathnames.h"

int fflg, iflg;

int	copy __P((char *, char *));
int	do_move __P((char *, char *));
void	err __P((const char *, ...));
int	fastcopy __P((char *, char *, struct stat *));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register int baselen, exitval, len;
	register char *p, *endp;
	struct stat sb;
	int ch;
	char path[MAXPATHLEN + 1];

	while (((ch = getopt(argc, argv, "-if")) != EOF))
		switch((char)ch) {
		case 'i':
			iflg = 1;
			break;
		case 'f':
			fflg = 1;
			break;
		case '-':		/* Undocumented; for compatibility. */
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
	 * If the stat on the target fails or the target isn't a directory,
	 * try the move.  More than 2 arguments is an error in this case.
	 */
	if (stat(argv[argc - 1], &sb) || !S_ISDIR(sb.st_mode)) {
		if (argc > 2)
			usage();
		exit(do_move(argv[0], argv[1]));
	}

	/* It's a directory, move each file into it. */
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
		if ((baselen + (len = strlen(p))) >= MAXPATHLEN) {
			err("%s: destination pathname too long", *argv);
			exitval = 1;
		} else {
			bcopy(p, endp, len + 1);
			exitval |= do_move(*argv, path);
		}
	}
	exit(exitval);
}

int
do_move(from, to)
	char *from, *to;
{
	struct stat sb;
	int ask, ch;

	/*
	 * Check access.  If interactive and file exists, ask user if it
	 * should be replaced.  Otherwise if file exists but isn't writable
	 * make sure the user wants to clobber it.
	 */
	if (!fflg && !access(to, F_OK)) {
		ask = 0;
		if (iflg) {
			(void)fprintf(stderr, "overwrite %s? ", to);
			ask = 1;
		}
		else if (access(to, W_OK) && !stat(to, &sb)) {
			(void)fprintf(stderr, "override mode %o on %s? ",
			    sb.st_mode & 07777, to);
			ask = 1;
		}
		if (ask) {
			if ((ch = getchar()) != EOF && ch != '\n')
				while (getchar() != '\n');
			if (ch != 'y')
				return (0);
		}
	}
	if (!rename(from, to))
		return (0);

	if (errno != EXDEV) {
		err("rename %s to %s: %s", from, to, strerror(errno));
		return (1);
	}

	/*
	 * If rename fails, and it's a regular file, do the copy internally;
	 * otherwise, use cp and rm.
	 */
	if (stat(from, &sb)) {
		err("%s: %s", from, strerror(errno));
		return (1);
	}
	return (S_ISREG(sb.st_mode) ?
	    fastcopy(from, to, &sb) : copy(from, to));
}

int
fastcopy(from, to, sbp)
	char *from, *to;
	struct stat *sbp;
{
	struct timeval tval[2];
	static u_int blen;
	static char *bp;
	register int nread, from_fd, to_fd;

	if ((from_fd = open(from, O_RDONLY, 0)) < 0) {
		err("%s: %s", from, strerror(errno));
		return (1);
	}
	if ((to_fd = open(to, O_CREAT|O_TRUNC|O_WRONLY, sbp->st_mode)) < 0) {
		err("%s: %s", to, strerror(errno));
		(void)close(from_fd);
		return (1);
	}
	if (!blen && !(bp = malloc(blen = sbp->st_blksize))) {
		err("%s", strerror(errno));
		return (1);
	}
	while ((nread = read(from_fd, bp, blen)) > 0)
		if (write(to_fd, bp, nread) != nread) {
			err("%s: %s", to, strerror(errno));
			goto err;
		}
	if (nread < 0) {
		err("%s: %s", from, strerror(errno));
err:		(void)unlink(to);
		(void)close(from_fd);
		(void)close(to_fd);
		return (1);
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
	return (0);
}

int
copy(from, to)
	char *from, *to;
{
	int pid, status;

	if (!(pid = vfork())) {
		execl(_PATH_CP, "mv", "-pR", from, to, NULL);
		err("%s: %s", _PATH_CP, strerror(errno));
		_exit(1);
	}
	(void)waitpid(pid, &status, 0);
	if (!WIFEXITED(status) || WEXITSTATUS(status))
		return (1);
	if (!(pid = vfork())) {
		execl(_PATH_RM, "mv", "-rf", from, NULL);
		err("%s: %s", _PATH_RM, strerror(errno));
		_exit(1);
	}
	(void)waitpid(pid, &status, 0);
	return (!WIFEXITED(status) || WEXITSTATUS(status));
}

void
usage()
{
	(void)fprintf(stderr,
"usage: mv [-if] src target;\n   or: mv [-if] src1 ... srcN directory\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "mv: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
}
