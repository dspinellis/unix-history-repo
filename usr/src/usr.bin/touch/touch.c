/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)touch.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * Attempt to set the modify date of a file to the current date.  If the
 * file exists, read and write its first character.  If the file doesn't
 * exist, create it, unless -c option prevents it.  If the file is read-only,
 * -f forces chmod'ing and touch'ing.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int	dontcreate;	/* set if -c option */
static int	force;		/* set if -f option */

void	err __P((const char *, ...));
int	readwrite __P((char *, off_t));
int	touch __P((char *));
__dead	void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, retval;

	dontcreate = force = retval = 0;
	while ((ch = getopt(argc, argv, "cf")) != EOF)
		switch((char)ch) {
		case 'c':
			dontcreate = 1;
			break;
		case 'f':
			force = 1;
			break;
		case '?':
		default:
			usage();
		}
	if (!*(argv += optind))
		usage();
	do {
		retval |= touch(*argv);
	} while (*++argv);
	exit(retval);
}

int
touch(filename)
	char *filename;
{
	struct stat sb;

	if (stat(filename, &sb) == -1) {
		if (!dontcreate)
			return (readwrite(filename, (off_t)0));
		err("%s: %s", filename, strerror(ENOENT));
		return (1);
	}
	if ((sb.st_mode & S_IFMT) != S_IFREG) {
		err("%s: %s", filename, strerror(EFTYPE));
		return (1);
	}
	if (!access(filename, R_OK | W_OK))
		return (readwrite(filename, sb.st_size));
	if (force) {
		int retval;

		if (chmod(filename, DEFFILEMODE)) {
			err("%s: add permissions: %s",
			    filename, strerror(errno));
			return (1);
		}
		retval = readwrite(filename, sb.st_size);
		if (chmod(filename, sb.st_mode)) {
			err("%s: restore permissions: %s",
			    filename, strerror(errno));
			return (1);
		}
		return (retval);
	}
	err("%s: cannot touch\n", filename);
	return (1);
}

int
readwrite(filename, size)
	char *filename;
	off_t size;
{
	int fd;
	char first;

	if (size) {
		fd = open(filename, O_RDWR, 0);
		if (fd == -1)
			goto error;
		if (read(fd, &first, 1) != 1)
			goto error;
		if (lseek(fd, (off_t)0, SEEK_SET) == -1)
			goto error;
		if (write(fd, &first, 1) != 1)
			goto error;
	} else {
		fd = creat(filename, DEFFILEMODE);
		if (fd == -1)
			goto error;
	}
	if (close(fd) == -1) {
error:		err("%s: %s", filename, strerror(errno));
		return (1);
	}
	return (0);
}

__dead void
usage()
{
	fprintf(stderr, "usage: touch [-cf] file ...\n");
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
	(void)fprintf(stderr, "touch: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
}
