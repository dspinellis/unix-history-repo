/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mv.c	5.7 (Berkeley) 4/21/88";
#endif /* not lint */

/*
 * mv file1 file2
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/file.h>
#include <stdio.h>
#include <errno.h>

#define	DELIM	'/'
#define MODEBITS 07777

#define	ISDIR(st)	(((st).st_mode&S_IFMT) == S_IFDIR)
#define	ISLNK(st)	(((st).st_mode&S_IFMT) == S_IFLNK)
#define	ISREG(st)	(((st).st_mode&S_IFMT) == S_IFREG)
#define	ISDEV(st) \
	(((st).st_mode&S_IFMT) == S_IFCHR || ((st).st_mode&S_IFMT) == S_IFBLK)

char	*dname();
int	iflag = 0;	/* interactive mode */
int	fflag = 0;	/* force overwriting */
extern	unsigned errno;

main(argc, argv)
	register int argc;
	register char **argv;
{
	extern int optind;
	struct stat st;
	int ch, r;
	char *dest;

	while ((ch = getopt(argc, argv, "-fi")) != EOF)
		switch((char)ch) {
		case '-':
			goto endarg;
		case 'f':
			fflag++;
			break;
		case 'i':
			iflag++;
			break;
		case '?':
		default:
			usage();
		}
endarg:	argv += optind;
	argc -= optind;

	if (argc < 2)
		usage();
	dest = argv[argc - 1];
	if (stat(dest, &st) >= 0 && ISDIR(st)) {
		for (r = 0; --argc; ++argv)
			r |= movewithshortname(*argv, dest);
		exit(r);
	}
	if (argc != 2)
		usage();
	r = move(argv[0], argv[1]);
	exit(r);
}

movewithshortname(src, dest)
	char *src, *dest;
{
	register char *shortname;
	char target[MAXPATHLEN + 1];

	shortname = dname(src);
	if (strlen(dest) + strlen(shortname) > MAXPATHLEN - 1) {
		error("%s/%s: pathname too long", dest,
			shortname);
		return (1);
	}
	(void)sprintf(target, "%s/%s", dest, shortname);
	return (move(src, target));
}

move(source, target)
	char *source, *target;
{
	int targetexists;
	struct stat s1, s2;

	if (lstat(source, &s1) < 0) {
		Perror2(source, "Cannot access");
		return (1);
	}
	/*
	 * First, try to rename source to destination.
	 * The only reason we continue on failure is if
	 * the move is on a nondirectory and not across
	 * file systems.
	 */
	targetexists = lstat(target, &s2) >= 0;
	if (targetexists) {
		if (s1.st_dev == s2.st_dev && s1.st_ino == s2.st_ino) {
			error("%s and %s are identical", source, target);
			return (1);
		}
		if (!fflag && isatty(fileno(stdin)))
			if (iflag) {
				if (!query("remove %s? ", target))
					return (1);
			}
			else if (access(target, W_OK) < 0 &&
			    !query("override protection %o for %s? ",
			    s2.st_mode & MODEBITS, target))
				return (1);
	}
	if (rename(source, target) >= 0)
		return (0);
	if (errno != EXDEV) {
		Perror2(errno == ENOENT && targetexists == 0 ? target : source,
		    "rename");
		return (1);
	}
	if (ISDIR(s1)) {
		error("can't mv directories across file systems");
		return (1);
	}
	if (targetexists && unlink(target) < 0) {
		Perror2(target, "Cannot unlink");
		return (1);
	}
	/*
	 * File can't be renamed, try to recreate the symbolic
	 * link or special device, or copy the file wholesale
	 * between file systems.
	 */
	if (ISLNK(s1)) {
		register m;
		char symln[MAXPATHLEN + 1];

		m = readlink(source, symln, sizeof (symln) - 1);
		if (m < 0) {
			Perror(source);
			return (1);
		}
		symln[m] = '\0';

		(void) umask(~(s1.st_mode & MODEBITS));
		if (symlink(symln, target) < 0) {
			Perror(target);
			return (1);
		}
		goto cleanup;
	}
	(void) umask(0);
	if (ISDEV(s1)) {
		struct timeval tv[2];

		if (mknod(target, s1.st_mode, s1.st_rdev) < 0) {
			Perror(target);
			return (1);
		}

		tv[0].tv_sec = s1.st_atime;
		tv[0].tv_usec = 0;
		tv[1].tv_sec = s1.st_mtime;
		tv[1].tv_usec = 0;
		(void) utimes(target, tv);
		goto cleanup;
	}
	if (ISREG(s1)) {
		register int fi, fo, n;
		struct timeval tv[2];
		char buf[MAXBSIZE];

		fi = open(source, 0);
		if (fi < 0) {
			Perror(source);
			return (1);
		}

		fo = creat(target, s1.st_mode & MODEBITS);
		if (fo < 0) {
			Perror(target);
			close(fi);
			return (1);
		}

		for (;;) {
			n = read(fi, buf, sizeof buf);
			if (n == 0) {
				break;
			} else if (n < 0) {
				Perror2(source, "read");
				close(fi);
				close(fo);
				return (1);
			} else if (write(fo, buf, n) != n) {
				Perror2(target, "write");
				close(fi);
				close(fo);
				return (1);
			}
		}

		close(fi);
		close(fo);

		tv[0].tv_sec = s1.st_atime;
		tv[0].tv_usec = 0;
		tv[1].tv_sec = s1.st_mtime;
		tv[1].tv_usec = 0;
		(void) utimes(target, tv);
		goto cleanup;
	}
	error("%s: unknown file type %o", source, s1.st_mode);
	return (1);

cleanup:
	if (unlink(source) < 0) {
		Perror2(source, "Cannot unlink");
		return (1);
	}
	return (0);
}

/*VARARGS*/
query(prompt, a1, a2)
	char *a1;
{
	register int i, c;

	fprintf(stderr, prompt, a1, a2);
	i = c = getchar();
	while (c != '\n' && c != EOF)
		c = getchar();
	return (i == 'y');
}

char *
dname(name)
	register char *name;
{
	register char *p;

	p = name;
	while (*p)
		if (*p++ == DELIM && *p)
			name = p;
	return name;
}

/*VARARGS*/
error(fmt, a1, a2)
	char *fmt;
{

	fprintf(stderr, "mv: ");
	fprintf(stderr, fmt, a1, a2);
	fprintf(stderr, "\n");
}

Perror(s)
	char *s;
{
	char buf[MAXPATHLEN + 10];

	(void)sprintf(buf, "mv: %s", s);
	perror(buf);
}

Perror2(s1, s2)
	char *s1, *s2;
{
	char buf[MAXPATHLEN + 20];

	(void)sprintf(buf, "mv: %s: %s", s1, s2);
	perror(buf);
}

usage()
{
	fputs("usage: mv [-if] file1 file2 or mv [-if] file/directory ... directory\n", stderr);
	exit(1);
}
