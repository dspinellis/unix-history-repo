/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)xinstall.c	5.11 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <a.out.h>
#include <grp.h>
#include <pwd.h>
#include <stdio.h>
#include <ctype.h>

#define	YES	1			/* yes/true */
#define	NO	0			/* no/false */

#define	PERROR(head, msg) { \
	fputs(head, stderr); \
	perror(msg); \
}

static struct passwd	*pp;
static struct group	*gp;
static int	docopy, dostrip,
		mode = 0755;
static char	*group, *owner,
		pathbuf[MAXPATHLEN];

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern char	*optarg;
	extern int	optind;
	struct stat	from_sb, to_sb;
	int	ch, no_target;
	char	*to_name;

	while ((ch = getopt(argc, argv, "cg:m:o:s")) != EOF)
		switch((char)ch) {
		case 'c':
			docopy = YES;
			break;
		case 'g':
			group = optarg;
			break;
		case 'm':
			mode = atoo(optarg);
			break;
		case 'o':
			owner = optarg;
			break;
		case 's':
			dostrip = YES;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;
	if (argc < 2)
		usage();

	/* get group and owner id's */
	if (group && !(gp = getgrnam(group))) {
		fprintf(stderr, "install: unknown group %s.\n", group);
		exit(1);
	}
	if (owner && !(pp = getpwnam(owner))) {
		fprintf(stderr, "install: unknown user %s.\n", owner);
		exit(1);
	}

	no_target = stat(to_name = argv[argc - 1], &to_sb);
	if (!no_target && (to_sb.st_mode & S_IFMT) == S_IFDIR) {
		for (; *argv != to_name; ++argv)
			install(*argv, to_name, YES);
		exit(0);
	}

	/* can't do file1 file2 directory/file */
	if (argc != 2)
		usage();

	if (!no_target) {
		if (stat(*argv, &from_sb)) {
			fprintf(stderr, "install: can't find %s.\n", *argv);
			exit(1);
		}
		if ((to_sb.st_mode & S_IFMT) != S_IFREG) {
			fprintf(stderr, "install: %s isn't a regular file.\n", to_name);
			exit(1);
		}
		if (to_sb.st_dev == from_sb.st_dev && to_sb.st_ino == from_sb.st_ino) {
			fprintf(stderr, "install: %s and %s are the same file.\n", *argv, to_name);
			exit(1);
		}
		/* unlink now... avoid ETXTBSY errors later */
		(void)unlink(to_name);
	}
	install(*argv, to_name, NO);
	exit(0);
}

/*
 * install --
 *	build a path name and install the file
 */
static
install(from_name, to_name, isdir)
	char	*from_name, *to_name;
	int	isdir;
{
	struct stat	from_sb;
	int	devnull, from_fd, to_fd;
	char	*C, *rindex();

	/* if try to install "/dev/null" to a directory, fails */
	if (isdir || strcmp(from_name, "/dev/null")) {
		if (stat(from_name, &from_sb)) {
			fprintf(stderr, "install: can't find %s.\n", from_name);
			exit(1);
		}
		if ((from_sb.st_mode & S_IFMT) != S_IFREG) {
			fprintf(stderr, "install: %s isn't a regular file.\n", from_name);
			exit(1);
		}
		/* build the target path */
		if (isdir) {
			(void)sprintf(pathbuf, "%s/%s", to_name, (C = rindex(from_name, '/')) ? ++C : from_name);
			to_name = pathbuf;
		}
		devnull = NO;
	}
	else
		devnull = YES;

	/* unlink now... avoid ETXTBSY errors later */
	(void)unlink(to_name);

	/* create target */
	if ((to_fd = open(to_name, O_CREAT|O_WRONLY|O_TRUNC, 0)) < 0) {
		PERROR("install: ", to_name);
		exit(1);
	}
	if (!devnull) {
		if ((from_fd = open(from_name, O_RDONLY, 0)) < 0) {
			(void)unlink(to_name);
			PERROR("install: open: ", from_name);
			exit(1);
		}
		if (dostrip)
			strip(from_fd, from_name, to_fd, to_name);
		else
			copy(from_fd, from_name, to_fd, to_name);
		(void)close(from_fd);
		if (!docopy)
			(void)unlink(from_name);
	}
	/* set owner, group, mode for target */
	if (fchmod(to_fd, mode)) {
		PERROR("install: fchmod: ", to_name);
		bad();
	}
	if ((group || owner) && fchown(to_fd, owner ? pp->pw_uid : -1,
	    group ? gp->gr_gid : -1)) {
		PERROR("install: fchown: ", to_name);
		bad();
	}
	(void)close(to_fd);
}

/*
 * strip --
 *	copy file, strip(1)'ing it at the same time
 */
static
strip(from_fd, from_name, to_fd, to_name)
	register int	from_fd, to_fd;
	char	*from_name, *to_name;
{
	typedef struct exec	EXEC;
	register long	size;
	register int	n;
	EXEC	head;
	char	buf[MAXBSIZE];
	off_t	lseek();

	if (read(from_fd, (char *)&head, sizeof(head)) < 0 || N_BADMAG(head)) {
		fprintf(stderr, "install: %s not in a.out format.\n", from_name);
		bad();
	}
	if (head.a_syms || head.a_trsize || head.a_drsize) {
		size = (long)head.a_text + head.a_data;
		head.a_syms = head.a_trsize = head.a_drsize = 0;
		if (head.a_magic == ZMAGIC)
			size += getpagesize() - sizeof(EXEC);
		if (write(to_fd, (char *)&head, sizeof(EXEC)) != sizeof(EXEC)) {
			PERROR("install: write: ", to_name);
			bad();
		}
		for (; size; size -= n)
			/* sizeof(buf) guaranteed to fit in an int */
			if ((n = read(from_fd, buf, (int)MIN(size, sizeof(buf)))) <= 0)
				break;
			else if (write(to_fd, buf, n) != n) {
				PERROR("install: write: ", to_name);
				bad();
			}
		if (size) {
			fprintf(stderr, "install: read: %s: premature EOF.\n", from_name);
			bad();
		}
		if (n == -1) {
			PERROR("install: read: ", from_name);
			bad();
		}
	}
	else {
		(void)lseek(from_fd, 0L, L_SET);
		copy(from_fd, from_name, to_fd, to_name);
	}
}

/*
 * copy --
 *	copy from one file to another
 */
static
copy(from_fd, from_name, to_fd, to_name)
	register int	from_fd, to_fd;
	char	*from_name, *to_name;
{
	register int	n;
	char	buf[MAXBSIZE];

	while ((n = read(from_fd, buf, sizeof(buf))) > 0)
		if (write(to_fd, buf, n) != n) {
			PERROR("install: write: ", to_name);
			bad();
		}
	if (n == -1) {
		PERROR("install: read: ", from_name);
		bad();
	}
}

/*
 * atoo --
 *	octal string to int
 */
static
atoo(str)
	register char	*str;
{
	register int	val;

	for (val = 0; isdigit(*str); ++str)
		val = val * 8 + *str - '0';
	return(val);
}

/*
 * bad --
 *	remove created target and die
 */
static
bad()
{
	(void)unlink(pathbuf);
	exit(1);
}

/*
 * usage --
 *	print a usage message and die
 */
static
usage()
{
	fputs("usage: install [-cs] [-g group] [-m mode] [-o owner] file1 file2;\n\tor file1 ... fileN directory\n", stderr);
	exit(1);
}
