/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)man.c	5.1 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/file.h>
#include <ctype.h>

#define	DEF_PAGER	"more -s"	/* paging filter */
#define	DEF_PATH	"/usr/man"	/* default path */
#define	NO		0		/* no/false */
#define	YES		1		/* yes/true */

/*
 * See code for more info on this kludge; suffice it to say that the
 * local, new and old defines *have* to map into the dirlist array
 * correctly.  Notice also that cat1 is in array slot 1, etc. etc.
 * Continue to notice that it's also ordered for searching, i.e.
 * slots 1 - N are in the order you wish to search the directories.
 */
#define	NO_SECTION	0
#define	LOCAL_SECTION	9
#define	NEW_SECTION	10
#define	OLD_SECTION	11
#define	isname(x)	(x==LOCAL_SECTION || x==NEW_SECTION || x==OLD_SECTION)
static char	*machine,		/* machine type */
		*manpath,		/* search path */
		*pager,			/* pager */
		*dirlist[] = {		/* sub-directory list */
	"notused", 	"cat1",		"cat2",		"cat3",
	"cat4",		"cat5",		"cat6",		"cat7",
	"cat8",		"local",	"new",		"old",
	NULL,
};

static int	nomore;			/* don't use more */

main(argc, argv)
	int	argc;
	char	**argv;
{
	int	section;
	char	**arg_start, **arg,
		*getenv();

	arg_start = argv;
	for (--argc, ++argv; argc && (*argv)[0] == '-'; --argc, ++argv)
		switch((*argv)[1]) {
		case 0:			/* just write to stdout */
			nomore = YES;
			break;
		case 'M':
		case 'P':		/* backward compatibility */
			if ((*argv)[2])
				manpath = *argv + 2;
			else {
				if (argc < 2) {
					fprintf(stderr, "%s: missing path\n", *argv);
					exit(1);
				}
				--argc;
				manpath = *++argv;
			}
			break;
		/*
		 * "man -f" and "man -k" are undocumented ways of calling
		 * whatis(1) and apropos(1).  Just strip out the flag
		 * argument and jump.
		 */
		case 'f':
			for (arg = argv; arg[0] = arg[1]; ++arg);
			*arg_start = "whatis";
			execvp(*arg_start, arg_start);
			fputs("whatis: Command not found.\n", stderr);
			exit(1);
		case 'k':
			for (arg = argv; *arg = arg[1]; ++arg);
			*arg_start = "apropos";
			execvp(*arg_start, arg_start);
			fputs("apropos: Command not found.\n", stderr);
			exit(1);
		case '?':
		default:
			usage();
		}
	if (!argc)
		usage();

	if (!nomore)
		if (!isatty(1))
			nomore = YES;
		else if (!(pager = getenv("PAGER")))
			pager = DEF_PAGER;
	if (!(machine = getenv("MACHINE")))
		machine = MACHINE;
	if (!manpath && !(manpath = getenv("MANPATH")))
		manpath = DEF_PATH;
	for (; *manpath && *manpath == ':'; ++manpath);

	for (; *argv; ++argv) {
		section = NO_SECTION;
		switch(**argv) {
		/*
		 * hardwired section numbers, fix here if they do; note,
		 * only works for single digits.
		 */
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8':
			if (!(*argv)[1]) {
				section = (*argv)[0] - '0';
				if (!*++argv) {
					fprintf(stderr, "man: what do you want from section %d?\n", section);
					exit(1);
				}
			}
			break;
		/*
		 * backward compatibility: manl == local, mann == new,
		 * mano == old;
		 */
		case 'l':
			if (!*argv[1] || !strcmp(*argv, dirlist[LOCAL_SECTION])) {
				section = LOCAL_SECTION;
				goto argtest;
			}
			break;
		case 'n':
			if (!*argv[1] || !strcmp(*argv, dirlist[NEW_SECTION])) {
				section = NEW_SECTION;
				goto argtest;
			}
			break;
		case 'o':
			if (!*argv[1] || !strcmp(*argv, dirlist[OLD_SECTION])) {
				section = OLD_SECTION;
argtest:			if (!*++argv) {
					fprintf(stderr, "man: what do you want from the %s section?\n", dirlist[section]);
					exit(1);
				}
			}
			break;
		}
		if (!manual(section, *argv))
			if (section == NO_SECTION)
				fprintf(stderr, "No manual entry for %s.\n", *argv);
			else if (isname(section))
				fprintf(stderr, "No entry for %s in the %s section of the manual.\n", *argv, dirlist[section]);
			else
				fprintf(stderr, "No entry for %s in section %d of the manual.\n", *argv, section);
	}
	exit(0);
}

static
manual(section, name)
	int	section;
	char	*name;
{
	register char	*beg, *end, **dir;
	char	*index();

	for (beg = manpath;; beg = end + 1) {
		if (end = index(beg, ':'))
			*end = '\0';
		if (section == NO_SECTION) {
			/* notice the +1... */
			for (dir = dirlist + 1; *dir; ++dir)
				if (find(beg, *dir, name))
					return(YES);
		}
		else if (find(beg, dirlist[section], name))
			return(YES);
		if (!end)
			return(NO);
	}
	/*NOTREACHED*/
}

static
find(beg, dir, name)
	char	*beg, *dir, *name;
{
	char	fname[MAXPATHLEN + 1];

	(void)sprintf(fname, "%s/%s/%s", beg, dir, name);
	if (!access(fname, R_OK)) {
		show(fname);
		return(YES);
	}
	(void)sprintf(fname, "%s/%s/%s/%s", beg, dir, machine, name);
	if (!access(fname, R_OK)) {
		show(fname);
		return(YES);
	}
	return(NO);
}

static
show(fname)
	char	*fname;
{
	register int	fd, n;
	char	buf[BUFSIZ];

	if (nomore) {
		if (!(fd = open(fname, O_RDONLY, 0))) {
			perror("man: open");
			exit(1);
		}
		while ((n = read(fd, buf, sizeof(buf))) > 0)
			if (write(1, buf, n) != n) {
				perror("man: write");
				exit(1);
			}
		if (n == -1) {
			perror("man: read");
			exit(1);
		}
		(void)close(fd);
	}
	else {
		/*
		 * use system(2) in case someone's pager is
		 * "command arg1 arg2"
		 */
		(void)sprintf(buf, "%s %s", pager, fname);
		(void)system(buf);
	}
}

static
usage()
{
	fputs("usage: man [-] [-M path] [section] title ...\n", stderr);
	exit(1);
}
