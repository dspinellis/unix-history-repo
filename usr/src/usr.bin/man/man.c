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
static char sccsid[] = "@(#)man.c	5.3 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/file.h>
#include <ctype.h>

#define	DEF_PAGER	"more -s"
#define	DEF_PATH	"/usr/man:/usr/local/man"
#define	LOCAL_PATH	"/usr/local/man"
#define	LOCAL_NAME	"local"
#define	NO		0
#define	YES		1

#define	NO_SECTION	0
#define	S_THREEF	9
#define	S_NEW		10
#define	S_OLD		11

/* this array maps a character (ex: '4') to an offset in dirlist */
#define	secno(x)	(seclist[(int)(x - '0')])
static int	seclist[] = { -1, 1, 4, 5, 6, 7, 3, 8, 2, -1, -1 };

/* sub directory list, ordered for searching */
typedef struct something_meaningful {
	char	*name,
		*msg;
} DIR;

DIR	dirlist[] = {		/* sub-directory list */
	"notused", "",		"cat1", "1st",		"cat8", "8th",
	"cat6", "6th",		"cat2", "2nd",		"cat3", "3rd",
	"cat4", "4th",		"cat5", "5th", 		"cat7", "7th",
	"cat3f", "3rd (F)",	"new", "new",		"old", "old",
	NULL, NULL,
};

static int	nomore;			/* copy file to stdout */
static char	*defpath,		/* default search path */
		*locpath,		/* local search path */
		*machine,		/* machine type */
		*manpath,		/* current search path */
		*pager;			/* requested pager */

main(argc, argv)
	int	argc;
	register char	**argv;
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
	if (!defpath && !(defpath = getenv("MANPATH")))
		defpath = DEF_PATH;
	locpath = LOCAL_PATH;
	for (; *defpath && *defpath == ':'; ++defpath);

	for (; *argv; ++argv) {
		section = NO_SECTION;
		manpath = DEF_PATH;
		switch(**argv) {
		/* hardwired section numbers, fix here if they change */
		case '1': case '2': case '4': case '5': case '6':
		case '7': case '8':
			if (!(*argv)[1]) {
				section = secno((*argv)[0]);
				goto numtest;
			}
			break;
		case '3':
			if (!(*argv)[1]) {			/* "3" */
				section = secno((*argv)[0]);
numtest:			if (!*++argv) {
					fprintf(stderr, "man: what do you want from the %s section of the manual?\n", dirlist[section].msg);
					exit(1);
				}
			}					/* "3[fF]" */
			if (((*argv)[1] == 'f'  || (*argv)[1] == 'F') && !(*argv)[2]) {
				section = S_THREEF;
				if (!*++argv) {
					fprintf(stderr, "man: what do you want from the %s section of the manual?\n", dirlist[S_THREEF].msg);
					exit(1);
				}
			}
			break;
		case 'l':					/* local */
			if (!(*argv)[1])			/* "l" */
				section = NO_SECTION;		/* "l2" */
			else if (isdigit((*argv)[1]) && !(*argv)[2])
				section = secno((*argv)[1]);
			else {
				int	lex;
				lex = strcmp(LOCAL_NAME, *argv);
				if (!lex)			/* "local" */
					section = NO_SECTION;	/* "local2" */
				else if (lex < 0 && isdigit((*argv)[sizeof(LOCAL_NAME) - 1]) && !(*argv)[sizeof(LOCAL_NAME)])
					section = secno((*argv)[sizeof(LOCAL_NAME) - 1]);
				else
					break;
			}
			if (!*++argv) {
				fputs("man: what do you want from the local section of the manual?\n", stderr);
				exit(1);
			}
			manpath = locpath;
			break;
		case 'n':					/* new */
			if (!(*argv)[1] || !strcmp(*argv, dirlist[S_NEW].name)) {
				section = S_NEW;
				goto strtest;
			}
			break;
		case 'o':					/* old */
			if (!(*argv)[1] || !strcmp(*argv, dirlist[S_OLD].name)) {
				section = S_OLD;
strtest:			if (!*++argv) {
					fprintf(stderr, "man: what do you want from the %s section of the manual?\n", dirlist[section].msg);
					exit(1);
				}
			}
			break;
		}
		if (!manual(section, *argv))
			if (manpath == locpath)
				fprintf(stderr, "No entry for %s in the %s section of the local manual.\n", *argv, dirlist[section].msg);
			else if (section == NO_SECTION)
				fprintf(stderr, "No entry for %s in the manual.\n", *argv);
			else
				fprintf(stderr, "No entry for %s in the %s section of the manual.\n", *argv, dirlist[section].msg);
	}
	exit(0);
}

static
manual(section, name)
	int	section;
	char	*name;
{
	register DIR	*dir;
	register char	*beg, *end;
	char	*index();

	for (beg = manpath;; beg = end + 1) {
		if (end = index(beg, ':'))
			*end = '\0';
		if (section == NO_SECTION) {
			for (dir = dirlist + 1; dir->name; ++dir)
				if (find(beg, dir->name, name))
					return(YES);
		}
		else if (find(beg, dirlist[section].name, name))
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

	(void)sprintf(fname, "%s/%s/%s.0", beg, dir, name);
	if (!access(fname, R_OK)) {
		show(fname);
		return(YES);
	}
	(void)sprintf(fname, "%s/%s/%s/%s.0", beg, dir, machine, name);
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
