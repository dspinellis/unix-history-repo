/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)man.c	5.10 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <ctype.h>

#define	DEF_PAGER	"/usr/ucb/more -s"
#define	DEF_PATH	"/usr/man:/usr/new/man:/usr/local/man"

#define	LOCAL_PATH	"/usr/local/man"
#define	LOCAL_NAME	"local"

#define	NEW_PATH	"/usr/new/man"
#define	NEW_NAME	"new"

#define	NO		0
#define	YES		1

#define	NO_SECTION	0
#define	S_THREEF	9
#define	S_OLD		10

/* this array maps a character (ex: '4') to an offset in stanlist */
#define	secno(x)	(seclist[(int)(x - '0')])
static int	seclist[] = { -1, 1, 4, 5, 6, 7, 3, 8, 2, -1, -1 };

/* sub directory list, ordered for searching */
typedef struct {
	char	*name,
		*msg;
} DIR;

DIR	stanlist[] = {		/* standard sub-directory list */
	"notused", "",		"cat1", "1st",		"cat8", "8th",
	"cat6", "6th",		"cat2", "2nd",		"cat3", "3rd",
	"cat4", "4th",		"cat5", "5th", 		"cat7", "7th",
	"cat3f", "3rd (F)",	"cat.old", "old",	NULL, NULL,
},	sec1list[] = {		/* section one list */
	"notused", "",		"cat1", "1st",		"cat8", "8th",
	"cat6", "6th",		"cat.old", "old",	NULL, NULL,
};

static DIR	*dirlist;		/* list of directories to search */
static int	nomore,			/* copy file to stdout */
		where;			/* just tell me where */
static char	*defpath,		/* default search path */
		*locpath,		/* local search path */
		*machine,		/* machine type */
		*manpath,		/* current search path */
		*newpath,		/* new search path */
		*pager;			/* requested pager */

main(argc, argv)
	int	argc;
	register char	**argv;
{
	int	section;
	char	**arg_start, **arg,
		*getenv(), *malloc();

	arg_start = argv;
	for (--argc, ++argv; argc && (*argv)[0] == '-'; --argc, ++argv)
		switch((*argv)[1]) {
		case 0:			/* just write to stdout */
			nomore = YES;
			break;
		case 'M':
		case 'P':		/* backward compatibility */
			if ((*argv)[2])
				defpath = *argv + 2;
			else {
				if (argc < 2) {
					fprintf(stderr, "%s: missing path\n", *argv);
					exit(1);
				}
				--argc;
				defpath = *++argv;
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
		case 'w':
			/*
			 * Deliberately undocumented; really only useful when
			 * you're moving man pages around.  Not worth adding.
			 */
			where = YES;
			break;
		case '?':
		default:
			fprintf(stderr, "man: illegal option -- %c\n", (*argv)[1]);
			usage();
		}
	if (!argc)
		usage();

	if (!nomore)
		if (!isatty(1))
			nomore = YES;
		else if (pager = getenv("PAGER")) {
			register char	*C;

			/*
			 * if the user uses "more", we make it "more -s"
			 * watch out for PAGER = "mypager /usr/ucb/more"
			 */
			for (C = pager; *C && !isspace(*C); ++C);
			for (; C > pager && *C != '/'; --C);
			if (C != pager)
				++C;
			/* make sure it's "more", not "morex" */
			if (!strncmp(C, "more", 4) && (!C[4] || isspace(C[4]))) {
				C += 4;
				/*
				 * sizeof is 1 more than # of chars, so,
				 * allocate for the rest of the PAGER
				 * environment variable, a space, and the EOS.
				 */
				if (!(pager = malloc((u_int)(strlen(C) + sizeof(DEF_PAGER) + 1)))) {
					fputs("man: out of space.\n", stderr);
					exit(1);
				}
				(void)sprintf(pager, "%s %s", DEF_PAGER, C);
			}
		}
		else
			pager = DEF_PAGER;
	if (!(machine = getenv("MACHINE")))
		machine = MACHINE;
	if (!defpath && !(defpath = getenv("MANPATH")))
		defpath = DEF_PATH;
	locpath = LOCAL_PATH;
	newpath = NEW_PATH;
	for (; *defpath && *defpath == ':'; ++defpath);

	/* Gentlemen... start your kludges! */
	for (; *argv; ++argv) {
		section = NO_SECTION;
		manpath = defpath;
		switch(**argv) {
		/*
		 * Section 1 requests are really for section 1, 6, 8, in the
		 * standard, local and new directories and section old. Since
		 * old isn't broken up into a directory of cat[1-8], we just
		 * treat it like a subdirectory of the others.
		 */
		case '1': case '2': case '4': case '5': case '6':
		case '7': case '8':
			if (!(*argv)[1]) {
				section = secno((*argv)[0]);
				goto numtest;
			}
			break;

		/* sect. 3 requests are for either section 3, or section 3F. */
		case '3':
			if (!(*argv)[1]) {			/* "3" */
				section = secno((*argv)[0]);
numtest:			if (!*++argv) {
					fprintf(stderr, "man: what do you want from the %s section of the manual?\n", stanlist[section].msg);
					exit(1);
				}
			}					/* "3[fF]" */
			else if (((*argv)[1] == 'f'  || (*argv)[1] == 'F') && !(*argv)[2]) {
				section = S_THREEF;
				if (!*++argv) {
					fprintf(stderr, "man: what do you want from the %s section of the manual?\n", stanlist[S_THREEF].msg);
					exit(1);
				}
			}
			break;
		/*
		 * Requests for the new or local sections can have subsection
		 * numbers appended to them, i.e. "local3" is really
		 * local/cat3.
		 */
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
			if (!(*argv)[1])			/* "n" */
				section = NO_SECTION;		/* "n2" */
			else if (isdigit((*argv)[1]) && !(*argv)[2])
				section = secno((*argv)[1]);
			else {
				int	lex;
				lex = strcmp(NEW_NAME, *argv);
				if (!lex)			/* "new" */
					section = NO_SECTION;	/* "new2" */
				else if (lex < 0 && isdigit((*argv)[sizeof(NEW_NAME) - 1]) && !(*argv)[sizeof(NEW_NAME)])
					section = secno((*argv)[sizeof(NEW_NAME) - 1]);
				else
					break;
			}
			if (!*++argv) {
				fputs("man: what do you want from the new section of the manual?\n", stderr);
				exit(1);
			}
			manpath = newpath;
			break;
		case 'o':					/* old */
			if (!(*argv)[1] || !strcmp(*argv, stanlist[S_OLD].msg)) {
				section = S_OLD;
				if (!*++argv) {
					fprintf(stderr, "man: what do you want from the %s section of the manual?\n", stanlist[section].msg);
					exit(1);
				}
			}
			break;
		}
		if (section == 1) {
			dirlist = sec1list;
			section = NO_SECTION;
		}
		else
			dirlist = stanlist;
		/*
		 * This is really silly, but I wanted to put out rational
		 * errors, not just "I couldn't find it."  This if statement
		 * knows an awful lot about what gets assigned to what in
		 * the switch statement we just passed through.  Sorry.
		 */
		if (!manual(section, *argv) && !where)
			if (manpath == locpath)
				if (section == NO_SECTION)
					fprintf(stderr, "No entry for %s in the local manual.\n", *argv);
				else
					fprintf(stderr, "No entry for %s in the %s section of the local manual.\n", *argv, stanlist[section].msg);
			else if (manpath == newpath)
				if (section == NO_SECTION)
					fprintf(stderr, "No entry for %s in the new manual.\n", *argv);
				else
					fprintf(stderr, "No entry for %s in the %s section of the new manual.\n", *argv, stanlist[section].msg);
			else if (dirlist == sec1list)
				fprintf(stderr, "No entry for %s in the 1st section of the manual.\n", *argv);
			else if (section == NO_SECTION)
				fprintf(stderr, "No entry for %s in the manual.\n", *argv);
			else
				fprintf(stderr, "No entry for %s in the %s section of the manual.\n", *argv, stanlist[section].msg);
	}
	exit(0);
}

/*
 * manual --
 *	given a section number and a file name go through the directory
 *	list and find a file that matches.
 */
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
		if (section == NO_SECTION)
			for (dir = dirlist; (++dir)->name;) {
				if (find(beg, dir->name, name))
					return(YES);
			}
		else if (find(beg, stanlist[section].name, name))
			return(YES);
		if (!end)
			return(NO);
	}
	/*NOTREACHED*/
}

/*
 * find --
 *	given a directory path, a sub-directory and a file name,
 *	see if a file exists in ${directory}/${dir}/{file name}
 *	or in ${directory}/${dir}/${machine}/${file name}.
 */
static
find(beg, dir, name)
	char	*beg, *dir, *name;
{
	char	fname[MAXPATHLEN + 1];

	(void)sprintf(fname, "%s/%s/%s.0", beg, dir, name);
	if (access(fname, R_OK)) {
		(void)sprintf(fname, "%s/%s/%s/%s.0", beg, dir, machine, name);
		if (access(fname, R_OK))
			return(NO);
	}
	if (where)
		printf("man: found in %s.\n", fname);
	else
		show(fname);
	return(!where);
}

/*
 * show --
 *	display the file
 */
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

/*
 * usage --
 *	print out a usage message and die
 */
static
usage()
{
	fputs("usage: man [-] [-M path] [section] title ...\n", stderr);
	exit(1);
}
