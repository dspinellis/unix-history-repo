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
static char sccsid[] = "@(#)man.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <ctype.h>

#define	DEF_PAGER	"/usr/ucb/more -s"
#define	DEF_PATH	"/usr/man:/usr/new/man:/usr/local/man"
#define	LOCAL_PATH	"/usr/local/man"
#define	NEW_PATH	"/usr/new/man"

#define	NO	0
#define	YES	1

static int	nomore,			/* copy file to stdout */
		where;			/* just tell me where */
static char	*command,		/* command buffer */
		*defpath,		/* default search path */
		*locpath,		/* local search path */
		*machine,		/* machine type */
		*manpath,		/* current search path */
		*newpath,		/* new search path */
		*pager;			/* requested pager */

main(argc, argv)
	int argc;
	register char **argv;
{
	char **arg_start, **arg, *getenv(), *malloc(), *strcpy();

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
		 * "man -f" and "man -k" are backward contemptible,
		 * undocumented ways of calling whatis(1) and apropos(1).
		 * Just strip out the flag argument and jump.
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
		/*
		 * Deliberately undocumented; really only useful when
		 * you're moving man pages around.  Not worth adding.
		 */
		case 'w':
			where = YES;
			break;
		case '?':
		default:
			fprintf(stderr, "man: illegal option -- %c\n", (*argv)[1]);
			goto usage;
		}

	if (!argc) {
usage:		fputs("usage: man [-] [-M path] [section] title ...\n", stderr);
		exit(1);
	}

	if (!nomore)
		if (!isatty(1))
			nomore = YES;
		else if (pager = getenv("PAGER")) {
			register char *p;

			/*
			 * if the user uses "more", we make it "more -s"
			 * watch out for PAGER = "mypager /usr/ucb/more"
			 */
			for (p = pager; *p && !isspace(*p); ++p);
			for (; p > pager && *p != '/'; --p);
			if (p != pager)
				++p;
			/* make sure it's "more", not "morex" */
			if (!strncmp(p, "more", 4) && (!p[4] || isspace(p[4]))) {
				p += 4;
				/*
				 * allocate for the rest of the PAGER
				 * environment variable, a space, and the EOS.
				 */
				if (!(pager = malloc((u_int)(strlen(p) + sizeof(DEF_PAGER) + 1)))) {
					fputs("man: out of space.\n", stderr);
					exit(1);
				}
				(void)sprintf(pager, "%s %s", DEF_PAGER, p);
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
	man(argv);
	/* use system(3) in case someone's pager is "foo arg1 arg2" */
	if (command)
		(void)system(command);
	exit(0);
}

typedef struct {
	char	*name, *msg;
} DIR;
static DIR	list1[] = {		/* section one list */
	"cat1", "1st",		"cat8", "8th",		"cat6", "6th",
	"cat.old", "old",	NULL, NULL,
},		list2[] = {		/* rest of the list */
	"cat2", "2nd",		"cat3", "3rd",		"cat4", "4th",
	"cat5", "5th", 		"cat7", "7th",		"cat3f", "3rd (F)",
	NULL, NULL,
},		list3[2];		/* single section */

static
man(argv)
	char **argv;
{
	register char *p;
	DIR *section, *getsect();
	int res;

	for (; *argv; ++argv) {
		manpath = defpath;
		section = NULL;
		switch(**argv) {
		case 'l':				/* local */
			for (p = *argv; isalpha(*p); ++p);
			if (!strncmp(*argv, "l", p - *argv) ||
			    !strncmp(*argv, "local", p - *argv)) {
				manpath = locpath;
				if (section = getsect(p))
					goto argtest;
			}
			break;
		case 'n':				/* new */
			for (p = *argv; isalpha(*p); ++p);
			if (!strncmp(*argv, "n", p - *argv) ||
			    !strncmp(*argv, "new", p - *argv)) {
				manpath = newpath;
				if (section = getsect(p))
					goto argtest;
			}
			break;
		/*
		 * old isn't really a separate section of the manual,
		 * and its entries are all in a single directory.
		 */
		case 'o':				/* old */
			for (p = *argv; isalpha(*p); ++p);
			if (!strncmp(*argv, "o", p - *argv) ||
			    !strncmp(*argv, "old", p - *argv)) {
				list3[0] = list1[3];
				section = list3;
				goto argtest;
			}
			break;
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8':
			if (!(section = getsect(*argv)))
				break;
argtest:		if (!*++argv) {
				fprintf(stderr, "man: what do you want from the %s section of the manual?\n", section->msg);
				exit(1);
			}
		}

		if (section)
			res = manual(section, *argv);
		else {
			res = manual(list1, *argv);
			res += manual(list2, *argv);
		}
		if (!res && !where) {
			if (manpath == locpath)
				if (section)
					fprintf(stderr, "No entry for %s in the %s section of the local manual.\n", *argv, section->msg);
				else
					fprintf(stderr, "No entry for %s in the local manual.\n", *argv);
			else if (manpath == newpath)
				if (section)
					fprintf(stderr, "No entry for %s in the %s section of the new manual.\n", *argv, section->msg);
				else
					fprintf(stderr, "No entry for %s in the new manual.\n", *argv);
			else if (section)
				fprintf(stderr, "No entry for %s in the %s section of the manual.\n", *argv, section->msg);
			else
				fprintf(stderr, "No entry for %s in the manual.\n", *argv);
			exit(1);
		}
	}
}

/*
 * manual --
 *	given a directory list and a file name find a file that
 *	matches; check ${directory}/${dir}/{file name} and
 *	${directory}/${dir}/${machine}/${file name}.
 */
static
manual(section, name)
	DIR *section;
	char *name;
{
	register char *beg, *end;
	register DIR *dp;
	register int res;
	char fname[MAXPATHLEN + 1], *index();

	for (beg = manpath, res = 0;; beg = end + 1) {
		if (end = index(beg, ':'))
			*end = '\0';
		for (dp = section; dp->name; ++dp) {
			(void)sprintf(fname, "%s/%s/%s.0", beg, dp->name, name);
			if (access(fname, R_OK)) {
				(void)sprintf(fname, "%s/%s/%s/%s.0", beg,
				    dp->name, machine, name);
				if (access(fname, R_OK))
					continue;
			}
			if (where)
				printf("man: found in %s.\n", fname);
			else if (nomore)
				cat(fname);
			else
				add(fname);
			res = 1;
		}
		if (!end)
			return(res);
		*end = ':';
	}
	/*NOTREACHED*/
}

/*
 * cat --
 *	cat out the file
 */
static
cat(fname)
	char *fname;
{
	register int fd, n;
	char buf[BUFSIZ];

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

/*
 * add --
 *	add a file name to the list for future paging
 */
static
add(fname)
	char *fname;
{
	static u_int buflen;
	static int len;
	static char *cp;
	int flen;
	char *malloc(), *realloc(), *strcpy();

	if (!command) {
		if (!(command = malloc(buflen = 1024))) {
			fputs("man: out of space.\n", stderr);
			exit(1);
		}
		len = strlen(strcpy(command, pager));
		cp = command + len;
	}
	flen = strlen(fname);
	if (len + flen + 2 > buflen) {		/* +2 == space, EOS */
		if (!(command = realloc(command, buflen += 1024))) {
			fputs("man: out of space.\n", stderr);
			exit(1);
		}
		cp = command + len;
	}
	*cp++ = ' ';
	len += flen + 1;			/* +1 = space */
	(void)strcpy(cp, fname);
	cp += flen;
}

/*
 * getsect --
 *	return a point to the section structure for a particular suffix
 */
static DIR *
getsect(s)
	char *s;
{
	switch(*s++) {
	case '1':
		if (!*s)
			return(list1);
		break;
	case '2':
		if (!*s) {
			list3[0] = list2[0];
			return(list3);
		}
		break;
	/* sect. 3 requests are for either section 3, or section 3[fF]. */
	case '3':
		if (!*s) {
			list3[0] = list2[1];
			return(list3);
		}
		else if ((*s == 'f'  || *s == 'F') && !*++s) {
			list3[0] = list2[5];
			return(list3);
		}
		break;
	case '4':
		if (!*s) {
			list3[0] = list2[2];
			return(list3);
		}
		break;
	case '5':
		if (!*s) {
			list3[0] = list2[3];
			return(list3);
		}
		break;
	case '6':
		if (!*s) {
			list3[0] = list1[2];
			return(list3);
		}
		break;
	case '7':
		if (!*s) {
			list3[0] = list2[4];
			return(list3);
		}
		break;
	case '8':
		if (!*s) {
			list3[0] = list1[1];
			return(list3);
		}
	}
	return((DIR *)NULL);
}
