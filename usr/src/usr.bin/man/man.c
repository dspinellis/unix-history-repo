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
static char sccsid[] = "@(#)man.c	5.19 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include "pathnames.h"

extern int errno;

char *command, *machine, *p_augment, *p_path, *pager, *progname;
int f_all, f_cat, f_where;

main(argc, argv)
	int argc;
	register char **argv;
{
	extern char *optarg;
	extern int optind;
	int ch;
	char *check_pager(), *config(), *getenv();

	progname = "man";
	while ((ch = getopt(argc, argv, "-acfkM:m:P:w")) != EOF)
		switch((char)ch) {
		case 'a':
			f_all = 1;
			break;
		case 'c':
		case '-':		/* deprecated */
			f_cat = 1;
			break;
		case 'm':
			p_augment = optarg;
			break;
		case 'M':
		case 'P':		/* backward compatibility */
			p_path = optarg;
			break;
			/*
			 * "man -f" and "man -k" are backward compatible,
			 * undocumented ways of calling whatis(1) and
			 * apropos(1).
			 */
		case 'f':
			jump(argv, "-f", "whatis");
			/* NOTREACHED */
		case 'k':
			jump(argv, "-k", "apropos");
			/* NOTREACHED */
		case 'w':
			f_all = f_where = 1;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;

	if (!*argv)
		usage();

	if (!f_cat)
		if (!isatty(1))
			f_cat = 1;
		else if (pager = getenv("PAGER"))
			pager = check_pager(pager);
		else
			pager = _PATH_PAGER;

	if (!(machine = getenv("MACHINE")))
		machine = MACHINE;

	if (!p_path && !(p_path = getenv("MANPATH")))
		p_path = config();

	man(argv);

	/* use system(3) in case someone's pager is "pager arg1 arg2" */
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

/*
 * man --
 *	main loop to find the manual page and print it out.
 */
man(argv)
	char **argv;
{
	DIR *section, *getsect();
	int res;

	for (; *argv; ++argv) {
		section = isdigit(**argv) ? getsect(*argv++) : NULL;
		if (*argv) {
			if (p_augment)
				if (section)
					res = manual(p_augment, section, *argv);
				else {
					res = manual(p_augment, list1, *argv);
					if (!res || f_all)
						res += manual(p_augment, list2,
						    *argv);
				}
			if (p_path)
				if (section)
					res = manual(p_path, section, *argv);
				else {
					res = manual(p_path, list1, *argv);
					if (!res || f_all)
						res += manual(p_path, list2,
						    *argv);
				}
			if (res || f_where)
				continue;
			(void)fprintf(stderr,
			    "man: no entry for %s in the ", *argv);
		} else
			(void)fprintf(stderr,
			    "man: what do you want from the ");
		if (section)
			(void)fprintf(stderr,
			    "%s section of the ", section->msg);
		if (*argv)
			(void)fprintf(stderr, "manual.\n");
		else
			(void)fprintf(stderr, "manual?\n");
		exit(1);
	}
}

/*
 * manual --
 *	given a path, a directory list and a file name, find a file
 *	that matches; check ${directory}/${dir}/{file name} and
 *	${directory}/${dir}/${machine}/${file name}.
 */
manual(path, section, name)
	char *path, *name;
	DIR *section;
{
	register char *end;
	register DIR *dp;
	register int res;
	char fname[MAXPATHLEN + 1];

	for (res = 0;; path = end + 1) {
		if (end = index(path, ':'))
			*end = '\0';
		for (dp = section; dp->name; ++dp) {
			(void)sprintf(fname, "%s/%s/%s.0",
			    path, dp->name, name);
			if (access(fname, R_OK)) {
				(void)sprintf(fname, "%s/%s/%s/%s.0", path,
				    dp->name, machine, name);
				if (access(fname, R_OK))
					continue;
			}
			if (f_where)
				(void)printf("man: found in %s.\n", fname);
			else if (f_cat)
				cat(fname);
			else
				add(fname);
			if (!f_all)
				return(1);
			res = 1;
		}
		if (!end)
			return(res);
		*end = ':';
	}
	/* NOTREACHED */
}

/*
 * cat --
 *	cat out the file
 */
cat(fname)
	char *fname;
{
	register int fd, n;
	char buf[BUFSIZ];

	if (!(fd = open(fname, O_RDONLY, 0))) {
		(void)fprintf(stderr, "man: %s: %s\n", fname, strerror(errno));
		exit(1);
	}
	while ((n = read(fd, buf, sizeof(buf))) > 0)
		if (write(1, buf, n) != n) {
			(void)fprintf(stderr,
			    "man: write: %s\n", strerror(errno));
			exit(1);
		}
	if (n == -1) {
		(void)fprintf(stderr, "man: read: %s\n", strerror(errno));
		exit(1);
	}
	(void)close(fd);
}

/*
 * add --
 *	add a file name to the list for future paging
 */
add(fname)
	char *fname;
{
	static u_int buflen;
	static int len;
	static char *cp;
	int flen;
	char *malloc(), *realloc(), *strcpy();

	if (!command) {
		if (!(command = malloc(buflen = 1024)))
			enomem();
		len = strlen(strcpy(command, pager));
		cp = command + len;
	}
	flen = strlen(fname);
	if (len + flen + 2 > buflen) {		/* +2 == space, EOS */
		if (!(command = realloc(command, buflen += 1024)))
			enomem();
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
DIR *
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
		break;
	}
	(void)fprintf(stderr, "man: unknown manual section.\n");
	exit(1);
	/* NOTREACHED */
}

/*
 * check_pager --
 *	check the user supplied page information
 */
char *
check_pager(name)
	char *name;
{
	register char *p;
	char *save, *malloc();

	/*
	 * if the user uses "more", we make it "more -s"; watch out for
	 * PAGER = "mypager /usr/ucb/more"
	 */
	for (p = name; *p && !isspace(*p); ++p);
	for (; p > name && *p != '/'; --p);
	if (p != name)
		++p;

	/* make sure it's "more", not "morex" */
	if (!strncmp(p, "more", 4) && (!p[4] || isspace(p[4]))){
		save = name;
		/* allocate space to add the "-s" */
		if (!(name =
		    malloc((u_int)(strlen(save) + sizeof("-s") + 1))))
			enomem();
		(void)sprintf(name, "%s %s", save, "-s");
	}
	return(name);
}

/*
 * jump --
 *	strip out flag argument and jump
 */
jump(argv, flag, name)
	char **argv, *name;
	register char *flag;
{
	register char **arg;

	argv[0] = name;
	for (arg = argv + 1; *arg; ++arg)
		if (!strcmp(*arg, flag))
			break;
	for (; *arg; ++arg)
		arg[0] = arg[1];
	execvp(name, argv);
	fprintf(stderr, "%s: Command not found.\n", name);
	exit(1);
}

/*
 * usage --
 *	print usage message and die
 */
usage()
{
	(void)fprintf(stderr,
	    "usage: man [-ac] [-M path] [-m path] [section] title ...\n");
	exit(1);
}
