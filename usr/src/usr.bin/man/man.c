/*
 * Copyright (c) 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1987, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)man.c	8.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/queue.h>

#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <fnmatch.h>
#include <glob.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "config.h"
#include "pathnames.h"

int f_all, f_where;

static void	 build_page __P((char *, char *));
static void	 cat __P((char *));
static char	*check_pager __P((char *));
static void	 cleanup __P((void));
static void	 how __P((char *));
static void	 jump __P((char **, char *, char *));
static int	 manual __P((char *, ENTRY *, glob_t *));
static void	 onsig __P((int));
static void	 usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	ENTRY *defp, *defnewp, *intmpp;
	ENTRY *section, *sectp, *sectnewp, *subp, *tp;
	glob_t pg;
	size_t len;
	int ch, f_cat, f_how, found;
	char **ap, *cmd, *machine, *p, *p_add, *p_path, *pager, *slashp;
	char *conffile, buf[MAXPATHLEN * 2];

	conffile = NULL;
	f_cat = f_how = 0;
	while ((ch = getopt(argc, argv, "-aC:cfhkM:m:P:w")) != EOF)
		switch (ch) {
		case 'a':
			f_all = 1;
			break;
		case 'C':
			conffile = optarg;
			break;
		case 'c':
		case '-':		/* Deprecated. */
			f_cat = 1;
			break;
		case 'h':
			f_how = 1;
			break;
		case 'm':
			p_add = optarg;
			break;
		case 'M':
		case 'P':		/* Backward compatibility. */
			p_path = optarg;
			break;
		/*
		 * The -f and -k options are backward compatible,
		 * undocumented ways of calling whatis(1) and apropos(1).
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
	argc -= optind;
	argv += optind;

	if (!*argv)
		usage();

	if (!f_cat && !f_how)
		if (!isatty(1))
			f_cat = 1;
		else if (pager = getenv("PAGER"))
			pager = check_pager(pager);
		else
			pager = _PATH_PAGER;

	/* Read the configuration file. */
	config(conffile);

	/* If there's no _default list, create an empty one. */
	if ((defp = getlist("_default")) == NULL)
		defp = addlist("_default");

	/* Get the machine type. */
	if ((machine = getenv("MACHINE")) == NULL)
		machine = MACHINE;

	/*
	 * 1: If the user specified a MANPATH variable, or set the -M
	 *    option, we replace the _default list with the user's list,
	 *    appending the entries in the _subdir list and the machine.
	 */
	if (p_path == NULL)
		p_path = getenv("MANPATH");
	if (p_path != NULL) {
		while ((tp = defp->list.qe_next) != NULL) {
			free(tp->s);
			queue_remove(&defp->list, tp, ENTRY *, list);
		}
		for (p = strtok(p_path, ":");
		    p != NULL; p = strtok(NULL, ":")) {
			slashp = p[strlen(p) - 1] == '/' ? "" : "/";
			subp = getlist("_subdir");
			if (subp != NULL)
				subp = subp->list.qe_next;
			for (; subp != NULL; subp = subp->list.qe_next) {
				(void)snprintf(buf, sizeof(buf), "%s%s%s{/%s,}",
				    p, slashp, subp->s, machine);
				if ((tp = malloc(sizeof(ENTRY))) == NULL ||
				    (tp->s = strdup(buf)) == NULL)
					err(1, NULL);
				queue_enter_tail(&defp->list,
				    tp, ENTRY *, list);
			}
		}
	}

	/*
	 * 2: If the user did not specify MANPATH, -M or a section, rewrite
	 *    the _default list to include the _subdir list and the machine.
	 */
	if ((section = getlist(*argv)) != NULL)
		++argv;
	if (p_path == NULL && section == NULL) {
		defnewp = addlist("_default_new");
		if (defp->list.qe_next != NULL)
			defp = defp->list.qe_next;
		for (; defp != NULL; defp = defp->list.qe_next) {
			slashp = defp->s[strlen(defp->s) - 1] == '/' ? "" : "/";
			subp = getlist("_subdir");
			if (subp != NULL)
				subp = subp->list.qe_next;
			for (; subp != NULL; subp = subp->list.qe_next) {
				(void)snprintf(buf, sizeof(buf), "%s%s%s{/%s,}",
				defp->s, slashp, subp->s, machine);
				if ((tp = malloc(sizeof(ENTRY))) == NULL ||
				    (tp->s = strdup(buf)) == NULL)
					err(1, NULL);
				queue_enter_tail(&defnewp->list,
				    tp, ENTRY *, list);
			}
		}
		defp = getlist("_default");
		while ((tp = defp->list.qe_next) != NULL) {
			free(tp->s);
			queue_remove(&defp->list, tp, ENTRY *, list);
		}
		free(defp->s);
		queue_remove(&defp->tags, defp, ENTRY *, tags);
		defnewp = getlist("_default_new");
		free(defnewp->s);
		defnewp->s = "_default";
		defp = defnewp;
	}

	/*
	 * 3: If the user set the -m option, insert the user's list before
	 *    whatever list we have, again appending the _subdir list and
	 *    the machine.
	 */
	if (p_add != NULL)
		for (p = strtok(p_add, ":"); p != NULL; p = strtok(NULL, ":")) {
			slashp = p[strlen(p) - 1] == '/' ? "" : "/";
			subp = getlist("_subdir");
			if (subp != NULL)
				subp = subp->list.qe_next;
			for (; subp != NULL; subp = subp->list.qe_next) {
				(void)snprintf(buf, sizeof(buf), "%s%s%s{/%s,}",
				    p, slashp, subp->s, machine);
				if ((tp = malloc(sizeof(ENTRY))) == NULL ||
				    (tp->s = strdup(buf)) == NULL)
					err(1, NULL);
				queue_enter_head(&defp->list,
				    tp, ENTRY *, list);
			}
		}

	/*
	 * 4: If none of MANPATH, -M, or -m were specified, and a section was,
	 *    rewrite the section's paths (if they have a trailing slash) to
	 *    append the _subdir list and the machine.  This then becomes the
	 *    _default list.
	 */
	if (p_path == NULL && p_add == NULL && section != NULL) {
		sectnewp = addlist("_section_new");
		if ((sectp = section)->list.qe_next != NULL)
			sectp = sectp->list.qe_next;
		for (; sectp != NULL; sectp = sectp->list.qe_next) {
			if (sectp->s[strlen(sectp->s) - 1] != '/') {
				(void)snprintf(buf, sizeof(buf),
				    "%s{/%s,}", sectp->s, machine);
				if ((tp = malloc(sizeof(ENTRY))) == NULL ||
				    (tp->s = strdup(buf)) == NULL)
					err(1, NULL);
				queue_enter_tail(&sectnewp->list,
				    tp, ENTRY *, list);
				continue;
			}
			subp = getlist("_subdir");
			if (subp != NULL)
				subp = subp->list.qe_next;
			for (; subp != NULL; subp = subp->list.qe_next) {
				(void)snprintf(buf, sizeof(buf),
				    "%s%s{/%s,}", sectp->s, subp->s, machine);
				if ((tp = malloc(sizeof(ENTRY))) == NULL ||
				    (tp->s = strdup(buf)) == NULL)
					err(1, NULL);
				queue_enter_tail(&sectnewp->list,
				    tp, ENTRY *, list);
			}
		}
		sectnewp->s = section->s;
		defp = sectnewp;
		queue_remove(&section->tags, section, ENTRY *, tags);
	}

	/*
	 * 5: Search for the files.  Set up an interrupt handler, so the
	 *    temporary files go away.
	 */
	(void)signal(SIGINT, onsig);

	memset(&pg, 0, sizeof(pg));
	for (found = 0; *argv; ++argv)
		if (manual(*argv, defp, &pg))
			found = 1;

	/*
	 * 7: If nothing found, we're done.
	 */
	if (!found) {
		cleanup();
		exit (1);
	}

	/* 8: If it's simple, display it fast. */
	if (f_cat) {
		found = 0;
		for (ap = pg.gl_pathv; *ap != NULL; ++ap) {
			if (*ap == '\0')
				continue;
			cat(*ap);
			if (!f_all) {
				found = 1;
				break;
			}
		}
		if (!found) {
			if (intmpp != NULL)
				intmpp = intmpp->list.qe_next;
			for (; intmpp != NULL; intmpp = intmpp->list.qe_next) {
				cat(intmpp->s);
				if (!f_all)
					break;
			}
		}
		cleanup();
		exit (0);
	}
	if (f_how) {
		found = 0;
		for (ap = pg.gl_pathv; *ap != NULL; ++ap) {
			if (*ap == '\0')
				continue;
			how(*ap);
			if (!f_all) {
				found = 1;
				break;
			}
		}
		if (!found) {
			intmpp = getlist("_intmp");
			if (intmpp != NULL)
				intmpp = intmpp->list.qe_next;
			for (; intmpp != NULL; intmpp = intmpp->list.qe_next) {
				how(intmpp->s);
				if (!f_all)
					break;
			}
		}
		cleanup();
		exit (0);
	}
	if (f_where) {
		for (ap = pg.gl_pathv; *ap != NULL; ++ap) {
			if (*ap == '\0')
				continue;
			(void)printf("%s\n", *ap);
		}
		intmpp = getlist("_intmp");
		if (intmpp != NULL)
			intmpp = intmpp->list.qe_next;
		for (; intmpp != NULL; intmpp = intmpp->list.qe_next)
			(void)printf("%s\n", intmpp->s);
		cleanup();
		exit (0);
	}
		
	/*
	 * 9: We display things in a single command; build a list of things
	 *    to display.
	 */
	found = 0;
	for (ap = pg.gl_pathv, len = strlen(pager) + 1; *ap != NULL; ++ap) {
		if (**ap == '\0')
			continue;
		len += strlen(*ap) + 1;
		if (!f_all) {
			found = 1;
			break;
		}
	}
	if (!found) {
		intmpp = getlist("_intmp");
		if (intmpp != NULL)
			intmpp = intmpp->list.qe_next;
		for (; intmpp != NULL; intmpp = intmpp->list.qe_next) {
			len += strlen(intmpp->s);
			if (!f_all)
				break;
		}
	}

	if ((cmd = malloc(len)) == NULL) {
		cleanup();
		err(1, NULL);
	}
	p = cmd;
	len = strlen(pager);
	memmove(p, pager, len);
	p += len;
	*p++ = ' ';
	found = 0;
	for (ap = pg.gl_pathv; *ap != NULL; ++ap) {
		if (**ap == '\0')
			continue;
		len = strlen(*ap);
		memmove(p, *ap, len);
		p += len;
		*p++ = ' ';
		if (!f_all) {
			found = 1;
			break;
		}
	}
	if (!found) {
		intmpp = getlist("_intmp");
		if (intmpp != NULL)
			intmpp = intmpp->list.qe_next;
		for (; intmpp != NULL; intmpp = intmpp->list.qe_next) {
			len = strlen(intmpp->s);
			memmove(p, intmpp->s, len);
			p += len;
			*p++ = ' ';
		}
	}
	*p = '\0';

	/* Use system(3) in case someone's pager is "pager arg1 arg2". */
	(void)system(cmd);

	cleanup();
	exit(0);
}

/*
 * manual --
 *	Search the manuals for the pages.
 */
static int
manual(page, list, pg)
	char *page;
	ENTRY *list;
	glob_t *pg;
{
	ENTRY *listp, *missp, *sufp, *tp;
	int anyfound, cnt, found;
	char *p, buf[128];

	anyfound = 0;
	buf[0] = '*';

	/* For each element in the list... */
	if (list != NULL)
		list = list->list.qe_next;
	for (listp = list; listp != NULL; listp = listp->list.qe_next) {
		(void)snprintf(buf, sizeof(buf), "%s/%s.*", listp->s, page);
		if (glob(buf,
		    GLOB_APPEND | GLOB_BRACE | GLOB_NOSORT | GLOB_QUOTE,
		    NULL, pg)) {
			cleanup();
			err(1, "globbing");
		}
		if (pg->gl_matchc == 0)
			continue;

		/* Find out if it's really a man page. */
		for (cnt = 1; cnt <= pg->gl_matchc; ++cnt) {

			/*
			 * Try the _suffix key words first.
			 *
			 * XXX
			 * Older versions of man.conf didn't have the suffix
			 * key words, it was assumed that everything was a .0.
			 * We just test for .0 first, it's fast and probably
			 * going to hit.
			 */
			if (!fnmatch("*.0",
			    pg->gl_pathv[pg->gl_pathc - cnt], 0))
				goto easy;

			sufp = getlist("_suffix");
			if (sufp != NULL)
				sufp = sufp->list.qe_next;
			for (found = 0;
			    sufp != NULL; sufp = sufp->list.qe_next) {
				(void)snprintf(buf,
				     sizeof(buf), "*%s", sufp->s);
				if (!fnmatch(buf,
				    pg->gl_pathv[pg->gl_pathc - cnt], 0)) {
					found = 1;
					break;
				}
			}
			if (found) {
easy:				anyfound = 1;
				if (!f_all)
					break;
				continue;
			}

			/* Try the _build key words next. */
			sufp = getlist("_build");
			if (sufp != NULL)
				sufp = sufp->list.qe_next;
			for (found = 0;
			    sufp != NULL; sufp = sufp->list.qe_next) {
				for (p = sufp->s;
				    *p != '\0' && !isspace(*p); ++p);
				if (*p == '\0')
					continue;
				*p = '\0';
				(void)snprintf(buf,
				     sizeof(buf), "*%s", sufp->s);
				if (!fnmatch(buf,
				    pg->gl_pathv[pg->gl_pathc - cnt], 0)) {
					if (!f_where) {
						build_page(p + 1,
						    pg->gl_pathv[pg->gl_pathc -
						    cnt]);
						pg->gl_pathv[pg->gl_pathc -
						    cnt] = "";
					}
					*p = ' ';
					found = 1;
					break;
				}
				*p = ' ';
			}
			if (found) {
				anyfound = 1;
				if (!f_all)
					break;
				continue;
			}

			/* It's not a man page, forget about it. */
			pg->gl_pathv[pg->gl_pathc - cnt] = "";
		}

		if (anyfound && !f_all)
			break;
	}

	/* If not found, enter onto the missing list. */
	if (!anyfound) {
		if ((missp = getlist("_missing")) == NULL)
			missp = addlist("_missing");
		if ((tp = malloc(sizeof(ENTRY))) == NULL ||
		    (tp->s = strdup(page)) == NULL) {
			cleanup();
			err(1, NULL);
		}
		queue_enter_tail(&missp->list, tp, ENTRY *, list);
	}
	return (anyfound);
}

/* 
 * build_page --
 *	Build a man page for display.
 */
static void
build_page(fmt, path)
	char *fmt, *path;
{
	static int warned;
	ENTRY *intmpp, *tp;
	int fd;
	char buf[MAXPATHLEN], cmd[MAXPATHLEN], tpath[sizeof(_PATH_TMP)];

	/* Let the user know this may take awhile. */
	if (!warned) {
		warned = 1;
		warnx("Formatting manual page...");
	}

	/* Add an "in tmp" list. */
	if ((intmpp = getlist("_intmp")) == NULL)
		intmpp = addlist("_intmp");

	/* Move to the printf(3) format string. */
	for (; *fmt && isspace(*fmt); ++fmt);

	/*
	 * Get a temporary file and build a version of the file to display.
	 * Link the built file into the list.
	 */
	(void)strcpy(tpath, _PATH_TMP);
	if ((fd = mkstemp(tpath)) == -1) {
		cleanup();
		err(1, "%s", tpath);
	}
	(void)snprintf(buf, sizeof(buf), "%s > %s", fmt, tpath);
	(void)snprintf(cmd, sizeof(cmd), buf, path);
	(void)system(cmd);
	(void)close(fd);
	if ((tp = malloc(sizeof(ENTRY))) == NULL ||
	    (tp->s = strdup(tpath)) == NULL) {
		cleanup();
		err(1, NULL);
	}
	queue_enter_tail(&intmpp->list, tp, ENTRY *, list);
}

/*
 * how --
 *	display how information
 */
static void
how(fname)
	char *fname;
{
	register FILE *fp;

	register int lcnt, print;
	register char *p;
	char buf[BUFSIZ];

	if (!(fp = fopen(fname, "r"))) {
		cleanup();
		err(1, "%s", fname);
	}
#define	S1	"SYNOPSIS"
#define	S2	"S\bSY\bYN\bNO\bOP\bPS\bSI\bIS\bS"
#define	D1	"DESCRIPTION"
#define	D2	"D\bDE\bES\bSC\bCR\bRI\bIP\bPT\bTI\bIO\bON\bN"
	for (lcnt = print = 0; fgets(buf, sizeof(buf), fp);) {
		if (!strncmp(buf, S1, sizeof(S1) - 1) ||
		    !strncmp(buf, S2, sizeof(S2) - 1)) {
			print = 1;
			continue;
		} else if (!strncmp(buf, D1, sizeof(D1) - 1) ||
		    !strncmp(buf, D2, sizeof(D2) - 1))
			return;
		if (!print)
			continue;
		if (*buf == '\n')
			++lcnt;
		else {
			for(; lcnt; --lcnt)
				(void)putchar('\n');
			for (p = buf; isspace(*p); ++p);
			(void)fputs(p, stdout);
		}
	}
	(void)fclose(fp);
}

/*
 * cat --
 *	cat out the file
 */
static void
cat(fname)
	char *fname;
{
	register int fd, n;
	char buf[BUFSIZ];

	if ((fd = open(fname, O_RDONLY, 0)) < 0) {
		cleanup();
		err(1, "%s", fname);
	}
	while ((n = read(fd, buf, sizeof(buf))) > 0)
		if (write(STDOUT_FILENO, buf, n) != n) {
			cleanup();
			err(1, "write");
		}
	if (n == -1) {
		cleanup();
		err(1, "read");
	}
	(void)close(fd);
}

/*
 * check_pager --
 *	check the user supplied page information
 */
static char *
check_pager(name)
	char *name;
{
	register char *p;
	char *save;

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
			err(1, NULL);
		(void)sprintf(name, "%s %s", save, "-s");
	}
	return(name);
}

/*
 * jump --
 *	strip out flag argument and jump
 */
static void
jump(argv, flag, name)
	char **argv, *flag, *name;
{
	char **arg;

	argv[0] = name;
	for (arg = argv + 1; *arg; ++arg)
		if (!strcmp(*arg, flag))
			break;
	for (; *arg; ++arg)
		arg[0] = arg[1];
	execvp(name, argv);
	(void)fprintf(stderr, "%s: Command not found.\n", name);
	exit(1);
}

/* 
 * onsig --
 *	If signaled, delete the temporary files.
 */
static void
onsig(signo)
	int signo;
{
	cleanup();

	(void)signal(SIGINT, SIG_DFL);
	(void)kill(getpid(), SIGINT);
}

/*
 * cleanup --
 *	Clean up temporary files, show any error messages.
 */
static void
cleanup()
{
	ENTRY *intmpp, *missp;
	int sverrno;

	sverrno = errno;

	missp = getlist("_missing");
	if (missp != NULL)
		missp = missp->list.qe_next;
	if (missp != NULL)
		for (; missp != NULL; missp = missp->list.qe_next)
			warnx("no entry for %s in the manual.", missp->s);

	intmpp = getlist("_intmp");
	if (intmpp != NULL)
		intmpp = intmpp->list.qe_next;
	for (; intmpp != NULL; intmpp = intmpp->list.qe_next)
		(void)unlink(intmpp->s);

	errno = sverrno;
}

/*
 * usage --
 *	print usage message and die
 */
static void
usage()
{
	(void)fprintf(stderr,
    "usage: man [-ac] [-C file] [-M path] [-m path] [section] title ...\n");
	exit(1);
}
