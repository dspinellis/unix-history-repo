/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ps.c	5.39 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/user.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/proc.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/kinfo.h>
#include <kvm.h>
#include <errno.h>
#include <unistd.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <paths.h>
#include "ps.h"

#ifdef SPPWAIT
#define NEWVM
#endif

KINFO *kinfo;
struct varent *vhead, *vtail;

int	eval;			/* exit value */
int	rawcpu;			/* -C */
int	sumrusage;		/* -S */
int	termwidth;		/* width of screen (0 == infinity) */
int	totwidth;		/* calculated width of requested variables */

static int needuser, needcomm;

enum sort { DEFAULT, SORTMEM, SORTCPU } sortby = DEFAULT;

uid_t	getuid();
char	*ttyname();

char dfmt[] = "pid tt state time command";
char jfmt[] = "user pid ppid pgid sess jobc state tt time command";
char lfmt[] = "uid pid ppid cpu pri nice vsz rss wchan state tt time command";
char   o1[] = "pid";
char   o2[] = "tt state time command";
char ufmt[] = "user pid %cpu %mem vsz rss tt state start time command";
char vfmt[] =
	"pid state time sl re pagein vsz rss lim tsiz trs %cpu %mem command";

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register struct proc *p;
	register size_t nentries;
	register struct varent *vent;
	register int i;
	struct winsize ws;
	dev_t ttydev;
	int all, ch, flag, fmt, lineno, pid, prtheader, uid, what, xflg;
	int pscomp();
	char *kludge_oldps_options();

	if ((ioctl(STDOUT_FILENO, TIOCGWINSZ, (char *)&ws) == -1 &&
	     ioctl(STDERR_FILENO, TIOCGWINSZ, (char *)&ws) == -1 &&
	     ioctl(STDIN_FILENO,  TIOCGWINSZ, (char *)&ws) == -1) ||
	     ws.ws_col == 0)
		termwidth = 79;
	else
		termwidth = ws.ws_col - 1;

	if (argc > 1)
		argv[1] = kludge_oldps_options(argv[1]);

	fmt = 0;
	all = xflg = 0;
	pid = uid = -1;
	ttydev = NODEV;
	while ((ch = getopt(argc, argv, "aCghjLlmO:o:p:rSTt:uvwx")) != EOF)
		switch((char)ch) {
		case 'a':
			all = 1;
			break;
		case 'C':
			rawcpu = 1;
			break;
		case 'g':
			break;	/* no-op */
		case 'h':
			prtheader = ws.ws_row > 5 ? ws.ws_row : 22;
			break;
		case 'j':
			parsefmt(jfmt);
			fmt = 1;
			break;
		case 'L': 
			showkey();
			exit(0);
		case 'l':
			parsefmt(lfmt);
			fmt = 1;
			break;
		case 'm':
			sortby = SORTMEM;
			break;
		case 'O':
			parsefmt(o1);
			parsefmt(optarg);
			parsefmt(o2);
			fmt = 1;
			break;
		case 'o':
			parsefmt(optarg);
			fmt = 1;
			break;
		case 'p':
			pid = atoi(optarg);
			xflg = 1;
			break;
		case 'r':
			sortby = SORTCPU;
			break;
		case 'S':
			sumrusage = 1;
			break;
		case 'T':
			if ((optarg = ttyname(STDIN_FILENO)) == NULL)
				error("stdin: not a terminal");
			/* FALLTHROUGH */
		case 't': {
			char *ttypath;
			struct stat stbuf;
			char pathbuf[MAXPATHLEN];

			if (strcmp(optarg, "co") == 0)
				ttypath = _PATH_CONSOLE;
			else if (*optarg != '/')
				(void) sprintf(ttypath = pathbuf, "%s%s",
				    _PATH_TTY, optarg);
			else
				ttypath = optarg;
			if (stat(ttypath, &stbuf) == -1) {
				(void)fprintf(stderr,
				    "ps: %s: %s\n", ttypath, strerror(errno));
				exit(1);
			}
			if (!S_ISCHR(stbuf.st_mode))
				error("%s: not a terminal", ttypath);
			ttydev = stbuf.st_rdev;
			break;
		}
		case 'u':
			parsefmt(ufmt);
			sortby = SORTCPU;
			fmt = 1;
			break;
		case 'v':
			parsefmt(vfmt);
			sortby = SORTMEM;
			fmt = 1;
			break;
		case 'w':
			if (termwidth < 131)
				termwidth = 131;
			else
				termwidth = UNLIMITED;
			break;
		case 'x':
			xflg = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (*argv) {
		char *nlistf, *memf = NULL, *swapf = NULL;

		nlistf = *argv++;
		if (*argv) {
			memf = *argv++;
			if (*argv)
				swapf = *argv++;
		}
		if (kvm_openfiles(nlistf, memf, swapf) == -1)
			error("kvm_openfiles: %s", kvm_geterr());
	}

	if (!fmt)
		parsefmt(dfmt);

	if (!all && ttydev == NODEV && pid == -1)  /* XXX - should be cleaner */
		uid = getuid();

	/*
	 * scan requested variables, noting what structures are needed,
	 * and adjusting header widths as appropiate.
	 */
	scanvars();
	/*
	 * get proc list
	 */
	if (uid != -1) {
		what = KINFO_PROC_UID;
		flag = uid;
	} else if (ttydev != NODEV) {
		what = KINFO_PROC_TTY;
		flag = ttydev;
	} else if (pid != -1) {
		what = KINFO_PROC_PID;
		flag = pid;
	/*
	 * select procs
	 */
	if ((nentries = kvm_getprocs(what, flag)) == -1) {
		(void) fprintf(stderr, "ps: %s\n", kvm_geterr());
		exit(1);
	}
	kinfo = (KINFO *)malloc(nentries * sizeof(KINFO));
	if (kinfo == NULL) {
		(void)fprintf(stderr, "ps: %s\n", strerror(ENOMEM));
		exit(1);
	}
	for (nentries = 0; p = kvm_nextproc(); ++nentries) {
		kinfo[nentries].ki_p = p;
		kinfo[nentries].ki_e = kvm_geteproc(p);
		if (needuser)
			saveuser(&kinfo[nentries]);
	}
	/*
	 * print header
	 */
	printheader();
	if (nentries == 0)
		exit(0);
	/*
	 * sort proc list
	 */
	qsort((void *)kinfo, nentries, sizeof(KINFO), pscomp);
	/*
	 * for each proc, call each variable output function.
	 */
	for (i = lineno = 0; i < nentries; i++) {
		if (xflg == 0 && (kinfo[i].ki_e->e_tdev == NODEV ||
		    (kinfo[i].ki_p->p_flag & SCTTY ) == 0))
			continue;
		for (vent = vhead; vent; vent = vent->next) {
			(*vent->var->oproc)(&kinfo[i], vent->var, vent->next);
			if (vent->next != NULL)
				(void) putchar(' ');
		}
		(void) putchar('\n');
		if (prtheader && lineno++ == prtheader-4) {
			(void) putchar('\n');
			printheader();
			lineno = 0;
		}
	}
	exit(eval);
}

scanvars()
{
	register struct varent *vent;
	register VAR *v;
	register int i;

	for (vent = vhead; vent; vent = vent->next) {
		v = vent->var;
		i = strlen(v->header);
		if (v->width < i)
			v->width = i;
		totwidth += v->width + 1;	/* +1 for space */
		if (v->flag & USER)
			needuser = 1;
		if (v->flag & COMM)
			needcomm = 1;
	}
	totwidth--;
}


/* XXX - redo */
saveuser(ki)
	KINFO *ki;
{
	register struct usave *usp;
	register struct user *up;

	if ((usp = (struct usave *)calloc(1, sizeof(struct usave))) == NULL) {
		(void)fprintf(stderr, "ps: %s\n", strerror(errno));
		exit(1);
	}
	ki->ki_u = usp;
	up = kvm_getu(ki->ki_p);
	/*
	 * save arguments if needed
	 */
	ki->ki_args = needcomm ? strdup(kvm_getargs(ki->ki_p, up)) : NULL;
	if (up != NULL) {
		/*
		 * save important fields
		 */
#ifdef NEWVM
		usp->u_start = up->u_stats.p_start;
		usp->u_ru = up->u_stats.p_ru;
		usp->u_cru = up->u_stats.p_cru;
#else
		usp->u_procp = up->u_procp;
		usp->u_start = up->u_start;
		usp->u_ru = up->u_ru;
		usp->u_cru = up->u_cru;
		usp->u_acflag = up->u_acflag;
#endif
	}
}

pscomp(k1, k2)
	KINFO *k1, *k2;
{
	int i;
#ifdef NEWVM
#define VSIZE(k) ((k)->ki_e->e_vm.vm_dsize + (k)->ki_e->e_vm.vm_ssize + \
		  (k)->ki_e->e_vm.vm_tsize)
#else
#define VSIZE(k) ((k)->ki_p->p_dsize + (k)->ki_p->p_ssize + (k)->ki_e->e_xsize)
#endif

	if (sortby == SORTCPU)
		return (getpcpu(k2) - getpcpu(k1));
	if (sortby == SORTMEM)
		return (VSIZE(k2) - VSIZE(k1));
	i =  k1->ki_e->e_tdev - k2->ki_e->e_tdev;
	if (i == 0)
		i = k1->ki_p->p_pid - k2->ki_p->p_pid;
	return (i);
}

/*
 * ICK (all for getopt), would rather hide the ugliness
 * here than taint the main code.
 *
 *  ps foo -> ps -foo
 *  ps 34 -> ps -p34
 *
 * The old convention that 't' with no trailing tty arg means the users
 * tty, is only supported if argv[1] doesn't begin with a '-'.  This same
 * feature is available with the option 'T', which takes no argument.
 */
char *
kludge_oldps_options(s)
	char *s;
{
	size_t len;
	char *newopts, *ns, *cp;

	len = strlen(s);
	if ((newopts = ns = malloc(len + 2)) == NULL) {
		(void)fprintf(stderr, "ps: %s\n", strerror(errno));
		exit(1);
	}
	/*
	 * options begin with '-'
	 */
	if (*s != '-')
		*ns++ = '-';	/* add option flag */
	/*
	 * gaze to end of argv[1]
	 */
	cp = s + len - 1;
	/*
	 * if last letter is a 't' flag with no argument (in the context
	 * of the oldps options -- option string NOT starting with a '-' --
	 * then convert to 'T' (meaning *this* terminal, i.e. ttyname(0)).
	 */
	if (*cp == 't' && *s != '-')
		*cp = 'T';
	else {
		/*
		 * otherwise check for trailing number, which *may* be a
		 * pid.
		 */
		while (cp >= s && isdigit(*cp))
			--cp;
	}
	cp++;
	bcopy(s, ns, (size_t)(cp - s));	/* copy up to trailing number */
	ns += cp - s;
	/*
	 * if there's a trailing number, and not a preceding 'p' (pid) or
	 * 't' (tty) flag, then assume it's a pid and insert a 'p' flag.
	 */
	if (isdigit(*cp) && (cp == s || cp[-1] != 't' && cp[-1] != 'p' &&
	    (cp - 1 == s || cp[-2] != 't')))
		*ns++ = 'p';
	(void) strcpy(ns, cp);		/* and append the number */

	return (newopts);
}

#ifdef lint
/* VARARGS1 */
error(fmt) char *fmt; { (void) fputs(fmt, stderr); exit(1); /* NOTREACHED */ }
#else
error(fmt)
	char *fmt;
{
	va_list ap;

	va_start(ap, fmt);
	(void) fprintf(stderr, "ps: ");
	(void) vfprintf(stderr, fmt, ap);
	(void) fprintf(stderr, "\n");
	va_end(ap);
	exit(1);
}
#endif

usage()
{
	(void) fprintf(stderr,
	    "usage:\tps [ -aChjlmrSsTuvwx ] [ -O|o fmt ] [ -p pid ] [ -t tty ] [ system ] [ core ] [ swap ]\n\t ps [ -L ]\n");
	exit(1);
}
