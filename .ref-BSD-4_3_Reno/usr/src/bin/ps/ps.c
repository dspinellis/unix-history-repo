/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ps.c	5.28 (Berkeley) 6/26/90";
#endif /* not lint */

#include <machine/pte.h>

#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/vm.h>
#include <sys/text.h>
#include <sys/stat.h>
#include <sys/mbuf.h>
#include <nlist.h>
#include <pwd.h>
#include <math.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <kvm.h>

struct usave {
	struct	proc *u_procp;
	struct	timeval u_start;
	struct	rusage u_ru;
	struct	rusage u_cru;
	short	u_cmask;
	char	u_acflag;
};

/*
 * to compute offset in common structures
 */
#define	POFF(x)		((int)&((struct proc *)0)->x)
#define	EOFF(x)		((int)&((struct eproc *)0)->x)
#define	UOFF(x)		((int)&((struct usave *)0)->x)
#define	ROFF(x)		((int)&((struct rusage *)0)->x)

enum type	{ CHAR, UCHAR, SHORT, USHORT, LONG, ULONG, KPTR };

#define	UIDFMT	"u"
#define UIDLEN	5
#define PIDFMT	"d"
#define PIDLEN	5
#define	USERLEN	8

int needuser, needcomm, neednlist;

int 	command(), ucomm(), logname(), pvar(), evar(), uvar(), rvar(), uname(), 
	runame(), state(), pri(), tdev(), tname(), longtname(), started(),
	lstarted(), wchan(), vsize(), rssize(), p_rssize(), cputime(), 
	pmem(), pcpu(), pagein(), maxrss(), tsize(), trss();
	/**
	utime(), stime(), ixrss(), idrss(), isrss();
	**/

struct	usave *saveuser();
char	*saveargs();

struct var {
	char	*name[8];	/* name(s) of variable */
	char	*header;	/* default header */
	int	flag;
#define	USER	0x01	/* requires user structure */
#define	LJUST	0x02	/* right adjust on output */
#define	COMM	0x04	/* requires exec arguments and environment (XXX) */
#define	NLIST	0x08	/* requires nlist to get extra variables */
	int	(*oproc)();	/* output routine */
	short	width;		/* printing width */
	/*
	 * The following (optional) elements are hooks for passing information
	 * to the generic output routines: pvar, evar, uvar (those which print
	 * simple elements from well known structures: proc, eproc, usave)
	 */
	int	off;		/* offset in structure */
	enum	type type;	/* type of element */
	char	*fmt;		/* printf format */
	/*
	 * glue to link selected fields together
	 */
	struct	var *next;
}  var[] = {
	{{"command", "comm", "args"}, "COMMAND", USER|LJUST|COMM, 
		command, 16}, 
	{{"ucomm"}, "COMMAND",	LJUST, ucomm, MAXCOMLEN},
	{{"logname"}, "LOGNAME", LJUST, logname, MAXLOGNAME},
	{{"flag", "f"}, "F", 0, pvar, 7, POFF(p_flag), LONG, "x"},
	{{"uid"}, "UID", 0, pvar, UIDLEN, POFF(p_uid),USHORT, UIDFMT},
	{{"ruid"}, "RUID", 0, pvar, UIDLEN, POFF(p_ruid), USHORT, UIDFMT},
	{{"svuid"}, "SVUID", 0, pvar, UIDLEN, POFF(p_svuid), USHORT, UIDFMT},
	{{"rgid"}, "RGID", 0, pvar, UIDLEN, POFF(p_rgid), USHORT, UIDFMT},
	{{"svgid"}, "SVGID", 0, pvar, UIDLEN, POFF(p_svgid), USHORT, UIDFMT},
	{{"pid"}, "PID", 0, pvar, PIDLEN, POFF(p_pid),SHORT, PIDFMT},
	{{"ppid"}, "PPID", 0, pvar, PIDLEN, POFF(p_ppid), SHORT, PIDFMT},
	{{"cp", "cpu"}, "CP", 0, pvar, 3, POFF(p_cpu), UCHAR, "d"},
	{{"xstat"}, "XSTAT", 0, pvar, 4, POFF(p_xstat), USHORT, "x"},
	{{"poip"}, "POIP", 0, pvar, 4, POFF(p_poip), SHORT, "d"},
	{{"nwchan"}, "WCHAN", 0, pvar, 6, POFF(p_wchan), KPTR, "x"},
	{{"wchan"}, "WCHAN", LJUST, wchan, 6},
	{{"rlink"}, "RLINK", 0, pvar, 8, POFF(p_rlink), KPTR, "x"},
	{{"ktrace", "traceflag"}, "KTRACE",
		0, pvar, 8, POFF(p_traceflag), LONG, "x"},
	{{"ktracep", "tracep"}, "KTRACEP",
		0, pvar, 8, POFF(p_tracep), LONG, "x"},
	{{"sig", "pending"}, "PENDING",
		0, pvar, 8, POFF(p_sig), LONG, "x"},
	{{"sigmask", "blocked"}, "BLOCKED",
		0, pvar, 8, POFF(p_sigmask), LONG, "x"},
	{{"sigignore", "ignored"}, "IGNORED",
		0, pvar, 8, POFF(p_sigignore), LONG, "x"},
	{{"sigcatch", "caught"}, "CAUGHT",
		0, pvar, 8, POFF(p_sigcatch), LONG, "x"},
	{{"user", "uname"}, "USER", LJUST, uname, USERLEN},
	{{"ruser", "runame"}, "RUSER", LJUST, runame, USERLEN},
	{{"pgid"}, "PGID", 0, evar, PIDLEN, EOFF(e_pgid), USHORT, PIDFMT},
	{{"jobc"}, "JOBC", 0, evar, 4, EOFF(e_jobc), SHORT, "d"},
	{{"sess", "session"}, "SESS", 0, evar, 6, EOFF(e_sess), KPTR, "x"},
	{{"tdev", "dev"}, "TDEV", 0, tdev, 4},
	{{"tname", "tty", "tt"}, "TT", LJUST, tname, 3},
	{{"longtname", "longtty"}, "TT", LJUST, longtname, 8},
	{{"tpgid"}, "TPGID", 0, evar, 4, EOFF(e_tpgid), USHORT, PIDFMT},
	{{"tsession", "tsess"}, "TSESS", 
		0, evar, 6, EOFF(e_tsess), KPTR, "x"},
	{{"paddr", "procaddr"}, "PADDR",
		0, evar, 6, EOFF(e_paddr), KPTR, "x"},
	{{"state", "stat"}, "STAT", 0, state, 4},
	{{"pri"}, "PRI", 0, pri, 3},
	{{"usrpri"}, "UPR", 0, pvar, 3, POFF(p_usrpri), CHAR, "d"},
	{{"nice", "ni"}, "NI", 0, pvar, 2, POFF(p_nice), CHAR, "d"}, 
	{{"vsize", "vsz"}, "VSZ", 0, vsize, 5},
	{{"rssize", "rsz"}, "RSZ", 0, rssize, 4},
	{{"rss", "p_rss"}, "RSS", 0, p_rssize, 4},
	{{"u_procp", "uprocp"}, "UPROCP",
		USER, uvar, 6, UOFF(u_procp), KPTR, "x"},
	{{"umask", "u_cmask"}, "UMASK",
		USER, uvar, 3, UOFF(u_cmask), CHAR, "#o"},
	{{"acflag", "acflg"}, "ACFLG",
		USER, uvar, 3, UOFF(u_acflag), SHORT, "x"},
	{{"start"}, "STARTED", USER|LJUST, started, 8},
	{{"lstart"}, "STARTED", USER|LJUST, lstarted, 28},
	{{"cputime", "time"}, "TIME", USER, cputime, 9},
	{{"p_ru"}, "P_RU", 0, pvar, 6, POFF(p_ru), KPTR, "x"},
	{{"pcpu", "%cpu"}, "%CPU", NLIST, pcpu, 4},
	{{"pmem", "%mem"}, "%MEM", NLIST, pmem, 4},
	{{"sl", "slp", "slptime"}, "SL",
		0, pvar, 3, POFF(p_slptime), CHAR, "d"},
	{{"re", "resident"}, "RE",
		0, pvar, 3, POFF(p_time), CHAR, "d"},
	{{"pagein", "majflt"}, "PAGEIN", USER, pagein, 6},
	{{"lim", "maxrss"}, "LIM", 0, maxrss, 5},
	{{"tsiz"}, "TSIZ", 0, tsize, 4},
	{{"trs"}, "TRS", 0, trss, 3},
	/***
	{{"utime"}, "UTIME", USER, utime, 4},
	{{"stime"}, "STIME", USER, stime, 4},
	{{"ixrss"}, "IXRSS", USER, ixrss, 4},
	{{"idrss"}, "IDRSS", USER, idrss, 4},
	{{"isrss"}, "ISRSS", USER, isrss, 4},
	***/
	{{"minflt"}, "MINFLT", 
		USER, rvar, 4, ROFF(ru_minflt), LONG, "d"},
	{{"majflt"}, "MAJFLT", 
		USER, rvar, 4, ROFF(ru_majflt), LONG, "d"},
	{{"nswap"}, "NSWAP", 	
		USER, rvar, 4, ROFF(ru_nswap), LONG, "d"},
	{{"inblock", "inblk"}, "INBLK", 	
		USER, rvar, 4, ROFF(ru_inblock), LONG, "d"},
	{{"oublock", "oublk"}, "OUBLK", 
		USER, rvar, 4, ROFF(ru_oublock), LONG, "d"},
	{{"msgsnd"}, "MSGSND", 
		USER, rvar, 4, ROFF(ru_msgsnd), LONG, "d"},
	{{"msgrcv"}, "MSGRCV", 
		USER, rvar, 4, ROFF(ru_msgrcv), LONG, "d"},
	{{"nsignals", "nsigs"}, "NSIGS", 
		USER, rvar, 4, ROFF(ru_nsignals), LONG, "d"},
	{{"nvcsw", "vcsw"}, "VCSW", 
		USER, rvar, 5, ROFF(ru_nvcsw), LONG, "d"},
	{{"nivcsw", "ivcsw"}, "IVCSW", 
		USER, rvar, 5, ROFF(ru_nivcsw), LONG, "d"},
	NULL
};

/* 
 * combination variables 
 */
struct combovar {
	char *name;
	char *replace;
} combovar[] = {
	"RUSAGE", "minflt majflt nswap inblock oublock \
		msgsnd msgrcv nsigs nvcsw nivcsw",
	0, 0
};
#define DFMT	"pid tname state cputime comm"
#define LFMT \
	"uid pid ppid cp pri nice vsz rss wchan state tname cputime comm"
#define	JFMT	"user pid ppid pgid sess jobc state tname cputime comm"
#define	SFMT	"uid pid sig sigmask sigignore sigcatch stat tname comm"
#define	VFMT \
	"pid tt state time sl re pagein vsz rss lim tsiz trs %cpu %mem comm"
#define UFMT \
	"uname pid %cpu %mem vsz rss tt state start time comm"

struct kinfo {
	struct proc *ki_p;	/* proc structure */
	struct eproc *ki_e;	/* extra stuff */
	struct usave *ki_u;	/* interesting parts of user */
	char *ki_args;		/* exec args (should be char **) */
	char *ki_env;		/* environment (should be char **) */
} *kinfo;

struct	var *vhead, *vtail;
int	termwidth;	/* width of screen (0 == infinity) */
#define UNLIMITED	0
int	totwidth;	/* calculated width of requested variables */
int	sumrusage;
int	rawcpu;
int	sortby;
#define	SORTMEM	1
#define	SORTCPU 2

int	uid = -1;
dev_t	ttydev = NODEV;
int	pid = -1;
int	all;
int	xflg;
int	prtheader;
int	lineno;

/*
 * variables retrieved via nlist
 */
struct	nlist psnl[] = {
	{"_ecmx"},
#define	X_ECMX		0
	{"_fscale"},
#define	X_FSCALE	1
	{"_ccpu"},
#define	X_CCPU		2
	{NULL}
};
int	fscale;
int	ecmx;
fixpt_t	ccpu;

#define USAGE	"ps [ -(o|O) fmt ] [ -wlvujnsaxSCLmcr ] [ -p pid ] [ -t tty ]"

main (argc, argv) 
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	int ch; 
	register i;
	register struct var *v;
	register struct proc *p;
	struct winsize ws;
	struct kinfo_proc *kprocs;
	int nentries;
	int fmt = 0;
	int pscomp();
	int what, flag;
	char *kludge_oldps_options();

	if ((ioctl(1, TIOCGWINSZ, &ws) == -1 && 
	     ioctl(2, TIOCGWINSZ, &ws) == -1 &&
	     ioctl(0, TIOCGWINSZ, &ws) == -1) ||
	     ws.ws_col == 0)
		termwidth = 79;
	else
		termwidth = ws.ws_col - 1;
	if (argc > 1)
		argv[1] = kludge_oldps_options(argv[1]);

	while ((ch = getopt(argc, argv, "o:O:wlvujnsaxt:p:SCLmrhTg")) != EOF)
		switch((char)ch) {
		case 'o':
			parsefmt(optarg);
			fmt++;
			break;
		case 'O':
			parsefmt("pid");
			parsefmt(optarg);
			parsefmt("state tt time command");
			fmt++;
			break;
		case 'w':
			if (termwidth < 131)
				termwidth = 131;
			else
				termwidth = UNLIMITED;
			break;
		case 'l':
			parsefmt(LFMT);
			fmt++;
			break;
		case 'v':
			parsefmt(VFMT);
			sortby = SORTMEM;
			fmt++;
			break;
		case 'u':
			parsefmt(UFMT);
			sortby = SORTCPU;
			fmt++;
			break;
		case 'j':
			parsefmt(JFMT);
			fmt++;
			break;
		case 's':
			parsefmt(SFMT);
			fmt++;
			break;
		case 'T':
		case 't': {
			struct stat stbuf;
			char *tname, *ttyname();
			char termname[MAXPATHLEN+1];

			if (ch == 'T') {
				if ((tname = ttyname(0)) == NULL)
					error("<stdin>: not a terminal");
			} else 
				tname = optarg;
			if (strlen(tname) == 2) {
				if (strcmp(tname, "co") == 0)
					strcpy(termname, "/dev/console");
				else {
					strcpy(termname, "/dev/tty");
					strcat(termname, tname);
				}
			} else if (*tname != '/') {
				strcpy(termname, "/dev/");
				strcat(termname, tname);
			} else
				strcpy(termname, tname);
			if (stat(termname, &stbuf) == -1)
				syserror(termname);
			if ((stbuf.st_mode & S_IFMT) != S_IFCHR)
				error("%s: not a terminal", termname);
			ttydev = stbuf.st_rdev;
			break;
		}
		case 'p':
			pid = atoi(optarg);
			xflg++;
			break;
		case 'S':
			sumrusage++;
			break;
		case 'C':
			rawcpu++;
			break;
		case 'L': {
			int i = 0;
			struct combovar *cb = &combovar[0];
			char *cp;

			v = &var[0];
			for (;;) {
				if (v->name[0] != NULL) {
					cp = v->name[0];
					v++;
				} else if (cb->name != NULL) {
					cp = cb->name;
					cb++;
				} else
					break;
				if (termwidth && 
				   (i += strlen(cp)+1) > termwidth)
					i = strlen(cp), printf("\n");
				printf("%s ", cp);
			}
			printf("\n");
			exit(0);
		}
		case 'a':
			all++;
			break;
		case 'x':
			xflg++;
			break;
		case 'm':
			sortby = SORTMEM;
			break;
		case 'r':
			sortby = SORTCPU;
			break;
		case 'h':
			prtheader = ws.ws_row > 5 ? ws.ws_row : 22;
			break;
		case 'g':
			break;	/* no-op */
		case '?':
		default:
			fprintf(stderr, "usage: %s\n", USAGE);
			exit(1);
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
		parsefmt(DFMT);

	if (!all && ttydev == NODEV && pid == -1)  /* XXX - should be cleaner */
		uid = getuid();

	/*
	 * scan requested variables, noting what structures are needed,
	 * and adjusting header widths as appropiate.
	 */
	scanvars();
#ifdef notdef
	if (sortby == SORTCPU)
		neednlist = 1;
#endif
	if (neednlist)
		donlist();
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
	} else
		what = KINFO_PROC_ALL;
	/*
	 * select procs
	 */
	if ((nentries = kvm_getprocs(what, flag)) == -1) {
		fprintf(stderr, "ps: %s\n", kvm_geterr());
		exit(1);
	}
	kinfo = (struct kinfo *)malloc(nentries * sizeof (struct kinfo));
	if (kinfo == NULL)
		error("out of memory");
	i = 0;
	while ((p = kvm_nextproc()) != NULL) {
		kinfo[i].ki_p = p;
		kinfo[i].ki_e = kvm_geteproc(p);
		if (needuser)
			saveuser(&kinfo[i]);
		i++;
	}
	nentries = i;
	/*
	 * print header
	 */
	printheader();
	if (nentries == 0)
		exit(0);
	/*
	 * sort proc list
	 */
	qsort(kinfo, nentries, sizeof (struct kinfo), pscomp);
	/*
	 * for each proc, call each variable output function.
	 */
	for (i = 0; i < nentries; i++) {
		if (xflg == 0 && (kinfo[i].ki_e->e_tdev == NODEV ||
		    (kinfo[i].ki_p->p_flag & SCTTY ) == 0))
			continue;
		for (v = vhead; v != NULL; v = v->next) {
			(*v->oproc)(&kinfo[i], v);
			if (v->next != NULL)
				putchar(' ');
		}
		putchar('\n');
		if (prtheader && lineno++ == prtheader-4) {
			putchar('\n');
			printheader();
			lineno = 0;
		}
	}

	exit(0);
}

#define FMTSEP	" \t,\n"

parsefmt(fmt)
	char *fmt;
{
	register char *f = fmt, *cp, *hp;
	struct var *v;
	char *strtok(), *index();
	char newbuf[1024], *nb = newbuf; /* XXX */
	char *lookupcombo();
	struct var *lookupvar();
	
	/*
	 * strtok is not &^%^& re-entrant, so we have
	 * only one level of expansion, looking for combo
	 * variables once here, and expanding the string
	 * before really parsing it.  With strtok_r,
	 * you would move the expansion to before the
	 * lookupvar inside the 2nd while loop with a
	 * recursive call to parsefmt.
	 */
	while ((cp = strtok(f, FMTSEP)) != NULL) {
		if ((hp = lookupcombo(cp)) == NULL);
			hp = cp;
		if (((nb + strlen(hp)) - newbuf) >= 1024)
			error("format too large");
		strcpy(nb, hp);
		while (*nb)
			nb++;
		*nb++ = ' ';
		*nb =  '\0';
		f = NULL;
	}
	f = newbuf;
	while ((cp = strtok(f, FMTSEP)) != NULL) {
		if (hp = index(cp, '='))
			*hp++ = '\0';
		v = lookupvar(cp);
		if (v == NULL)
			error("unknown variable in format: %s", cp);
		if (v->next != NULL || vtail == v)
			error("can't specify a variable twice: %s", cp);
		if (hp)
			v->header = hp;
		if (vhead == NULL)
			vhead = vtail = v;
		else {
			vtail->next = v;
			vtail = v;
		}
		f = NULL;	/* for strtok */
	}

}

scanvars()
{
	register i;
	register struct var *v;

	for (v = vhead; v != NULL; v = v->next) {
		i = strlen(v->header);
		if (v->width < i)
			v->width = i;
		totwidth += v->width + 1;	/* +1 for space */
		if (v->flag & USER)
			needuser = 1;
		if (v->flag & COMM)
			needcomm = 1;
		if (v->flag & NLIST)
			neednlist = 1;
	}
	totwidth--;
}
printheader()
{
	register struct var *v;

	for (v = vhead; v != NULL; v = v->next) {
		if (v->flag & LJUST) {
			if (v->next == NULL)	/* last one */
				printf("%s", v->header);
			else
				printf("%-*s",v->width, v->header);
		} else
			printf("%*s",v->width, v->header);
		if (v->next != NULL)
			putchar(' ');
	}
	putchar('\n');
}

command(k, v) 
	struct kinfo *k;
	struct var *v;
{

	if (v->next == NULL) {		
		/* last field */
		if (termwidth == UNLIMITED)
			printf("%s", k->ki_args);
		else {
			register left = termwidth - (totwidth - v->width);
			register char *cp = k->ki_args;

			if (left < 1)	/* already wrapped, just use std width */
				left = v->width;
			while (left-- && *cp)
				putchar(*cp++);
		}
	} else
		printf("%-*.*s", v->width, v->width, k->ki_args);
		
}

ucomm(k, v) 
	struct kinfo *k;
	struct var *v;
{

	printf("%-*s", v->width, k->ki_p->p_comm);
}

logname(k, v) 
	struct kinfo *k;
	struct var *v;
{

	printf("%-*s", v->width, k->ki_p->p_logname);
}

state(k, v)
	struct kinfo *k;
	struct var *v;
{
	char buf[16];
	register char *cp = buf;
	register struct proc *p = k->ki_p;
	register flag = p->p_flag;

	switch (p->p_stat) {

	case SSTOP:
		*cp = 'T';
		break;

	case SSLEEP:
		if (flag & SSINTR)	/* interuptable (long) */
			*cp = p->p_slptime >= MAXSLP ? 'I' : 'S';
		else 
			*cp = (flag & SPAGE) ? 'P' : 'D';
		break;

	case SRUN:
	case SIDL:
		*cp = 'R';
		break;

	case SZOMB:
		*cp = 'Z';
		break;

	default:
		*cp = '?';
	}
	cp++;
	if (flag & SLOAD) {
		if (p->p_rssize > p->p_maxrss)
			*cp++ = '>';
	} else
		*cp++ = 'W';
	if (p->p_nice < NZERO)
		*cp++ = '<';
	else if (p->p_nice > NZERO)
		*cp++ = 'N';
	if (flag & SUANOM)
		*cp++ = 'A';
	else if (flag & SSEQL)
		*cp++ = 'S';
	if (flag & STRC)
		*cp++ = 'X';
	if (flag & SWEXIT)
		*cp++ = 'E';
	if (flag & SVFORK)
		*cp++ = 'V';
	if (flag & (SSYS|SLOCK|SULOCK|SKEEP|SPHYSIO))
		*cp++ = 'L';
	if (k->ki_e->e_flag & EPROC_SLEADER)
		*cp++ = 's';
	if ((flag & SCTTY) && k->ki_e->e_pgid == k->ki_e->e_tpgid)
		*cp++ = '+';
	*cp = '\0';
	printf("%-*s", v->width, buf);
}

pri(k, v)
	struct kinfo *k;
	struct var *v;
{

	printf("%*d", v->width, k->ki_p->p_pri - PZERO);
}

uname(k, v)
	struct kinfo *k;
	struct var *v;
{

	printf("%-*s", v->width, user_from_uid(k->ki_p->p_uid, 0));
}

runame(k, v)
	struct kinfo *k;
	struct var *v;
{

	printf("%-*s", v->width, user_from_uid(k->ki_p->p_ruid, 0));
}

tdev(k, v)
	struct kinfo *k;
	struct var *v;
{
	dev_t dev = k->ki_e->e_tdev;

	if (dev == NODEV)
		printf("%*s", v->width, "??");
	else {
		char buff[16];

		sprintf(buff, "%d/%d", major(dev), minor(dev));
		printf("%*s", v->width, buff);
	}
}

extern char *devname();

tname(k, v)
	struct kinfo *k;
	struct var *v;
{
	dev_t dev = k->ki_e->e_tdev;
	char *tname;

	if (dev == NODEV || (tname = devname(dev, S_IFCHR)) == NULL)
		printf("%-*s", v->width, "??");
	else {
		if (strncmp(tname, "tty", 3) == 0)
			tname += 3;
		printf("%*.*s%c", v->width-1, v->width-1, tname,
			k->ki_e->e_flag & EPROC_CTTY ? ' ' : '-');
	}
}

longtname(k, v)
	struct kinfo *k;
	struct var *v;
{
	dev_t dev = k->ki_e->e_tdev;
	char *tname;

	if (dev == NODEV || (tname = devname(dev, S_IFCHR)) == NULL)
		printf("%-*s", v->width, "??");
	else
		printf("%-*s", v->width, tname);
}

#include <sys/time.h>

started(k, v)
	struct kinfo *k;
	struct var *v;
{
	extern char *attime();

	printf("%-*s", v->width, k->ki_u ? 
		attime(&k->ki_u->u_start.tv_sec) : "-");
		
}

lstarted(k, v)
	struct kinfo *k;
	struct var *v;
{
	extern char *ctime();
	char *tp; 

	if (k->ki_u)
		(tp = ctime(&k->ki_u->u_start.tv_sec))[24] = '\0';
	else
		tp = "-";
	printf("%-*s", v->width, tp);
}

wchan(k, v)
	struct kinfo *k;
	struct var *v;
{

	if (k->ki_p->p_wchan) {
		if (k->ki_p->p_pri > PZERO)
			printf("%-*.*s", v->width, v->width, k->ki_e->e_wmesg);
		else
			printf("%*x", v->width, 
				(int)k->ki_p->p_wchan &~ KERNBASE);
	} else
		printf("%-*s", v->width, "-");
}

#define pgtok(a)        (((a)*NBPG)/1024)
#define pgtok(a)        (((a)*NBPG)/1024)

vsize(k, v)
	struct kinfo *k;
	struct var *v;
{

	printf("%*d", v->width, 
		pgtok(k->ki_p->p_dsize + k->ki_p->p_ssize + k->ki_e->e_xsize));
}

rssize(k, v)
	struct kinfo *k;
	struct var *v;
{

	printf("%*d", v->width, 
		pgtok(k->ki_p->p_rssize + (k->ki_e->e_xccount ? 
		      (k->ki_e->e_xrssize / k->ki_e->e_xccount) : 0)));
}

p_rssize(k, v)		/* doesn't account for text */
	struct kinfo *k;
	struct var *v;
{

	printf("%*d", v->width, pgtok(k->ki_p->p_rssize));
}

cputime(k, v)
	struct kinfo *k;
	struct var *v;
{
	long secs;
	long psecs;	/* "parts" of a second. first micro, then centi */
	char obuff[128];

	if (k->ki_p->p_stat == SZOMB || k->ki_u == NULL) {
		secs = 0;
		psecs = 0;
	} else {
		secs = k->ki_p->p_utime.tv_sec + 
			k->ki_p->p_stime.tv_sec;
		psecs = k->ki_p->p_utime.tv_usec + 
			k->ki_p->p_stime.tv_usec;
		if (sumrusage) {
			secs += k->ki_u->u_cru.ru_utime.tv_sec + 
				k->ki_u->u_cru.ru_stime.tv_sec;
			psecs += k->ki_u->u_cru.ru_utime.tv_usec + 
				k->ki_u->u_cru.ru_stime.tv_usec;
		}
		/*
		 * round and scale to 100's
		 */
		psecs = (psecs + 5000) / 10000;
		if (psecs >= 100) {
			psecs -= 100;
			secs++;
		}
	}
	sprintf(obuff, "%3ld:%02ld.%02ld", secs/60, secs%60, psecs);
	printf("%*s", v->width, obuff);
}

double
getpcpu(k)
	struct kinfo *k;
{
	/*
	 * note: this routine requires ccpu and fscale
	 * be initialized.  If you call this routine from
	 * somewhere new, insure that the "neednlist" flag
	 * gets set.
	 */
	struct proc *p = k->ki_p;
#define	fxtofl(fixpt)	((double)(fixpt) / fscale)

	if (p->p_time == 0 || (p->p_flag & SLOAD) == 0)	/* XXX - I don't like this */
		return (0.0);
	if (rawcpu)
		return (100.0 * fxtofl(p->p_pctcpu));
	return (100.0 * fxtofl(p->p_pctcpu) /
		(1.0 - exp(p->p_time * log(fxtofl(ccpu)))));
}

pcpu(k, v)
	struct kinfo *k;
	struct var *v;
{

	printf("%*.1f", v->width, getpcpu(k));
}

double
getpmem(k, v)
	struct kinfo *k;
	struct var *v;
{
	struct proc *p = k->ki_p;
	struct eproc *e = k->ki_e;
	double fracmem;
	int szptudot;
	/*
	 * note: this routine requires that ecmx
	 * be initialized.  If you call this routine from
	 * somewhere new, insure that the "neednlist" flag
	 * gets set.
	 */

	if (p->p_flag & SLOAD == 0)
		return (0.0);
	szptudot = UPAGES + clrnd(ctopt(p->p_dsize + p->p_ssize + e->e_xsize));
	fracmem = ((float)p->p_rssize + szptudot)/CLSIZE/ecmx;
	if (p->p_textp && e->e_xccount)
		fracmem += ((float)e->e_xrssize)/CLSIZE/e->e_xccount/ecmx;
	return (100.0 * fracmem);
}

pmem(k, v)
	struct kinfo *k;
	struct var *v;
{

	printf("%*.1f", v->width, getpmem(k));
}

pagein(k, v)
	struct kinfo *k;
	struct var *v;
{

	printf("%*d", v->width, k->ki_u ? k->ki_u->u_ru.ru_majflt : 0);
}

maxrss(k, v)
	struct kinfo *k;
	struct var *v;
{

	if (k->ki_p->p_maxrss != (RLIM_INFINITY/NBPG))
		printf("%*d", v->width, pgtok(k->ki_p->p_maxrss));
	else
		printf("%*s", v->width, "-");
}

tsize(k, v)
	struct kinfo *k;
	struct var *v;
{
	
	printf("%*d", v->width, pgtok(k->ki_e->e_xsize));
}

trss(k, v)
	struct kinfo *k;
	struct var *v;
{

	printf("%*d", v->width, pgtok(k->ki_e->e_xrssize));
}

/*
 * Generic output routines.  Print fields from various prototype
 * structures.
 */
pvar(k, v) 
	struct kinfo *k;
	struct var *v;
{
 
	printval((char *)((char *)k->ki_p + v->off), v);
}

evar(k, v) 
	struct kinfo *k;
	struct var *v;
{
 
	printval((char *)((char *)k->ki_e + v->off), v);
}

uvar(k, v) 
	struct kinfo *k;
	struct var *v;
{
 
	if (k->ki_u)
		printval((char *)((char *)k->ki_u + v->off), v);
	else
		printf("%*s", v->width, "-");
}

rvar(k, v)
	struct kinfo *k;
	struct var *v;
{
	
	if (k->ki_u)
		printval((char *)((char *)(&k->ki_u->u_ru) + v->off), v);
	else
		printf("%*s", v->width, "-");
}

char *
lookupcombo(cp)
	char *cp;
{
	register struct combovar *cv = &combovar[0];

	for (; cv->name; cv++)
		if (strcmp(cp, cv->name) == 0)
			return (cv->replace);
	return (NULL);
}

struct var *
lookupvar(cp)
	char *cp;
{
	register int i, j;

	for (i=0; var[i].name[0] != NULL; i++)
		for (j=0; var[i].name[j] != NULL; j++)
			if (strcmp(cp, var[i].name[j]) == 0)
				return (&var[i]);
	return (NULL);
}

printval(bp, v)
	char *bp;
	struct var *v;
{
	static char ofmt[32] = "%";
	register char *cp = ofmt+1, *fcp = v->fmt;

	if (v->flag & LJUST)
		*cp++ = '-';
	*cp++ = '*';
	while (*cp++ = *fcp++)
		;

	switch (v->type) {
	case CHAR:
		printf(ofmt, v->width, *(char *)bp);
		break;

	case UCHAR:
		printf(ofmt, v->width, *(u_char *)bp);
		break;

	case SHORT:
		printf(ofmt, v->width, *(short *)bp);
		break;

	case USHORT:
		printf(ofmt, v->width, *(u_short *)bp);
		break;

	case LONG:
		printf(ofmt, v->width, *(long *)bp);
		break;

	case ULONG:
		printf(ofmt, v->width, *(u_long *)bp);
		break;

	case KPTR:
		printf(ofmt, v->width, *(u_long *)bp &~ KERNBASE);
		break;

	default:
		error("unknown type %d", v->type);
	}
}

/* XXX - redo */
struct usave *
saveuser(ki) 
	struct kinfo *ki;
{
	register struct usave *usp;
	register struct user *up;
	
	if ((usp = (struct usave *)calloc(1, sizeof (struct usave))) == NULL) {
		fprintf(stderr, "ps: out of memory\n");
		exit(1);
	}
	ki->ki_u = usp;
	up = kvm_getu(ki->ki_p);
	/*
	 * save arguments if needed
	 */
	if (needcomm)
		ki->ki_args = saveargs(ki->ki_p, up);
	else
		ki->ki_args = NULL;
	if (up != NULL) { 
		/*
		 * save important fields
		 */
		usp->u_procp = up->u_procp;
		usp->u_start = up->u_start;
		usp->u_ru = up->u_ru;
		usp->u_cru = up->u_cru;
		usp->u_cmask = up->u_cmask;
		usp->u_acflag = up->u_acflag;
	}
	return;
}

char *
saveargs(p, up)
	struct proc *p;
	struct user *up;
{
	char *savestr();

	return(savestr(kvm_getargs(p, up)));
}
	

pscomp(k1, k2)
	struct kinfo *k1, *k2;
{
	int i;
#define VSIZE(k) ((k)->ki_p->p_dsize + (k)->ki_p->p_ssize + (k)->ki_e->e_xsize)

	if (sortby == SORTCPU)
		return (getpcpu(k2) - getpcpu(k1));
#ifdef notyet
	if (sortby == SORTRUN)
		return (proc_compare(k1->ki_p, k2->ki_p));
#endif
	if (sortby == SORTMEM)
		return (VSIZE(k2) - VSIZE(k1));
	i =  k1->ki_e->e_tdev - k2->ki_e->e_tdev;
	if (i == 0)
		i = k1->ki_p->p_pid - k2->ki_p->p_pid;
	return (i);
}

donlist()
{
	if (kvm_nlist(psnl) != 0)
		error("can't get namelist");
	if (kvm_read(psnl[X_FSCALE].n_value, &fscale, sizeof(int)) !=
	    sizeof (int))
		error("error reading fscale: %s", kvm_geterr());
	if (kvm_read(psnl[X_ECMX].n_value, &ecmx, sizeof(int)) !=
	    sizeof (int))
		error("error reading ecmx: %s", kvm_geterr());
	if (kvm_read(psnl[X_CCPU].n_value, &ccpu, sizeof(fixpt_t)) !=
	    sizeof (fixpt_t))
		error("error reading ccpu: %s", kvm_geterr());
}

char *
savestr(cp)
	char *cp;
{
	register unsigned len;
	register char *dp;

	len = strlen(cp);
	dp = (char *)calloc(len+1, sizeof (char));
	(void) strcpy(dp, cp);
	return (dp);
}

error(a, b, c, d, e)
	char *a, *b, *c, *d, *e;
{
	fprintf(stderr, "ps: ");
	fprintf(stderr, a, b, c, d, e);
	fprintf(stderr, "\n");
	exit(1);
}

syserror(a)
	char *a;
{
	extern errno;

	error("%s: %s", a, strerror(errno));
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
	int len = strlen(s), numlen = 0;
	char *newopts, *ns, *cp;

	if ((newopts = ns = (char *)malloc(len+2)) == NULL)
		error("out of memory");
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
	 * then convert to 'T' (meaning *this* terminal, i.e. ttyname(0).
	 */
	if (*cp == 't' && *s != '-')
		*cp = 'T';
	else {
		/*
		 * otherwise check for trailing number, which *may* be a
		 * pid.
		 */
		while (isdigit(*cp)) {
			--cp;
			numlen++;
		}
	}
	cp++;
	bcopy(s, ns, cp - s);	/* copy everything up to trailing number */
	while (*ns)
		ns++;
	/*
	 * if there's a trailing number, and not a preceding 'p' (pid) or
	 * 't' (tty) flag, then assume it's a pid and insert a 'p' flag.
	 */
	if (isdigit(*cp) && (cp == s || *(cp-1) != 't' && *(cp-1) != 'p' &&
	   ((cp-1) == s || *(cp-2) != 't')))
		*ns++ = 'p';
	strcat(ns, cp);		/* and append the number */

	return (newopts);
}
