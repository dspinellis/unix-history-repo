/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)w.c	5.32 (Berkeley) %G%";
#endif /* not lint */

#define ADDRHACK

/*
 * w - print system status (who and what)
 *
 * This program is similar to the systat command on Tenex/Tops 10/20
 *
 */
#include <sys/param.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/kinfo.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <ctype.h>
#include <kvm.h>
#include <nlist.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utmp.h>
#include <paths.h>

#ifdef ADDRHACK
#include <sys/socket.h>
#include <netdb.h>
#endif

char	*program;
int	ttywidth;		/* width of tty */
int	argwidth;		/* width of tty */
int	header = 1;		/* true if -h flag: don't print heading */
int	wcmd = 1;		/* true if this is w(1), and not uptime(1) */
#ifdef ADDRHACK
int	nflag = 0;		/* true if -n flag: don't convert addrs */
#endif
int	nusers;			/* number of users logged in now */
char *	sel_user;		/* login of particular user selected */
time_t	now;			/* the current time of day */
struct	timeval boottime;
time_t	uptime;			/* time of last reboot & elapsed time since */
struct	utmp utmp;
struct	winsize ws;
int	sortidle;		/* sort bu idle time */
#ifdef ADDRHACK
char	domain[64];
#endif
kvm_t *kd;

#if __STDC__
void error(const char *fmt, ...);
#else
void error();
#endif

void prttime();

/*
 * One of these per active utmp entry.
 */
struct	entry {
	struct	entry *next;
	struct	utmp utmp;
	dev_t	tdev;		/* dev_t of terminal */
	int	idle;		/* idle time of terminal in minutes */
	struct	kinfo_proc *kp;	/* `most interesting' proc */
	char	*args;		/* arg list of interesting process */
} *ep, *ehead = NULL, **nextp = &ehead;

struct nlist nl[] = {
	{ "_boottime" },
#define X_BOOTTIME	0
#if defined(hp300) || defined(i386)
	{ "_cn_tty" },
#define X_CNTTY		1
#endif
	{ "" },
};

#define USAGE "[ -hi ] [ user ]"
#define usage()	fprintf(stderr, "usage: %s: %s\n", program, USAGE)

main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	register struct kinfo_proc *kp;
	struct stat *stp, *ttystat();
	FILE *ut;
	char *cp;
	int nentries;
	int ch;
	extern char *optarg;
	extern int optind;
	char *attime();
	char errbuf[80];

	program = argv[0];
	/*
	 * are we w(1) or uptime(1)
	 */
	if ((cp = rindex(program, '/')) || *(cp = program) == '-')
		cp++;
	if (*cp == 'u')
		wcmd = 0;

	while ((ch = getopt(argc, argv, "hiflnsuw")) != EOF)
		switch((char)ch) {
		case 'h':
			header = 0;
			break;
		case 'i':
			sortidle++;
			break;
		case 'n':
#ifdef ADDRHACK
			++nflag;
#endif
			break;
		case 'f': case 'l': case 's': case 'u': case 'w':
			error("[-flsuw] no longer supported");
			usage();
			exit(1);
		case '?':
		default:
			usage();
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if (*argv)
		sel_user = *argv;

	kd = kvm_openfiles((char *)0, (char *)0, (char *)0, O_RDONLY, errbuf);
	if (kd == NULL) {
		error("%s", errbuf);
		exit(1);
	}
	if (header && kvm_nlist(kd, nl) != 0) {
		error("can't get namelist");
		exit(1);
	}
#ifdef ADDRHACK
	if (!nflag) {
		if (gethostname(domain, sizeof(domain) - 1) < 0 ||
		    (cp = index(domain, '.')) == 0)
			domain[0] = '\0';
		else {
			domain[sizeof(domain) - 1] = '\0';
			bcopy(cp, domain, strlen(cp) + 1);
		}
	}
#endif
	time(&now);
	ut = fopen(_PATH_UTMP, "r");
	while (fread(&utmp, sizeof(utmp), 1, ut)) {
		if (utmp.ut_name[0] == '\0')
			continue;
		nusers++;
		if (wcmd == 0 || (sel_user &&
		    strncmp(utmp.ut_name, sel_user, UT_NAMESIZE) != 0))
			continue;
		if ((ep = (struct entry *)
		     calloc(1, sizeof (struct entry))) == NULL) {
			error("out of memory");
			exit(1);
		}
		*nextp = ep;
		nextp = &(ep->next);
		bcopy(&utmp, &(ep->utmp), sizeof (struct utmp));
		stp = ttystat(ep->utmp.ut_line);
		ep->tdev = stp->st_rdev;
#if defined(hp300) || defined(i386)
		/*
		 * XXX  If this is the console device, attempt to ascertain
		 * the true console device dev_t.
		 */
		if (ep->tdev == 0) {
			static dev_t cn_dev;

			if (nl[X_CNTTY].n_value) {
				struct tty cn_tty, *cn_ttyp;
				
				if (kvm_read(kd, (u_long)nl[X_CNTTY].n_value,
				    (char *)&cn_ttyp, sizeof(cn_ttyp)) > 0) {
					(void)kvm_read(kd, (u_long)cn_ttyp,
					    (char *)&cn_tty, sizeof (cn_tty));
					cn_dev = cn_tty.t_dev;
				}
				nl[X_CNTTY].n_value = 0;
			}
			ep->tdev = cn_dev;
		}
#endif
		ep->idle = ((now - stp->st_atime) + 30) / 60; /* secs->mins */
		if (ep->idle < 0)
			ep->idle = 0;
	}
	fclose(ut);

	if (header || wcmd == 0) {
		double	avenrun[3];
		int days, hrs, mins;

		/*
		 * Print time of day
		 */
		fputs(attime(&now), stdout);
		/*
		 * Print how long system has been up.
		 * (Found by looking for "boottime" in kernel)
		 */
		(void)kvm_read(kd, (u_long)nl[X_BOOTTIME].n_value,
		    (char *)&boottime, sizeof (boottime));
		uptime = now - boottime.tv_sec;
		uptime += 30;
		days = uptime / (60*60*24);
		uptime %= (60*60*24);
		hrs = uptime / (60*60);
		uptime %= (60*60);
		mins = uptime / 60;

		printf("  up");
		if (days > 0)
			printf(" %d day%s,", days, days>1?"s":"");
		if (hrs > 0 && mins > 0) {
			printf(" %2d:%02d,", hrs, mins);
		} else {
			if (hrs > 0)
				printf(" %d hr%s,", hrs, hrs>1?"s":"");
			if (mins > 0)
				printf(" %d min%s,", mins, mins>1?"s":"");
		}

		/* Print number of users logged in to system */
		printf("  %d user%s", nusers, nusers>1?"s":"");

		/*
		 * Print 1, 5, and 15 minute load averages.
		 */
		printf(",  load average:");
		(void)getloadavg(avenrun, sizeof(avenrun) / sizeof(avenrun[0]));
		for (i = 0; i < (sizeof(avenrun)/sizeof(avenrun[0])); i++) {
			if (i > 0)
				printf(",");
			printf(" %.2f", avenrun[i]);
		}
		printf("\n");
		if (wcmd == 0)		/* if uptime(1) then done */
			exit(0);
#define HEADER	"USER    TTY FROM              LOGIN@  IDLE WHAT\n"
#define WUSED	(sizeof (HEADER) - sizeof ("WHAT\n"))
		printf(HEADER);
	}

	if ((kp = kvm_getprocs(kd, KINFO_PROC_ALL, 0, &nentries)) == NULL)
		error("%s", kvm_geterr(kd));
	for (i = 0; i < nentries; i++, kp++) {
		register struct proc *p = &kp->kp_proc;
		register struct eproc *e;

		if (p->p_stat == SIDL || p->p_stat == SZOMB)
			continue;
		e = &kp->kp_eproc;
		for (ep = ehead; ep != NULL; ep = ep->next) {
			if (ep->tdev == e->e_tdev && e->e_pgid == e->e_tpgid) {
				/*
				 * Proc is in foreground of this terminal
				 */
				if (proc_compare(&ep->kp->kp_proc, p))
					ep->kp = kp;
				break;
			}
		}
	}
	if ((ioctl(1, TIOCGWINSZ, &ws) == -1 &&
	     ioctl(2, TIOCGWINSZ, &ws) == -1 &&
	     ioctl(0, TIOCGWINSZ, &ws) == -1) || ws.ws_col == 0)
	       ttywidth = 79;
        else
	       ttywidth = ws.ws_col - 1;
	argwidth = ttywidth - WUSED;
	if (argwidth < 4)
		argwidth = 8;
	for (ep = ehead; ep != NULL; ep = ep->next) {
		char *fmt_argv();

		if (ep->kp == NULL) {
			ep->args = "-";
			continue;
		}
		ep->args = fmt_argv(kvm_getargv(kd, ep->kp, argwidth),
		    ep->kp->kp_proc.p_comm, MAXCOMLEN);
		if (ep->args == NULL) {
			error("out of memory");
			exit(1);
		}
	}
	/* sort by idle time */
	if (sortidle && ehead != NULL) {
		struct entry *from = ehead, *save;
		
		ehead = NULL;
		while (from != NULL) {
			for (nextp = &ehead;
			    (*nextp) && from->idle >= (*nextp)->idle;
			    nextp = &(*nextp)->next)
				continue;
			save = from;
			from = from->next;
			save->next = *nextp;
			*nextp = save;
		}
	}
			
	for (ep = ehead; ep != NULL; ep = ep->next) {
#ifdef ADDRHACK
		register char *x;
		register struct hostent *hp;
		unsigned long l;
		char buf[64];
#endif

		cp = *ep->utmp.ut_host ? ep->utmp.ut_host : "-";
#ifdef ADDRHACK
		if (x = index(cp, ':'))
			*x++ = '\0';
		if (!nflag && isdigit(*cp) &&
		    (long)(l = inet_addr(cp)) != -1 &&
		    (hp = gethostbyaddr((char *)&l, sizeof(l),
		    AF_INET))) {
			    if (domain[0] != '\0') {
				    cp = hp->h_name;
				    cp += strlen(hp->h_name);
				    cp -= strlen(domain);
				    if (cp > hp->h_name &&
					strcmp(cp, domain) == 0)
					*cp = '\0';
			    }
			    cp = hp->h_name;
		}
		if (x) {
			sprintf(buf, "%s:%s", cp, x);
			cp = buf;
		}
#endif
		printf("%-*.*s %-2.2s %-*.*s %s",
			UT_NAMESIZE, UT_NAMESIZE, ep->utmp.ut_name,
			strncmp(ep->utmp.ut_line, "tty", 3) == 0 ?
				ep->utmp.ut_line+3 : ep->utmp.ut_line,
			UT_HOSTSIZE, UT_HOSTSIZE, *cp ? cp : "-",
			attime(&ep->utmp.ut_time));
		if (ep->idle >= 36 * 60)
			printf(" %ddays ", (ep->idle + 12 * 60) / (24 * 60));
		else
			prttime(ep->idle, " ");
		printf("%.*s\n", argwidth, ep->args);
	}
	exit(0);
}

struct stat *
ttystat(line)
	char *line;
{
	static struct stat statbuf;
	char ttybuf[sizeof (_PATH_DEV) + UT_LINESIZE + 1];

	sprintf(ttybuf, "%s/%.*s", _PATH_DEV, UT_LINESIZE, line);
	(void) stat(ttybuf, &statbuf);

	return (&statbuf);
}

/*
 * prttime prints a time in hours and minutes or minutes and seconds.
 * The character string tail is printed at the end, obvious
 * strings to pass are "", " ", or "am".
 */
void
prttime(tim, tail)
	time_t tim;
	char *tail;
{

	if (tim >= 60) {
		printf(" %2d:", tim/60);
		tim %= 60;
		printf("%02d", tim);
	} else if (tim >= 0)
		printf("    %2d", tim);
	printf("%s", tail);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
error(const char *fmt, ...)
#else
error(va_alist)
	va_dcl
#endif
{
#if !__STDC__
	char *fmt;
#endif
	va_list ap;

	fprintf(stderr, "%s: ", program);
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
	fmt = va_arg(ap, char *);
#endif
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}
