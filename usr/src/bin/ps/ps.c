/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)ps.c	5.18 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/vm.h>
#include <sys/text.h>
#include <sys/stat.h>
#include <sys/mbuf.h>
#include <machine/pte.h>
#include <a.out.h>
#include <pwd.h>
#include <math.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include "pathnames.h"

char *nl_names[] = {
	"_proc",
#define	X_PROC		0
	"_Usrptmap",
#define	X_USRPTMAP	1
	"_usrpt",
#define	X_USRPT		2
	"_text",
#define	X_TEXT		3
	"_nswap",
#define	X_NSWAP		4
	"_maxslp",
#define	X_MAXSLP	5
	"_ccpu",
#define	X_CCPU		6
	"_ecmx",
#define	X_ECMX		7
	"_nproc",
#define	X_NPROC		8
	"_ntext",
#define	X_NTEXT		9
	"_dmmin",
#define	X_DMMIN		10
	"_dmmax",
#define	X_DMMAX		11
	"_Sysmap",
#define	X_SYSMAP	12
	"_Syssize",
#define	X_SYSSIZE	13
	"_inode",
#define X_INODE		14
	"_file",
#define X_FILE		15
	"_cfree",
#define X_CFREE		16
	"_callout",
#define X_CALLOUT	17
	"_swapmap",
#define X_SWAPMAP	18
	"_argmap",
#define X_ARGMAP	19
	"_kernelmap",
#define X_KERNELMAP	20
	"_mbmap",
#define X_MBMAP		21
	"_namecache",
#define X_NCH		22
	"_quota",
#define X_QUOTA		23
	"_dquot",
#define X_DQUOT		24
	"_swbuf",
#define X_SWBUF		25
	"_buf",
#define X_BUF		26
	"_cmap",
#define X_CMAP		27
	"_buffers",
#define X_BUFFERS	28
	"_fscale",
#define X_FSCALE	29
	""
};

struct nlist *nl;			/* all because we can't init unions */
int nllen;				/* # of nlist entries */

struct	savcom {
	union {
		struct	lsav *lp;
		float	u_pctcpu;
		struct	vsav *vp;
		int	s_ssiz;
	} s_un;
	struct	asav *ap;
} *savcom;

struct	asav {
	char	*a_cmdp;
	int	a_flag;
	short	a_stat, a_uid, a_pid, a_nice, a_pri, a_slptime, a_time;
	size_t	a_size, a_rss, a_tsiz, a_txtrss;
	short	a_xccount;
	char	a_tty[MAXNAMLEN+1];
	dev_t	a_ttyd;
	time_t	a_cpu;
	size_t	a_maxrss;
};

char	*lhdr;
int	wcwidth;		/* width of the wchan field for sprintf*/
struct	lsav {
	short	l_ppid;
	u_char	l_cpu;
	int	l_addr;
	caddr_t	l_wchan;
};

char	*uhdr;
char	*shdr;

char	*vhdr;
struct	vsav {
	u_int	v_majflt;
	size_t	v_swrss, v_txtswrss;
	float	v_pctcpu;
};

#define	NPROC	16

struct	proc proc[NPROC];		/* a few, for less syscalls */
struct	proc *mproc;
struct	text *text;

union {
	struct	user user;
	char	upages[UPAGES][NBPG];
} user;
#define u	user.user

#ifndef	PSFILE
char	*psdb	= _PATH_PSDATABASE;
#else
char	*psdb	= PSFILE;
#endif

int	chkpid = -1;
int	aflg, cflg, eflg, gflg, kflg, lflg, nflg, sflg,
	uflg, vflg, xflg, Uflg;
int	nchans;				/* total # of wait channels */
char	*tptr;
char	*gettty(), *getcmd(), *getname(), *savestr(), *state();
char	*rindex(), *calloc(), *sbrk(), *strcpy(), *strcat(), *strncat();
char	*strncpy(), *index(), *ttyname(), mytty[MAXPATHLEN+1];
char	*malloc(), *getchan();
long	lseek();
off_t	vtophys();
double	pcpu(), pmem();
int	wchancomp();
int	pscomp();
int	nswap, maxslp;
struct	text *atext;
fixpt_t	ccpu;
int	ecmx;
struct	pte *Usrptmap, *usrpt;
int	nproc, ntext, fscale;
int	dmmin, dmmax;
struct	pte *Sysmap;
int	Syssize;

int	nttys;

struct	ttys {
	dev_t	ttyd;
	int cand;
	char	name[MAXNAMLEN+1];
} *allttys;
int cand[16] = {-1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1};
struct lttys {
	struct ttys ttys;
	struct lttys *next;
} *lallttys;

/*
 * struct for the symbolic wait channel info
 *
 * WNAMESIZ is the max # of chars saved of the symbolic wchan gleaned
 * from the namelist.  Normally, only WSNAMESIZ are printed in the long
 * format, unless the terminal width is greater than WTSIZ wide.
 */
#define WNAMESIZ	12
#define WSNAMESIZ	6
#define WTSIZ		95

struct wchan {
	char	wc_name[WNAMESIZ+1];	/* symbolic name */
	caddr_t wc_caddr;		/* addr in kmem */
} *wchanhd;				/* an array sorted by wc_caddr */

#define NWCINDEX	10		/* the size of the index array */

caddr_t wchan_index[NWCINDEX];		/* used to speed searches */
/*
 * names listed here are not kept as wait channels -- this is used to 
 * remove names that confuse ps, like symbols that define the end of an
 * array that happen to be equal to the next symbol.
 */
char *wchan_stop_list[] = {
	"umbabeg",
	"umbaend",
	"calimit",
	NULL
};

int	npr;

int	cmdstart;
int	twidth;
struct	winsize win;
char	*kmemf, *memf, *swapf, *nlistf;
int	kmem, mem, swap = -1;
int	rawcpu, sumcpu;

int	pcbpf;
int	argaddr;

#define	pgtok(a)	((a)/(1024/NBPG))

main(argc, argv)
	char **argv;
{
	register int i, j;
	register char *ap;
	int uid;
	off_t procp;
	int width;

	if (ioctl(1, TIOCGWINSZ, &win) == -1)
		twidth = 80;
	else
		twidth = (win.ws_col == 0 ? 80 : win.ws_col);
	argc--, argv++;
	if (argc > 0) {
		ap = argv[0];
		while (*ap) switch (*ap++) {

		case 'C':
			rawcpu++;
			break;
		case 'S':
			sumcpu++;
			break;

		case 'U':
			Uflg++;
			break;

		case 'a':
			aflg++;
			break;
		case 'c':
			cflg = !cflg;
			break;
		case 'e':
			eflg++;
			break;
		case 'g':
			gflg++;
			break;
		case 'k':
			kflg++;
			break;
		case 'l':
			lflg++;
			break;
		case 'n':
			nflg++;
			break;
		case 's':
			sflg++;
			break;
		case 't':
			if (*ap)
				tptr = ap;
			else if ((tptr = ttyname(0)) != 0) {
				tptr = strcpy(mytty, tptr);
				if (strncmp(tptr, _PATH_DEV,
				    sizeof(_PATH_DEV) - 1) == 0)
					tptr += 5;
			}
			if (strncmp(tptr, "tty", 3) == 0)
				tptr += 3;
			aflg++;
			gflg++;
			if (tptr && *tptr == '?')
				xflg++;
			while (*ap)
				ap++;
			break;
		case 'u': 
			uflg++;
			break;
		case 'v':
			cflg = 1;
			vflg++;
			break;
		case 'w':
			if (twidth < 132)
				twidth = 132;
			else
				twidth = BUFSIZ;
			break;
		case 'x':
			xflg++;
			break;
		default:
			if (!isdigit(ap[-1]))
				break;
			chkpid = atoi(--ap);
			*ap = 0;
			aflg++;
			xflg++;
			break;
		}
	}
	openfiles(argc, argv);
	getkvars(argc, argv);
	uid = getuid();
	printhdr();
	procp = getw(nl[X_PROC].n_value);
	nproc = getw(nl[X_NPROC].n_value);
	savcom = (struct savcom *)calloc((unsigned) nproc, sizeof (*savcom));
	for (i=0; i<nproc; i += NPROC) {
		klseek(kmem, (long)procp, 0);
		j = nproc - i;
		if (j > NPROC)
			j = NPROC;
		j *= sizeof (struct proc);
		if (read(kmem, (char *)proc, j) != j) {
			cantread("proc table", kmemf);
			exit(1);
		}
		procp += j;
		for (j = j / sizeof (struct proc) - 1; j >= 0; j--) {
			mproc = &proc[j];
			if (mproc->p_stat == 0 ||
			    mproc->p_pgrp == 0 && xflg == 0)
				continue;
			if (tptr == 0 && gflg == 0 && xflg == 0 &&
			    mproc->p_ppid == 1)
				continue;
			if (uid != mproc->p_uid && aflg==0)
				continue;
			if (chkpid != -1 && chkpid != mproc->p_pid)
				continue;
			if (vflg && gflg == 0 && xflg == 0) {
				if (mproc->p_stat == SZOMB ||
				    mproc->p_flag&SWEXIT)
					continue;
				if (mproc->p_slptime > MAXSLP &&
				    (mproc->p_stat == SSLEEP ||
				     mproc->p_stat == SSTOP))
				continue;
			}
			save();
		}
	}
	width = twidth - cmdstart - 2;
	if (width < 0)
		width = 0;
	qsort((char *) savcom, npr, sizeof(savcom[0]), pscomp);
	for (i=0; i<npr; i++) {
		register struct savcom *sp = &savcom[i];
		if (lflg)
			lpr(sp);
		else if (vflg)
			vpr(sp);
		else if (uflg)
			upr(sp);
		else
			spr(sp);
		if (sp->ap->a_stat == SZOMB)
			printf(" %.*s", twidth - cmdstart - 2, "<defunct>");
		else if (sp->ap->a_flag & SWEXIT)
			printf(" %.*s", twidth - cmdstart - 2, "<exiting>");
		else if (sp->ap->a_pid == 0)
			printf(" %.*s", twidth - cmdstart - 2, "swapper");
		else if (sp->ap->a_pid == 2)
			printf(" %.*s", twidth - cmdstart - 2, "pagedaemon");
		else
			printf(" %.*s", twidth - cmdstart - 2, sp->ap->a_cmdp);
		printf("\n");
	}
	exit(npr == 0);
}

getw(loc)
	unsigned long loc;
{
	int word;

	klseek(kmem, (long)loc, 0);
	if (read(kmem, (char *)&word, sizeof (word)) != sizeof (word))
		printf("error reading kmem at %x\n", loc);
	return (word);
}

klseek(fd, loc, off)
	int fd;
	long loc;
	int off;
{
	if (kflg) {
		if ((loc = vtophys(loc)) == -1)
			return;
	}
	(void) lseek(fd, (long)loc, off);
}

/*
 * Version allows change of db format w/o temporarily bombing ps's
 */
char thisversion[4] = "V2";		/* length must remain 4 */

writepsdb(unixname)
	char *unixname;
{
	register FILE *fp;
	struct lttys *lt;
	struct stat stb;

	setgid(getgid());
	setuid(getuid());
	if ((fp = fopen(psdb, "w")) == NULL) {
		perror(psdb);
		exit(1);
	} else
		fchmod(fileno(fp), 0644);

	fwrite(thisversion, sizeof thisversion, 1, fp);
	fwrite(unixname, strlen(unixname) + 1, 1, fp);
	if (stat(unixname, &stb) < 0)
		stb.st_mtime = 0;
	fwrite((char *) &stb.st_mtime, sizeof stb.st_mtime, 1, fp);

	fwrite((char *) &nllen, sizeof nllen, 1, fp);
	fwrite((char *) nl, sizeof (struct nlist), nllen, fp);
	fwrite((char *) cand, sizeof (cand), 1, fp);
	fwrite((char *) &nttys, sizeof nttys, 1, fp);
	for (lt = lallttys ; lt ; lt = lt->next)
		fwrite((char *)&lt->ttys, sizeof (struct ttys), 1, fp);
	fwrite((char *) &nchans, sizeof nchans, 1, fp);
	fwrite((char *) wchanhd, sizeof (struct wchan), nchans, fp);
	fwrite((char *) wchan_index, sizeof (caddr_t), NWCINDEX, fp);
	fclose(fp);
}

readpsdb(unixname)
	char *unixname;
{
	register i;
	register FILE *fp;
	char unamebuf[BUFSIZ];
	char *p	= unamebuf;
	char dbversion[sizeof thisversion];
	struct stat stb;
	time_t dbmtime;
	extern int errno;

	if ((fp = fopen(psdb, "r")) == NULL) {
		if (errno == ENOENT)
			return (0);
		perror(psdb);
		exit(1);
	}

	/*
	 * Does the db file match this unix?
	 */
	fread(dbversion, sizeof dbversion, 1, fp);
	if (bcmp(thisversion, dbversion, sizeof thisversion))
		goto bad;
	while ((*p = getc(fp)) != '\0')
		p++;
	if (strcmp(unixname, unamebuf))
		goto bad;
	fread((char *) &dbmtime, sizeof dbmtime, 1, fp);
	if (stat(unixname, &stb) < 0)
		stb.st_mtime = 0;
	if (stb.st_mtime != dbmtime)
		goto bad;

	fread((char *) &nllen, sizeof nllen, 1, fp);
	nl = (struct nlist *) malloc (nllen * sizeof (struct nlist));
	fread((char *) nl, sizeof (struct nlist), nllen, fp);
	fread((char *) cand, sizeof (cand), 1, fp);
	fread((char *) &nttys, sizeof nttys, 1, fp);
	allttys = (struct ttys *)malloc(sizeof(struct ttys)*nttys);
	if (allttys == NULL) {
		fprintf(stderr, "ps: Can't malloc space for tty table\n");
		exit(1);
	}
	fread((char *) allttys, sizeof (struct ttys), nttys, fp);
	fread((char *) &nchans, sizeof nchans, 1, fp);
	wchanhd = (struct wchan *) malloc(nchans * sizeof (struct wchan));
	if (wchanhd == NULL) {
		fprintf(stderr, "ps: Can't malloc space for wait channels\n");
		nflg++;
		fseek(fp, (long) nchans * sizeof (struct wchan), 1);
	} else
		fread((char *) wchanhd, sizeof (struct wchan), nchans, fp);
	fread((char *) wchan_index, sizeof (caddr_t), NWCINDEX, fp);
	fclose(fp);
	return(1);

bad:
	fclose(fp);
	return(0);
}

openfiles(argc, argv)
	char **argv;
{

	kmemf = _PATH_KMEM;
	if (kflg)
		kmemf = argc > 2 ? argv[2] : _PATH_VMCORE;
	kmem = open(kmemf, 0);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	if (kflg)  {
		mem = kmem;
		memf = kmemf;
	} else {
		memf = _PATH_MEM;
		mem = open(memf, 0);
		if (mem < 0) {
			perror(memf);
			exit(1);
		}
	}
	if (kflg == 0 || argc > 3) {
		swapf = argc>3 ? argv[3]: _PATH_DRUM;
		swap = open(swapf, 0);
		if (swap < 0) {
			perror(swapf);
			exit(1);
		}
	}
}

getkvars(argc, argv)
	char **argv;
{
	int faildb = 0;			/* true if psdatabase init failed */
	int i;

	nlistf = argc > 1 ? argv[1] : _PATH_UNIX;
	if (Uflg) {
		init_nlist();
		nlist(nlistf, nl);
		getvchans();
		getdev();
		writepsdb(nlistf);
		exit (0);
	} else if (!readpsdb(nlistf)) {
		init_nlist();
		if (!kflg)
			nl[X_SYSMAP].n_un.n_name = "";
		faildb = 1;
		nlist(nlistf, nl);
		nttys = 0;
		getdev();
	}

	if (nl[0].n_type == 0) {
		fprintf(stderr, "%s: No namelist\n", nlistf);
		exit(1);
	}
	if (kflg) {
		/* We must do the sys map first because klseek uses it */
		long	addr;

		Syssize = nl[X_SYSSIZE].n_value;
		Sysmap = (struct pte *)
			calloc((unsigned) Syssize, sizeof (struct pte));
		if (Sysmap == NULL) {
			fprintf(stderr, "Out of space for Sysmap\n");
			exit(1);
		}
		addr = (long) nl[X_SYSMAP].n_value;
		addr &= ~KERNBASE;
		(void) lseek(kmem, addr, 0);
		read(kmem, (char *) Sysmap, Syssize * sizeof (struct pte));
	}
	if (faildb)
		getvchans();
	usrpt = (struct pte *)nl[X_USRPT].n_value;
	Usrptmap = (struct pte *)nl[X_USRPTMAP].n_value;
	klseek(kmem, (long)nl[X_NSWAP].n_value, 0);
	if (read(kmem, (char *)&nswap, sizeof (nswap)) != sizeof (nswap)) {
		cantread("nswap", kmemf);
		exit(1);
	}
	klseek(kmem, (long)nl[X_MAXSLP].n_value, 0);
	if (read(kmem, (char *)&maxslp, sizeof (maxslp)) != sizeof (maxslp)) {
		cantread("maxslp", kmemf);
		exit(1);
	}
	klseek(kmem, (long)nl[X_CCPU].n_value, 0);
	if (read(kmem, (char *)&ccpu, sizeof (ccpu)) != sizeof (ccpu)) {
		cantread("ccpu", kmemf);
		exit(1);
	}
	klseek(kmem, (long)nl[X_ECMX].n_value, 0);
	if (read(kmem, (char *)&ecmx, sizeof (ecmx)) != sizeof (ecmx)) {
		cantread("ecmx", kmemf);
		exit(1);
	}
	if (uflg || vflg) {
		ntext = getw(nl[X_NTEXT].n_value);
		text = (struct text *)
			calloc((unsigned) ntext, sizeof (struct text));
		if (text == 0) {
			fprintf(stderr, "no room for text table\n");
			exit(1);
		}
		atext = (struct text *)getw(nl[X_TEXT].n_value);
		klseek(kmem, (long)atext, 0);
		if (read(kmem, (char *)text, ntext * sizeof (struct text))
		    != ntext * sizeof (struct text)) {
			cantread("text table", kmemf);
			exit(1);
		}
	}
	dmmin = getw(nl[X_DMMIN].n_value);
	dmmax = getw(nl[X_DMMAX].n_value);
	fscale = getw(nl[X_FSCALE].n_value);
}

/*
 * get the valloc'ed kernel variables for symbolic wait channels
 */
getvchans()
{
	int i, tmp;

	if (nflg)
		return;

#define addv(i) 	addchan(&nl[i].n_un.n_name[1], getw(nl[i].n_value))
	addv(X_INODE);
	addv(X_FILE);
	addv(X_PROC);
	addv(X_TEXT);
	addv(X_CFREE);
	addv(X_CALLOUT);
	addv(X_SWAPMAP);
	addv(X_ARGMAP);
	addv(X_KERNELMAP);
	addv(X_MBMAP);
	addv(X_NCH);
	if (nl[X_QUOTA].n_value != 0) {	/* these are #ifdef QUOTA */
		addv(X_QUOTA);
		addv(X_DQUOT);
	}
	addv(X_SWBUF);
	addv(X_BUF);
	addv(X_CMAP);
	addv(X_BUFFERS);
	qsort(wchanhd, nchans, sizeof (struct wchan), wchancomp);
	for (i = 0; i < NWCINDEX; i++) {
		tmp = i * nchans;
		wchan_index[i] = wchanhd[tmp / NWCINDEX].wc_caddr;
	}
#undef addv
}
printhdr()
{
	char *hdr;

	if (sflg+lflg+vflg+uflg > 1) {
		fprintf(stderr, "ps: specify only one of s,l,v and u\n");
		exit(1);
	}
	if (lflg) {
		if (nflg)
			wcwidth = 6;
		else if (twidth > WTSIZ)
			wcwidth = -WNAMESIZ;
		else
			wcwidth = -WSNAMESIZ;
		if ((hdr = malloc(strlen(lhdr) + WNAMESIZ)) == NULL) {
			fprintf(stderr, "ps: out of memory\n");
			exit(1);
		}
		(void)sprintf(hdr, lhdr, wcwidth, "WCHAN");
	} else if (vflg)
		hdr = vhdr;
	else if (uflg) {
		/* add enough on so that it can hold the sprintf below */
		if ((hdr = malloc(strlen(uhdr) + 10)) == NULL) {
			fprintf(stderr, "ps: out of memory\n");
			exit(1);
		}
		(void)sprintf(hdr, uhdr, nflg ? " UID" : "USER    ");
	} else
		hdr = shdr;
	if (lflg+vflg+uflg+sflg == 0)
		hdr += strlen("SSIZ ");
	cmdstart = strlen(hdr);
	printf("%s COMMAND\n", hdr);
	(void) fflush(stdout);
}

cantread(what, fromwhat)
	char *what, *fromwhat;
{

	fprintf(stderr, "ps: error reading %s from %s\n", what, fromwhat);
}

struct	direct *dbuf;
int	dialbase;

getdev()
{
	register DIR *df;
	struct ttys *t;
	struct lttys *lt;

	if (chdir(_PATH_DEV) < 0) {
		perror(_PATH_DEV);
		exit(1);
	}
	dialbase = -1;
	if ((df = opendir(".")) == NULL) {
		fprintf(stderr, "ps: can't open . in %s\n", _PATH_DEV);
		exit(1);
	}
	while ((dbuf = readdir(df)) != NULL) 
		maybetty();
	closedir(df);
	allttys = (struct ttys *)malloc(sizeof(struct ttys)*nttys);
	if (allttys == NULL) {
		fprintf(stderr, "ps: can't malloc space for tty table\n");
		exit(1);
	}
	for (lt = lallttys, t = allttys; lt ; lt = lt->next, t++)
		*t = lt->ttys;
}

/*
 * Attempt to avoid stats by guessing minor device
 * numbers from tty names.  Console is known,
 * know that r(hp|up|mt) are unlikely as are different mem's,
 * floppy, null, tty, etc.
 */
maybetty()
{
	register char *cp = dbuf->d_name;
	static struct lttys *dp;
	struct lttys *olddp;
	int x;
	struct stat stb;

	switch (cp[0]) {

	case 'c':
		if (!strcmp(cp, "console")) {
			x = 0;
			goto donecand;
		}
		/* cu[la]? are possible!?! don't rule them out */
		break;

	case 'd':
		if (!strcmp(cp, "drum"))
			return;
		break;

	case 'f':
		if (!strcmp(cp, "floppy"))
			return;
		break;

	case 'k':
		cp++;
		if (*cp == 'U')
			cp++;
		goto trymem;

	case 'r':
		cp++;
#define is(a,b) cp[0] == 'a' && cp[1] == 'b'
		if (is(h,p) || is(r,a) || is(u,p) || is(h,k) 
		    || is(r,b) || is(m,t)) {
			cp += 2;
			if (isdigit(*cp) && cp[2] == 0)
				return;
		}
		break;

	case 'm':
trymem:
		if (cp[0] == 'm' && cp[1] == 'e' && cp[2] == 'm' && cp[3] == 0)
			return;
		if (cp[0] == 'm' && cp[1] == 't')
			return;
		break;

	case 'n':
		if (!strcmp(cp, "null"))
			return;
		if (!strncmp(cp, "nrmt", 4))
			return;
		break;

	case 'p':
		if (cp[1] && cp[1] == 't' && cp[2] == 'y')
			return;
		break;

	case 'v':
		if ((cp[1] == 'a' || cp[1] == 'p') && isdigit(cp[2]) &&
		    cp[3] == 0)
			return;
		break;
	}
	cp = dbuf->d_name + dbuf->d_namlen - 1;
	x = 0;
	if (cp[-1] == 'd') {
		if (dialbase == -1) {
			if (stat("ttyd0", &stb) == 0)
				dialbase = stb.st_rdev & 017;
			else
				dialbase = -2;
		}
		if (dialbase == -2)
			x = 0;
		else
			x = 11;
	}
	if (cp > dbuf->d_name && isdigit(cp[-1]) && isdigit(*cp))
		x += 10 * (cp[-1] - ' ') + cp[0] - '0';
	else if (*cp >= 'a' && *cp <= 'f')
		x += 10 + *cp - 'a';
	else if (isdigit(*cp))
		x += *cp - '0';
	else
		x = -1;
donecand:
	olddp = dp;
	dp = (struct lttys *)malloc(sizeof(struct lttys));
	if (dp == NULL) {
		fprintf(stderr, "ps: Can't malloc space for tty table\n");
		exit(1);
	}
	if (lallttys == NULL)
		lallttys = dp;
	nttys++;
	if (olddp)
		olddp->next = dp;
	dp->next = NULL;
	(void) strcpy(dp->ttys.name, dbuf->d_name);
	if (Uflg) {
		if (stat(dp->ttys.name, &stb) == 0 &&
		   (stb.st_mode&S_IFMT)==S_IFCHR)
			dp->ttys.ttyd = x = stb.st_rdev;
		else {
			nttys--;
			if (lallttys == dp)
				lallttys = NULL;
			free(dp);
			dp = olddp;
			if (dp)
				dp->next = NULL;
			return;
		}
	} else
		dp->ttys.ttyd = -1;
	if (x == -1)
		return;
	x &= 017;
	dp->ttys.cand = cand[x];
	cand[x] = nttys-1;
}

char *
gettty()
{
	register char *p;
	register struct ttys *dp;
	struct stat stb;
	int x;

	if (u.u_ttyp == 0)
		return(" ?");
	x = u.u_ttyd & 017;
	for (dp = &allttys[cand[x]]; dp != &allttys[-1];
	     dp = &allttys[dp->cand]) {
		if (dp->ttyd == -1) {
			if (stat(dp->name, &stb) == 0 &&
			   (stb.st_mode&S_IFMT)==S_IFCHR)
				dp->ttyd = stb.st_rdev;
			else
				dp->ttyd = -2;
		}
		if (dp->ttyd == u.u_ttyd)
			goto found;
	}
	/* ick */
	for (dp = allttys; dp < &allttys[nttys]; dp++) {
		if (dp->ttyd == -1) {
			if (stat(dp->name, &stb) == 0 &&
			   (stb.st_mode&S_IFMT)==S_IFCHR)
				dp->ttyd = stb.st_rdev;
			else
				dp->ttyd = -2;
		}
		if (dp->ttyd == u.u_ttyd)
			goto found;
	}
	return ("?");
found:
	p = dp->name;
	if (p[0]=='t' && p[1]=='t' && p[2]=='y')
		p += 3;
	return (p);
}

save()
{
	register struct savcom *sp;
	register struct asav *ap;
	register char *cp;
	register struct text *xp;
	char *ttyp, *cmdp;

	if (mproc->p_stat != SZOMB && getu() == 0)
		return;
	ttyp = gettty();
	if (xflg == 0 && ttyp[0] == '?' || tptr && strncmp(tptr, ttyp, 2))
		return;
	sp = &savcom[npr];
	cmdp = getcmd();
	if (cmdp == 0)
		return;
	sp->ap = ap = (struct asav *)calloc(1, sizeof (struct asav));
	sp->ap->a_cmdp = cmdp;
#define e(a,b) ap->a = mproc->b
	e(a_flag, p_flag); e(a_stat, p_stat); e(a_nice, p_nice);
	e(a_uid, p_uid); e(a_pid, p_pid); e(a_pri, p_pri);
	e(a_slptime, p_slptime); e(a_time, p_time);
	ap->a_tty[0] = ttyp[0];
	ap->a_tty[1] = ttyp[1] ? ttyp[1] : ' ';
	if (ap->a_stat == SZOMB) {
		ap->a_cpu = 0;
	} else {
		ap->a_size = mproc->p_dsize + mproc->p_ssize;
		e(a_rss, p_rssize); 
		ap->a_ttyd = u.u_ttyd;
		ap->a_cpu = u.u_ru.ru_utime.tv_sec + u.u_ru.ru_stime.tv_sec;
		if (sumcpu)
			ap->a_cpu += u.u_cru.ru_utime.tv_sec + u.u_cru.ru_stime.tv_sec;
		if (mproc->p_textp && text) {
			xp = &text[mproc->p_textp - atext];
			ap->a_tsiz = xp->x_size;
			ap->a_txtrss = xp->x_rssize;
			ap->a_xccount = xp->x_ccount;
		}
	}
#undef e
	ap->a_maxrss = mproc->p_maxrss;
	if (lflg) {
		register struct lsav *lp;

		sp->s_un.lp = lp = (struct lsav *)
			calloc(1, sizeof (struct lsav));
#define e(a,b) lp->a = mproc->b
		e(l_ppid, p_ppid); e(l_cpu, p_cpu);
		if (ap->a_stat != SZOMB)
			e(l_wchan, p_wchan);
#undef e
		lp->l_addr = pcbpf;
	} else if (vflg) {
		register struct vsav *vp;

		sp->s_un.vp = vp = (struct vsav *)
			calloc(1, sizeof (struct vsav));
#define e(a,b) vp->a = mproc->b
		if (ap->a_stat != SZOMB) {
			e(v_swrss, p_swrss);
			vp->v_majflt = u.u_ru.ru_majflt;
			if (mproc->p_textp)
				vp->v_txtswrss = xp->x_swrss;
		}
		vp->v_pctcpu = pcpu();
#undef e
	} else if (uflg)
		sp->s_un.u_pctcpu = pcpu();
	else if (sflg) {
		if (ap->a_stat != SZOMB) {
			for (cp = (char *)u.u_stack;
			    cp < &user.upages[UPAGES][0]; )
				if (*cp++)
					break;
			sp->s_un.s_ssiz = (&user.upages[UPAGES][0] - cp);
		}
	}

	npr++;
}

double
pmem(ap)
	register struct asav *ap;
{
	double fracmem;
	int szptudot;

	if ((ap->a_flag&SLOAD) == 0)
		fracmem = 0.0;
	else {
		szptudot = UPAGES + clrnd(ctopt(ap->a_size+ap->a_tsiz));
		fracmem = ((float)ap->a_rss+szptudot)/CLSIZE/ecmx;
		if (ap->a_xccount)
			fracmem += ((float)ap->a_txtrss)/CLSIZE/
			    ap->a_xccount/ecmx;
	}
	return (100.0 * fracmem);
}

#define	fxtofl(fixpt)	((double) fixpt / fscale)
double
pcpu()
{
	time_t time;

	time = mproc->p_time;
	if (time == 0 || (mproc->p_flag&SLOAD) == 0)
		return (0.0);
	if (rawcpu)
		return (100.0 * fxtofl(mproc->p_pctcpu));
	return (100.0 * fxtofl(mproc->p_pctcpu) /
	        (1.0 - exp(time * log(fxtofl(ccpu)))));
}
#undef fxtofl

getu()
{
	struct pte *pteaddr, apte;
	struct pte arguutl[UPAGES+CLSIZE];
	register int i;
	int ncl, size;

	size = sflg ? ctob(UPAGES) : sizeof (struct user);
	if ((mproc->p_flag & SLOAD) == 0) {
		if (swap < 0)
			return (0);
		(void) lseek(swap, (long)dtob(mproc->p_swaddr), 0);
		if (read(swap, (char *)&user.user, size) != size) {
			fprintf(stderr, "ps: cant read u for pid %d from %s\n",
			    mproc->p_pid, swapf);
			return (0);
		}
		pcbpf = 0;
		argaddr = 0;
		return (1);
	}
	pteaddr = &Usrptmap[btokmx(mproc->p_p0br) + mproc->p_szpt - 1];
	klseek(kmem, (long)pteaddr, 0);
	if (read(kmem, (char *)&apte, sizeof(apte)) != sizeof(apte)) {
		printf("ps: cant read indir pte to get u for pid %d from %s\n",
		    mproc->p_pid, kmemf);
		return (0);
	}
	lseek(mem,
	    (long)ctob(apte.pg_pfnum+1) - (UPAGES+CLSIZE) * sizeof (struct pte),
		0);
	if (read(mem, (char *)arguutl, sizeof(arguutl)) != sizeof(arguutl)) {
		printf("ps: cant read page table for u of pid %d from %s\n",
		    mproc->p_pid, memf);
		return (0);
	}
	if (arguutl[0].pg_fod == 0 && arguutl[0].pg_pfnum)
		argaddr = ctob(arguutl[0].pg_pfnum);
	else
		argaddr = 0;
	pcbpf = arguutl[CLSIZE].pg_pfnum;
	ncl = (size + NBPG*CLSIZE - 1) / (NBPG*CLSIZE);
	while (--ncl >= 0) {
		i = ncl * CLSIZE;
		lseek(mem, (long)ctob(arguutl[CLSIZE+i].pg_pfnum), 0);
		if (read(mem, user.upages[i], CLSIZE*NBPG) != CLSIZE*NBPG) {
			printf("ps: cant read page %d of u of pid %d from %s\n",
			    arguutl[CLSIZE+i].pg_pfnum, mproc->p_pid, memf);
			return(0);
		}
	}
	return (1);
}

char *
getcmd()
{
	char cmdbuf[CLSIZE*NBPG];
	union {
		char	argc[CLSIZE*NBPG];
		int	argi[CLSIZE*NBPG/sizeof (int)];
	} argspac;
	register char *cp;
	register int *ip;
	char c;
	int nbad;
	struct dblock db;
	char *file;

	if (mproc->p_stat == SZOMB || mproc->p_flag&(SSYS|SWEXIT))
		return ("");
	if (cflg) {
		(void) strncpy(cmdbuf, u.u_comm, sizeof (u.u_comm));
		return (savestr(cmdbuf));
	}
	if ((mproc->p_flag & SLOAD) == 0 || argaddr == 0) {
		if (swap < 0)
			goto retucomm;
		vstodb(0, CLSIZE, &u.u_smap, &db, 1);
		(void) lseek(swap, (long)dtob(db.db_base), 0);
		if (read(swap, (char *)&argspac, sizeof(argspac))
		    != sizeof(argspac))
			goto bad;
		file = swapf;
	} else {
		lseek(mem, (long)argaddr, 0);
		if (read(mem, (char *)&argspac, sizeof (argspac))
		    != sizeof (argspac))
			goto bad;
		file = memf;
	}
	ip = &argspac.argi[CLSIZE*NBPG/sizeof (int)];
	ip -= 2;		/* last arg word and .long 0 */
	while (*--ip)
		if (ip == argspac.argi)
			goto retucomm;
	*(char *)ip = ' ';
	ip++;
	nbad = 0;
	for (cp = (char *)ip; cp < &argspac.argc[CLSIZE*NBPG]; cp++) {
		c = *cp & 0177;
		if (c == 0)
			*cp = ' ';
		else if (c < ' ' || c > 0176) {
			if (++nbad >= 5*(eflg+1)) {
				*cp++ = ' ';
				break;
			}
			*cp = '?';
		} else if (eflg == 0 && c == '=') {
			while (*--cp != ' ')
				if (cp <= (char *)ip)
					break;
			break;
		}
	}
	*cp = 0;
	while (*--cp == ' ')
		*cp = 0;
	cp = (char *)ip;
	(void) strncpy(cmdbuf, cp, &argspac.argc[CLSIZE*NBPG] - cp);
	if (cp[0] == '-' || cp[0] == '?' || cp[0] <= ' ') {
		(void) strcat(cmdbuf, " (");
		(void) strncat(cmdbuf, u.u_comm, sizeof(u.u_comm));
		(void) strcat(cmdbuf, ")");
	}
	return (savestr(cmdbuf));

bad:
	fprintf(stderr, "ps: error locating command name for pid %d from %s\n",
	    mproc->p_pid, file);
retucomm:
	(void) strcpy(cmdbuf, " (");
	(void) strncat(cmdbuf, u.u_comm, sizeof (u.u_comm));
	(void) strcat(cmdbuf, ")");
	return (savestr(cmdbuf));
}

char	*lhdr =
"      F  UID   PID  PPID CP PRI NI ADDR    SZ  RSS %*sSTAT TT  TIME";
lpr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;
	register struct lsav *lp = sp->s_un.lp;

	printf("%7x %4d %5u %5u %2d %3d %2d %4x %5d %4d",
	    (ap->a_flag &~ SPTECHG),				/* XXX */
	    ap->a_uid, ap->a_pid, lp->l_ppid,
	    lp->l_cpu > 99 ? 99 : lp->l_cpu, ap->a_pri-PZERO,
	    ap->a_nice, lp->l_addr, pgtok(ap->a_size), pgtok(ap->a_rss));
	if (lp->l_wchan == 0)
		printf(" %*s", wcwidth, "");
	else if (nflg)
		printf(" %*x", wcwidth, (int)lp->l_wchan&~KERNBASE);
	else
		printf(" %*.*s", wcwidth, abs(wcwidth), getchan(lp->l_wchan));
	printf(" %-2.3s ", state(ap));
	ptty(ap->a_tty);
	ptime(ap);
}

ptty(tp)
	char *tp;
{

	printf("%-2.2s", tp);
}

ptime(ap)
	struct asav *ap;
{

	printf(" %3ld:%02ld", ap->a_cpu / 60, ap->a_cpu % 60);
}

char	*uhdr =
"%s   PID %%CPU %%MEM    SZ   RSS TT STAT TIME";
upr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;
	int vmsize, rmsize;

	vmsize = pgtok((ap->a_size + ap->a_tsiz));
	rmsize = pgtok(ap->a_rss);
	if (ap->a_xccount)
		rmsize += pgtok(ap->a_txtrss/ap->a_xccount);
	if (nflg)
		printf("%4d ", ap->a_uid);
	else
		printf("%-8.8s ", getname(ap->a_uid));
	printf("%5d %4.1f %4.1f %5d %5d",
	    ap->a_pid, sp->s_un.u_pctcpu, pmem(ap), vmsize, rmsize);
	putchar(' ');
	ptty(ap->a_tty);
	printf(" %-2.3s", state(ap));
	ptime(ap);
}

char *vhdr =
" SIZE  PID TT STAT TIME SL RE PAGEIN  SIZE   RSS   LIM TSIZ TRS %CPU %MEM"+5;
vpr(sp)
	struct savcom *sp;
{
	register struct vsav *vp = sp->s_un.vp;
	register struct asav *ap = sp->ap;

	printf("%5u ", ap->a_pid);
	ptty(ap->a_tty);
	printf(" %-2.3s", state(ap));
	ptime(ap);
	printf(" %2d %2d %6d %5d %5d",
	   ap->a_slptime > 99 ? 99 : ap-> a_slptime,
	   ap->a_time > 99 ? 99 : ap->a_time, vp->v_majflt,
	   pgtok(ap->a_size), pgtok(ap->a_rss));
	if (ap->a_maxrss == (RLIM_INFINITY/NBPG))
		printf("    xx");
	else
		printf(" %5d", pgtok(ap->a_maxrss));
	printf(" %4d %3d %4.1f %4.1f",
	   pgtok(ap->a_tsiz), pgtok(ap->a_txtrss), vp->v_pctcpu, pmem(ap));
}

char	*shdr =
"SSIZ   PID TT STAT  TIME";
spr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;

	if (sflg)
		printf("%4d ", sp->s_un.s_ssiz);
	printf("%5u", ap->a_pid);
	putchar(' ');
	ptty(ap->a_tty);
	printf(" %-2.3s", state(ap));
	ptime(ap);
}

char *
state(ap)
	register struct asav *ap;
{
	static char res[5];
	char *cp = res;

	switch (ap->a_stat) {

	case SSTOP:
		*cp = 'T';
		break;

	case SSLEEP:
		if (ap->a_pri >= PZERO)
			if (ap->a_slptime >= MAXSLP)
				*cp = 'I';
			else
				*cp = 'S';
		else if (ap->a_flag & SPAGE)
			*cp = 'P';
		else
			*cp = 'D';
		break;

	case SWAIT:
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
	if (ap->a_flag & SLOAD) {
		if (ap->a_rss > ap->a_maxrss)
			*cp++ = '>';
	} else
		*cp++ = 'W';
	if (ap->a_nice < NZERO)
		*cp++ = '<';
	else if (ap->a_nice > NZERO)
		*cp++ = 'N';
	if (ap->a_flag & SUANOM)
		*cp++ = 'A';
	else if (ap->a_flag & SSEQL)
		*cp++ = 'S';
	*cp = '\0';
	return (res);
}

/*
 * Given a base/size pair in virtual swap area,
 * return a physical base/size pair which is the
 * (largest) initial, physically contiguous block.
 */
vstodb(vsbase, vssize, dmp, dbp, rev)
	register int vsbase;
	int vssize;
	struct dmap *dmp;
	register struct dblock *dbp;
{
	register int blk = dmmin;
	register swblk_t *ip = dmp->dm_map;

	vsbase = ctod(vsbase);
	vssize = ctod(vssize);
	if (vsbase < 0 || vsbase + vssize > dmp->dm_size)
		panic("vstodb");
	while (vsbase >= blk) {
		vsbase -= blk;
		if (blk < dmmax)
			blk *= 2;
		ip++;
	}
	if (*ip <= 0 || *ip + blk > nswap)
		panic("vstodb *ip");
	dbp->db_size = min(vssize, blk - vsbase);
	dbp->db_base = *ip + (rev ? blk - (vsbase + dbp->db_size) : vsbase);
}

/*ARGSUSED*/
panic(cp)
	char *cp;
{

#ifdef DEBUG
	printf("%s\n", cp);
#endif
}

min(a, b)
{

	return (a < b ? a : b);
}

pscomp(s1, s2)
	struct savcom *s1, *s2;
{
	register int i;

	if (uflg)
		return (s2->s_un.u_pctcpu > s1->s_un.u_pctcpu ? 1 : -1);
	if (vflg)
		return (vsize(s2) - vsize(s1));
	i = s1->ap->a_ttyd - s2->ap->a_ttyd;
	if (i == 0)
		i = s1->ap->a_pid - s2->ap->a_pid;
	return (i);
}

vsize(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;
	register struct vsav *vp = sp->s_un.vp;
	
	if (ap->a_flag & SLOAD)
		return (ap->a_rss +
		    ap->a_txtrss / (ap->a_xccount ? ap->a_xccount : 1));
	return (vp->v_swrss + (ap->a_xccount ? 0 : vp->v_txtswrss));
}

#include <utmp.h>

struct	utmp utmp;
#define	NMAX	(sizeof (utmp.ut_name))
#define SCPYN(a, b)	strncpy(a, b, NMAX)

#define NUID	64

struct ncache {
	int	uid;
	char	name[NMAX+1];
} nc[NUID];

/*
 * This function assumes that the password file is hashed
 * (or some such) to allow fast access based on a uid key.
 */
char *
getname(uid)
{
	register struct passwd *pw;
	struct passwd *getpwent();
	register int cp;
#ifdef notdef
	extern int _pw_stayopen;

	_pw_stayopen = 1;
#endif

#if	(((NUID) & ((NUID) - 1)) != 0)
	cp = uid % (NUID);
#else
	cp = uid & ((NUID) - 1);
#endif
	if (uid >= 0 && nc[cp].uid == uid && nc[cp].name[0])
		return (nc[cp].name);
	pw = getpwuid(uid);
	if (!pw)
		return (0);
	nc[cp].uid = uid;
	SCPYN(nc[cp].name, pw->pw_name);
	return (nc[cp].name);
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

/*
 * This routine was stolen from adb to simulate memory management
 * on the VAX.
 */
off_t
vtophys(loc)
long	loc;
{
	register	p;
	off_t	newloc;

	newloc = loc & ~KERNBASE;
	p = btop(newloc);
	if ((loc & KERNBASE) == 0) {
		fprintf(stderr, "Vtophys: translating non-kernel address\n");
		return((off_t) -1);
	}
	if (p >= Syssize) {
		fprintf(stderr, "Vtophys: page out of bound (%d>=%d)\n",
			p, Syssize);
		return((off_t) -1);
	}
	if (Sysmap[p].pg_v == 0
	&& (Sysmap[p].pg_fod || Sysmap[p].pg_pfnum == 0)) {
		fprintf(stderr, "Vtophys: page not valid\n");
		return((off_t) -1);
	}
	loc = (long) (ptob(Sysmap[p].pg_pfnum) + (loc & PGOFSET));
	return(loc);
}

/*
 * since we can't init unions, the cleanest way to use a.out.h instead
 * of nlist.h (required since nlist() uses some defines) is to do a
 * runtime copy into the nl array -- sigh
 */
init_nlist()
{
	register struct nlist *np;
	register char **namep;

	nllen = sizeof nl_names / sizeof (char *);
	np = nl = (struct nlist *) malloc(nllen * sizeof (struct nlist));
	if (np == NULL) {
		fprintf(stderr, "ps: out of memory allocating namelist\n");
		exit(1);
	}
	namep = &nl_names[0];
	while (nllen > 0) {
		np->n_un.n_name = *namep;
		if (**namep == '\0')
			break;
		namep++;
		np++;
	}
}

/*
 * nlist - retreive attributes from name list (string table version)
 * 	modified to add wait channels - Charles R. LaBrec 8/85
 */
nlist(name, list)
	char *name;
	struct nlist *list;
{
	register struct nlist *p, *q;
	register char *s1, *s2;
	register n, m;
	int maxlen, nreq;
	FILE *f;
	FILE *sf;
	off_t sa;		/* symbol address */
	off_t ss;		/* start of strings */
	int type;
	struct exec buf;
	struct nlist space[BUFSIZ/sizeof (struct nlist)];
	char nambuf[BUFSIZ];

	maxlen = 0;
	for (q = list, nreq = 0; q->n_un.n_name && q->n_un.n_name[0]; q++, nreq++) {
		q->n_type = 0;
		q->n_value = 0;
		q->n_desc = 0;
		q->n_other = 0;
		n = strlen(q->n_un.n_name);
		if (n > maxlen)
			maxlen = n;
	}
	f = fopen(name, "r");
	if (f == NULL)
		return (-1);
	fread((char *)&buf, sizeof buf, 1, f);
	if (N_BADMAG(buf)) {
		fclose(f);
		return (-1);
	}
	sf = fopen(name, "r");
	if (sf == NULL) {
		/* ??? */
		fclose(f);
		return(-1);
	}
	sa = N_SYMOFF(buf);
	ss = sa + buf.a_syms;
	n = buf.a_syms;
	fseek(f, sa, 0);
	while (n) {
		m = sizeof (space);
		if (n < m)
			m = n;
		if (fread((char *)space, m, 1, f) != 1)
			break;
		n -= m;
		for (q = space; (m -= sizeof(struct nlist)) >= 0; q++) {
			if (q->n_un.n_strx == 0 || q->n_type & N_STAB)
				continue;
			/*
			 * since we know what type of symbols we will get,
			 * we can make a quick check here -- crl
			 */
			type = q->n_type & (N_TYPE | N_EXT);
			if ((q->n_type & N_TYPE) != N_ABS
			    && type != (N_EXT | N_DATA)
			    && type != (N_EXT | N_BSS))
				continue;
			fseek(sf, ss+q->n_un.n_strx, 0);
			fread(nambuf, maxlen+1, 1, sf);
			/* if using wchans, add it to the list of channels */
			if (!nflg)
				addchan(&nambuf[1], (caddr_t) q->n_value);
			for (p = list; p->n_un.n_name && p->n_un.n_name[0]; p++) {
				s1 = p->n_un.n_name;
				s2 = nambuf;
				if (strcmp(p->n_un.n_name, nambuf) == 0) {
					p->n_value = q->n_value;
					p->n_type = q->n_type;
					p->n_desc = q->n_desc;
					p->n_other = q->n_other;
					--nreq;
					break;
				}
			}
		}
	}
alldone:
	fclose(f);
	fclose(sf);
	return (nreq);
}

/*
 * add the given channel to the channel list
 */
addchan(name, caddr)
char *name;
caddr_t caddr;
{
	static int left = 0;
	register struct wchan *wp;
	register char **p;

	for (p = wchan_stop_list; *p; p++) {
		if (**p != *name)	/* quick check first */
			continue;
		if (strncmp(name, *p, WNAMESIZ) == 0)
			return;		/* if found, don't add */
	}
	if (left == 0) {
		if (wchanhd) {
			left = 100;
			wchanhd = (struct wchan *) realloc(wchanhd,
				(nchans + left) * sizeof (struct wchan));
		} else {
			left = 600;
			wchanhd = (struct wchan *) malloc(left
				* sizeof (struct wchan));
		}
		if (wchanhd == NULL) {
			fprintf(stderr, "ps: out of memory allocating wait channels\n");
			nflg++;
			return;
		}
	}
	left--;
	wp = &wchanhd[nchans++];
	strncpy(wp->wc_name, name, WNAMESIZ);
	wp->wc_name[WNAMESIZ] = '\0';
	wp->wc_caddr = caddr;
}

/*
 * returns the symbolic wait channel corresponding to chan
 */
char *
getchan(chan)
register caddr_t chan;
{
	register i, iend;
	register char *prevsym;
	register struct wchan *wp;

	prevsym = "???";		/* nothing, to begin with */
	if (chan) {
		for (i = 0; i < NWCINDEX; i++)
			if ((unsigned) chan < (unsigned) wchan_index[i])
				break;
		iend = i--;
		if (i < 0)		/* can't be found */
			return prevsym;
		iend *= nchans;
		iend /= NWCINDEX;
		i *= nchans;
		i /= NWCINDEX;
		wp = &wchanhd[i];
		for ( ; i < iend; i++, wp++) {
			if ((unsigned) wp->wc_caddr > (unsigned) chan)
				break;
			prevsym = wp->wc_name;
		}
	}
	return prevsym;
}

/*
 * used in sorting the wait channel array
 */
int
wchancomp (w1, w2)
struct wchan *w1, *w2;
{
	register unsigned c1, c2;

	c1 = (unsigned) w1->wc_caddr;
	c2 = (unsigned) w2->wc_caddr;
	if (c1 > c2)
		return 1;
	else if (c1 == c2)
		return 0;
	else
		return -1;
}
