#ifndef lint
static	char *sccsid = "@(#)ps.c	4.18 (Berkeley) %G%";
#endif

/*
 * ps
 */

#include <stdio.h>
#include <ctype.h>
#include <nlist.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/tty.h>
#include <dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/pte.h>
#include <sys/vm.h>
#include <sys/text.h>
#include <sys/stat.h>
#include <math.h>
#include <sys/vlimit.h>

struct nlist nl[] = {
	{ "_proc" },
#define	X_PROC		0
	{ "_Usrptmap" },
#define	X_USRPTMA	1
	{ "_usrpt" },
#define	X_USRPT		2
	{ "_text" },
#define	X_TEXT		3
	{ "_nswap" },
#define	X_NSWAP		4
	{ "_maxslp" },
#define	X_MAXSLP	5
	{ "_ccpu" },
#define	X_CCPU		6
	{ "_ecmx" },
#define	X_ECMX		7
	{ "_nproc" },
#define	X_NPROC		8
	{ "_ntext" },
#define	X_NTEXT		9
	{ "_hz" },
#define	X_HZ		10
	{ 0 },
};

struct	savcom {
	union {
		struct	lsav *lp;
		float	u_pctcpu;
		struct	vsav *vp;
		int	s_ssiz;
		struct	sssav *ssp;
	} sun;
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
struct	lsav {
	short	l_ppid;
	char	l_cpu;
	int	l_addr;
	caddr_t	l_wchan;
};

char	*sshdr;
struct	sssav {
	short 	ss_ppid;
	short	ss_brother;
	short	ss_sons;
};

char	*uhdr;
char	*shdr;

char	*vhdr;
struct	vsav {
	u_int	v_majflt;
	size_t	v_swrss, v_txtswrss;
	float	v_pctcpu;
};

struct	proc proc[8];		/* 8 = a few, for less syscalls */
struct	proc *mproc;
struct	text *text;

union {
	struct	user user;
	char	upages[UPAGES][NBPG];
} user;
#define u	user.user

#define clear(x) 	((int)x & 0x7fffffff)

int	chkpid;
int	aflg, cflg, eflg, gflg, kflg, lflg, sflg, ssflg,
	nonssflg, uflg, vflg, xflg;
char	*tptr;
char	*gettty(), *getcmd(), *getname(), *savestr(), *alloc(), *state();
char	*rindex(), *calloc(), *sbrk(), *strcpy(), *strcat(), *strncat();
long	lseek();
double	pcpu(), pmem();
int	pscomp();
int	nswap, maxslp;
struct	text *atext;
double	ccpu;
int	ecmx;
struct	pte *Usrptma, *usrpt;
int	nproc, ntext, hz;

struct	ttys {
	char	name[MAXNAMLEN+1];
	dev_t	ttyd;
	struct	ttys *next;
	struct	ttys *cand;
} *allttys, *cand[16];

int	npr;

int	cmdstart;
int	twidth;
char	*kmemf, *memf, *swapf, *nlistf;
int	kmem, mem, swap = -1;
int	rawcpu, sumcpu;

int	pcbpf;
int	argaddr;
extern	char _sobuf[];

main(argc, argv)
	char **argv;
{
	register int i, j;
	register char *ap;
	int uid;
	off_t procp;

	twidth = 80;
	if (ap = rindex(argv[0], '/'))
		ap++;
	else
		ap = argv[0];
	if (*ap == 's')				/* If name starts with 's' */
		ssflg++;

	argc--, argv++;
	if (argc > 0) {
		ap = argv[0];
		while (*ap) switch (*ap++) {

		case 'C':
			rawcpu++; nonssflg++;
			break;
		case 'S':
			sumcpu++;
			break;
		case 'a':
			aflg++;
			break;
		case 'c':
			cflg = !cflg; nonssflg++;
			break;
		case 'e':
			eflg++; nonssflg++;
			break;
		case 'g':
			gflg++; nonssflg++;
			break;
		case 'k':
			kflg++; nonssflg++;
			break;
		case 'l':
			lflg++; nonssflg++;
			break;
		case 's':
			sflg++; nonssflg++;
			break;
		case 't':
			if (*ap)
				tptr = ap;
			aflg++; nonssflg++;
			gflg++;
			if (*tptr == '?')
				xflg++;
			while (*ap)
				ap++;
			break;
		case 'u': 
			uflg++; nonssflg++;
			break;
		case 'v':
			cflg = 1;
			vflg++; nonssflg++;
			break;
		case 'w':
			if (twidth == 80)
				twidth = 132;
			else
				twidth = BUFSIZ;
			break;
		case 'x':
			xflg++;
			break;
		case 'y':		/* Rand 2/81 */
			ssflg++;
			break;
		default:
			if (!isdigit(ap[-1]))
				break;
			chkpid = atoi(--ap);
			*ap = 0;
			aflg++;	nonssflg++;
			xflg++;
			break;
		}
	}
	if (ssflg) {
		if (nonssflg) {
			fprintf (stderr, "Usage: ss [axwS]\n");
			exit(1);
		}
		uflg++;
		gflg++;
	}
	openfiles(argc, argv);
	getkvars(argc, argv);
	if (chdir("/dev") < 0) {
		perror("/dev");
		exit(1);
	}
	getdev();
	uid = getuid();
	printhdr();
	procp = getw(nl[X_PROC].n_value);
	nproc = getw(nl[X_NPROC].n_value);
	hz = getw(nl[X_HZ].n_value);
	savcom = (struct savcom *)calloc(nproc, sizeof (*savcom));
	for (i=0; i<nproc; i += 8) {
		klseek(kmem, (long)procp, 0);
		j = nproc - i;
		if (j > 8)
			j = 8;
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
			if (uid != mproc->p_uid && aflg==0 ||
			    chkpid != 0 && chkpid != mproc->p_pid)
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
	qsort(savcom, npr, sizeof(savcom[0]), pscomp);
	if (ssflg) {
		walk(npr);
		exit (npr == 0);
	}
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
		if (sp->ap->a_flag & SWEXIT)
			printf(" <exiting>");
		else if (sp->ap->a_stat == SZOMB)
			printf(" <defunct>");
		else if (sp->ap->a_pid == 0)
			printf(" swapper");
		else if (sp->ap->a_pid == 2)
			printf(" pagedaemon");
		else if (sp->ap->a_pid == 3 && sp->ap->a_flag & SSYS)
			printf(" ip input");
		else
			printf(" %.*s", twidth - cmdstart - 2, sp->ap->a_cmdp);
		printf("\n");
	}
	exit(npr == 0);
}

getw(loc)
	unsigned long loc;
{
	long word;

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

	if (kflg)
		loc &= 0x7fffffff;
	(void) lseek(fd, (long)loc, off);
}

openfiles(argc, argv)
	char **argv;
{

	kmemf = "/dev/kmem";
	if (kflg)
		kmemf = argc > 2 ? argv[2] : "/vmcore";
	kmem = open(kmemf, 0);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	if (kflg)  {
		mem = kmem;
		memf = kmemf;
	} else {
		memf = "/dev/mem";
		mem = open(memf, 0);
		if (mem < 0) {
			perror(memf);
			exit(1);
		}
	}
	if (kflg == 0 || argc > 3) {
		swapf = argc>3 ? argv[3]: "/dev/drum";
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
	register struct nlist *nlp;

	nlistf = argc > 1 ? argv[1] : "/vmunix";
	nlist(nlistf, nl);
	if (nl[0].n_type == 0) {
		fprintf(stderr, "%s: No namelist\n", nlistf);
		exit(1);
	}
	if (kflg)
		for (nlp = nl; nlp < &nl[sizeof (nl)/sizeof (nl[0])]; nlp++)
			nlp->n_value = clear(nlp->n_value);
	usrpt = (struct pte *)nl[X_USRPT].n_value;	/* don't clear!! */
	Usrptma = (struct pte *)nl[X_USRPTMA].n_value;
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
		text = (struct text *)alloc(ntext * sizeof (struct text));
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
}

printhdr()
{
	char *hdr;

	if (sflg+lflg+vflg+uflg > 1) {
		fprintf(stderr, "ps: specify only one of s,l,v and u\n");
		exit(1);
	}
	hdr = ssflg ? sshdr :
		(lflg ? lhdr : 
			(vflg ? vhdr : 
				(uflg ? uhdr : shdr))); 
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

	dialbase = -1;
	if ((df = opendir(".")) == NULL) {
		fprintf(stderr, "Can't open . in /dev\n");
		exit(1);
	}
	while ((dbuf = readdir(df)) != NULL) 
		maybetty();
	closedir(df);
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
	register struct ttys *dp;
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
		if (*cp == 'r' || *cp == 'u' || *cp == 'h')
			cp++;
#define is(a,b) cp[0] == 'a' && cp[1] == 'b'
		if (is(r,p) || is(u,p) || is(r,k) || is(r,m) || is(m,t)) {
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
	dp = (struct ttys *)alloc(sizeof (struct ttys));
	(void) strcpy(dp->name, dbuf->d_name);
	dp->next = allttys;
	dp->ttyd = -1;
	allttys = dp;
	if (x == -1)
		return;
	x &= 017;
	dp->cand = cand[x];
	cand[x] = dp;
}

char *
gettty()
{
	register char *p;
	register struct ttys *dp;
	struct stat stb;
	int x;

	if (u.u_ttyp == 0)
		return("?");
	x = u.u_ttyd & 017;
	for (dp = cand[x]; dp; dp = dp->cand) {
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
	for (dp = allttys; dp; dp = dp->next) {
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
	sp->ap = ap = (struct asav *)alloc(sizeof (struct asav));
	sp->ap->a_cmdp = cmdp;
#define e(a,b) ap->a = mproc->b
	e(a_flag, p_flag); e(a_stat, p_stat); e(a_nice, p_nice);
	e(a_uid, p_uid); e(a_pid, p_pid); e(a_pri, p_pri);
	e(a_slptime, p_slptime); e(a_time, p_time);
	ap->a_tty[0] = ttyp[0];
	ap->a_tty[1] = ttyp[1] ? ttyp[1] : ' ';
	if (ap->a_stat == SZOMB) {
		register struct xproc *xp = (struct xproc *)mproc;

		ap->a_cpu = xp->xp_vm.vm_utime + xp->xp_vm.vm_stime;
	} else {
		ap->a_size = mproc->p_dsize + mproc->p_ssize;
		e(a_rss, p_rssize); 
		ap->a_ttyd = u.u_ttyd;
		ap->a_cpu = u.u_vm.vm_utime + u.u_vm.vm_stime;
		if (sumcpu)
			ap->a_cpu += u.u_cvm.vm_utime + u.u_cvm.vm_stime;
		if (mproc->p_textp && text) {
			xp = &text[mproc->p_textp - atext];
			ap->a_tsiz = xp->x_size;
			ap->a_txtrss = xp->x_rssize;
			ap->a_xccount = xp->x_ccount;
		}
	}
#undef e
	ap->a_cpu /= hz;
	ap->a_maxrss = mproc->p_maxrss;
	if (lflg) {
		register struct lsav *lp;

		sp->sun.lp = lp = (struct lsav *)alloc(sizeof (struct lsav));
#define e(a,b) lp->a = mproc->b
		e(l_ppid, p_ppid); e(l_cpu, p_cpu);
		if (ap->a_stat != SZOMB)
			e(l_wchan, p_wchan);
#undef e
		lp->l_addr = pcbpf;
	} else if (vflg) {
		register struct vsav *vp;

		sp->sun.vp = vp = (struct vsav *)alloc(sizeof (struct vsav));
#define e(a,b) vp->a = mproc->b
		if (ap->a_stat != SZOMB) {
			e(v_swrss, p_swrss);
			vp->v_majflt = u.u_vm.vm_majflt;
			if (mproc->p_textp)
				vp->v_txtswrss = xp->x_swrss;
		}
		vp->v_pctcpu = pcpu();
#undef e
	} else if (uflg) {
		if (!ssflg)
			sp->sun.u_pctcpu = pcpu();
		else {
			register struct sssav *ssp;

			sp->sun.ssp =ssp= (struct sssav *)alloc(sizeof (struct sssav));
			ssp->ss_ppid = mproc->p_ppid;
		}
	} else if (sflg) {
		if (ap->a_stat != SZOMB) {
			for (cp = (char *)u.u_stack;
			    cp < &user.upages[UPAGES][0]; )
				if (*cp++)
					break;
			sp->sun.s_ssiz = (&user.upages[UPAGES][0] - cp);
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

double
pcpu()
{
	time_t time;

	time = mproc->p_time;
	if (time == 0 || (mproc->p_flag&SLOAD) == 0)
		return (0.0);
	if (rawcpu)
		return (100.0 * mproc->p_pctcpu);
	return (100.0 * mproc->p_pctcpu / (1.0 - exp(time * log(ccpu))));
}

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
		(void) lseek(swap, (long)ctob(mproc->p_swaddr), 0);
		if (read(swap, (char *)&user.user, size) != size) {
			fprintf(stderr, "ps: cant read u for pid %d from %s\n",
			    mproc->p_pid, swapf);
			return (0);
		}
		pcbpf = 0;
		argaddr = 0;
		return (1);
	}
	if (kflg)
		mproc->p_p0br = (struct pte *)clear(mproc->p_p0br);
	pteaddr = &Usrptma[btokmx(mproc->p_p0br) + mproc->p_szpt - 1];
	klseek(kmem, (long)pteaddr, 0);
	if (read(kmem, (char *)&apte, sizeof(apte)) != sizeof(apte)) {
		printf("ps: cant read indir pte to get u for pid %d from %s\n",
		    mproc->p_pid, swapf);
		return (0);
	}
	klseek(mem,
	    (long)ctob(apte.pg_pfnum+1) - (UPAGES+CLSIZE) * sizeof (struct pte),
		0);
	if (read(mem, (char *)arguutl, sizeof(arguutl)) != sizeof(arguutl)) {
		printf("ps: cant read page table for u of pid %d from %s\n",
		    mproc->p_pid, kmemf);
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
		klseek(mem, (long)ctob(arguutl[CLSIZE+i].pg_pfnum), 0);
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
	char cmdbuf[BUFSIZ];
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
		(void) lseek(swap, (long)ctob(db.db_base), 0);
		if (read(swap, (char *)&argspac, sizeof(argspac))
		    != sizeof(argspac))
			goto bad;
		file = swapf;
	} else {
		klseek(mem, (long)argaddr, 0);
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
/*
	if (xflg == 0 && gflg == 0 && tptr == 0 && cp[0] == '-')
		return (0);
*/
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
"     F UID   PID  PPID CP PRI NI ADDR  SZ  RSS WCHAN STAT TT  TIME";
lpr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;
	register struct lsav *lp = sp->sun.lp;

	printf("%6x%4d%6u%6u%3d%4d%3d%5x%4d%5d",
	    ap->a_flag, ap->a_uid,
	    ap->a_pid, lp->l_ppid, lp->l_cpu&0377, ap->a_pri-PZERO,
	    ap->a_nice-NZERO, lp->l_addr, ap->a_size/2, ap->a_rss/2);
	printf(lp->l_wchan ? " %5x" : "      ", (int)lp->l_wchan&0xfffff);
	printf(" %4.4s ", state(ap));
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

	printf("%3ld:%02ld", ap->a_cpu / hz, ap->a_cpu % hz);
}

char	*uhdr =
"USER       PID %CPU %MEM   SZ  RSS TT STAT  TIME";
upr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;
	int vmsize, rmsize;

	vmsize = (ap->a_size + ap->a_tsiz)/2;
	rmsize = ap->a_rss/2;
	if (ap->a_xccount)
		rmsize += ap->a_txtrss/ap->a_xccount/2;
	printf("%-8.8s %5d%5.1f%5.1f%5d%5d",
	    getname(ap->a_uid), ap->a_pid, sp->sun.u_pctcpu, pmem(ap),
	    vmsize, rmsize);
	putchar(' ');
	ptty(ap->a_tty);
	printf(" %4.4s", state(ap));
	ptime(ap);
}

char *vhdr =
"  PID TT STAT  TIME SL RE PAGEIN SIZE  RSS  LIM TSIZ TRS %CPU %MEM";
vpr(sp)
	struct savcom *sp;
{
	register struct vsav *vp = sp->sun.vp;
	register struct asav *ap = sp->ap;

	printf("%5u ", ap->a_pid);
	ptty(ap->a_tty);
	printf(" %4.4s", state(ap));
	ptime(ap);
	printf("%3d%3d%7d%5d%5d",
	   ap->a_slptime > 99 ? 99 : ap-> a_slptime,
	   ap->a_time > 99 ? 99 : ap->a_time, vp->v_majflt,
	   ap->a_size/2, ap->a_rss/2);
	if (ap->a_maxrss == (INFINITY/NBPG))
		printf("   xx");
	else
		printf("%5d", ap->a_maxrss/2);
	printf("%5d%4d%5.1f%5.1f",
	   ap->a_tsiz/2, ap->a_txtrss/2, vp->v_pctcpu, pmem(ap));
}

char	*shdr =
"SSIZ   PID TT STAT  TIME";
spr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;

	if (sflg)
		printf("%4d ", sp->sun.s_ssiz);
	printf("%5u", ap->a_pid);
	putchar(' ');
	ptty(ap->a_tty);
	printf(" %4.4s", state(ap));
	ptime(ap);
}

char *
state(ap)
	register struct asav *ap;
{
	char stat, load, nice, anom;
	static char res[5];

	switch (ap->a_stat) {

	case SSTOP:
		stat = 'T';
		break;

	case SSLEEP:
		if (ap->a_pri >= PZERO)
			if (ap->a_slptime >= MAXSLP)
				stat = 'I';
			else
				stat = 'S';
		else if (ap->a_flag & SPAGE)
			stat = 'P';
		else
			stat = 'D';
		break;

	case SWAIT:
	case SRUN:
	case SIDL:
		stat = 'R';
		break;

	case SZOMB:
		stat = 'Z';
		break;

	default:
		stat = '?';
	}
	load = ap->a_flag & SLOAD ? (ap->a_rss>ap->a_maxrss ? '>' : ' ') : 'W';
	if (ap->a_nice < NZERO)
		nice = '<';
	else if (ap->a_nice > NZERO)
		nice = 'N';
	else
		nice = ' ';
	anom = (ap->a_flag&SUANOM) ? 'A' : ((ap->a_flag&SSEQL) ? 'S' : ' ');
	res[0] = stat; res[1] = load; res[2] = nice; res[3] = anom;
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
	register int blk = DMMIN;
	register swblk_t *ip = dmp->dm_map;

	if (vsbase < 0 || vsbase + vssize > dmp->dm_size)
		panic("vstodb");
	while (vsbase >= blk) {
		vsbase -= blk;
		if (blk < DMMAX)
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

	if (uflg && !ssflg)
		return (s2->sun.u_pctcpu > s1->sun.u_pctcpu ? 1 : -1);
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
	register struct vsav *vp = sp->sun.vp;
	
	if (ap->a_flag & SLOAD)
		return (ap->a_rss +
		    ap->a_txtrss / (ap->a_xccount ? ap->a_xccount : 1));
	return (vp->v_swrss + (ap->a_xccount ? 0 : vp->v_txtswrss));
}

#define	NMAX	8
#define	NUID	2048

char	names[NUID][NMAX+1];

/*
 * Stolen from ls...
 */
char *
getname(uid)
{
	register struct passwd *pw;
	static init;
	struct passwd *getpwent();

	if (uid >= 0 && uid < NUID && names[uid][0])
		return (&names[uid][0]);
	if (init == 2)
		return (0);
	if (init == 0)
		setpwent(), init = 1;
	while (pw = getpwent()) {
		if (pw->pw_uid >= NUID)
			continue;
		if (names[pw->pw_uid][0])
			continue;
		(void) strncpy(names[pw->pw_uid], pw->pw_name, NMAX);
		if (pw->pw_uid == uid)
			return (&names[uid][0]);
	}
	init = 2;
	endpwent();
	return (0);
}

char	*freebase;
int	nleft;

char *
alloc(size)
	int size;
{
	register char *cp;
	register int i;

	if (size > nleft) {
		freebase = (char *)sbrk((int)(i = size > 2048 ? size : 2048));
		if (freebase == 0) {
			fprintf(stderr, "ps: ran out of memory\n");
			exit(1);
		}
		nleft = i - size;
	} else
		nleft -= size;
	cp = freebase;
	for (i = size; --i >= 0; )
		*cp++ = 0;
	freebase = cp;
	return (cp - size);
}

char *
savestr(cp)
	char *cp;
{
	register int len;
	register char *dp;

	len = strlen(cp);
	dp = (char *)alloc(len+1);
	(void) strcpy(dp, cp);
	return (dp);
}

walk(np)
	int np;
{
	register int i, j, k, l, m;
#undef afl
#undef sfl
#define afl(i,f) savcom[i].ap -> f
#define sfl(i,f) savcom[i].sun.ssp -> f

	for(i = 0; i < np; i = j) {
		for(j = i; afl(j,a_ttyd) == afl(i,a_ttyd); j++) {
			sfl(j,ss_brother) = -1;
			sfl(j,ss_sons) = -1;
		}
		for(k = i+1; k < j; k++) {
			if(sfl(k,ss_ppid) == sfl(i,ss_ppid)) {
				for(l=i; sfl(l,ss_brother) != -1; 
					l=sfl(l,ss_brother)) ;
				sfl(l,ss_brother) = k;
				goto next;
			}
			for(l = i; l < j; l++) {
				if(l == k) continue;
				if(sfl(k,ss_ppid) == afl(l,a_pid)) {
					if(sfl(l,ss_sons) == -1)
					    sfl(l,ss_sons) = k;
					else {
					    for(m = sfl(l,ss_sons);
						sfl(m,ss_brother) != -1;
						m = sfl(m,ss_brother)) ;
					    sfl(m,ss_brother) = k;
					}
					goto next;
				}
			}
			for(l = i; l < j; l++) {
				if(l == k) continue;
				if(sfl(k,ss_ppid) == sfl(l,ss_ppid)) {
					for(m = k; sfl(m,ss_brother) != -1;
					    m = sfl(m,ss_brother)) ;
					sfl(m,ss_brother) = l;
				}
			}
		next: ;
		}
		walk1(i, 0);
	}
}

walk1(pno, depth)
	int pno, depth;
{
	if(pno == -1)
		return;
/***    printf("%5d, %d\n",outargs[pno].o_pid, depth);  ***/
	walkpr(&savcom[pno], depth);
	walk1(sfl(pno,ss_sons), depth+1);
	walk1(sfl(pno,ss_brother), depth);
}

char *sshdr =
"TTY User     SZ RSS  CPU  S      PID  ";

walkpr(a, depth)
	register struct savcom *a;
	int depth;
{

	if(!depth) {
		printf("%-2.2s", a->ap->a_tty);
		printf(" %-8.8s", getname(a->ap->a_uid));
	} else
		printf("   %-8s", &".......*"[8-(depth<=8?depth:8)]);
	printf("%4d%4d", a->ap->a_size/2, a->ap->a_rss/2);
	ptime(a->ap);
	/* Once there was a "CCPU" field here.  Subsumed by -S now. */
	printf(" %4.4s", state(a->ap));
	printf("%6u ", a->ap->a_pid);
	if (a->ap->a_pid == 0)
		printf(" swapper");
	else if (a->ap->a_pid == 2)
		printf(" pagedaemon");
	else
		printf(" %.*s", twidth - cmdstart - 2, a->ap->a_cmdp);
	putchar('\n');
}
