/*
 * ps; VAX 4BSD version
 */

#include <stdio.h>
#include <ctype.h>
#include <nlist.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/tty.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/pte.h>
#include <sys/vm.h>
#include <sys/text.h>
#include <sys/stat.h>

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
	{ 0 },
};

struct	savcom {
	union {
		struct	lsav *lp;
		float	u_pctcpu;
		struct	vsav *vp;
		int	s_ssiz;
	} sun;
	struct	asav *ap;
} savcom[NPROC];

struct	asav {
	char	*a_cmdp;
	int	a_flag;
	short	a_stat, a_uid, a_pid, a_nice, a_pri, a_slptime, a_time;
	size_t	a_size, a_rss;
	char	a_tty[DIRSIZ+1];
	dev_t	a_ttyd;
	time_t	a_cpu;
};

char	*lhdr;
/*	     F UID   PID  PPID CP PRI NI ADDR  SZ  RSS WCHAN S  TT  TIME */
struct	lsav {
	short	l_ppid;
	char	l_cpu;
	int	l_addr;
	caddr_t	l_wchan;
};

char	*uhdr;
/*	USER       PID %CPU NICE  SZ  RSS TT F  TIME */

char	*shdr;
/*	SSIZ   PID TT S  TIME */

char	*vhdr;
/*	F     PID TT  TIME RES SL  MINFLT  MAJFLT SIZE  RSS  SRS TSIZ TRS PF*/
struct	vsav {
	u_int	v_minflt, v_majflt;
	size_t	v_swrss, v_tsiz, v_txtrss, v_txtswrss;
	short	v_xccount;
	short	v_aveflt;
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
int	aflg, cflg, eflg, gflg, kflg, lflg, sflg, uflg, vflg, xflg;
char	*tptr;
char	*gettty(), *getcmd(), *getname(), *savestr(), *alloc(), *state();
int	pscomp();
int	nswap, maxslp;
struct	pte *Usrptma, *usrpt;

struct	ttys {
	char	name[DIRSIZ+1];
	dev_t	ttyd;
	struct	ttys *next;
	struct	ttys *cand;
} *allttys, *cand[16];

struct	savcom savcom[NPROC];
int	npr;

int	cmdstart;
int	twidth;
char	*kmemf, *memf, *swapf, *nlistf;
int	kmem, mem, swap;

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

	if (chdir("/dev") < 0) {
		perror("/dev");
		exit(1);
	}
	twidth = 80;
	setbuf(stdout, _sobuf);
	argc--, argv++;
	if (argc > 0) {
		ap = argv[0];
		while (*ap) switch (*ap++) {

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
		case 's':
			sflg++;
			break;
		case 't':
			if (*ap)
				tptr = ap;
			aflg++;
			gflg++;
			if (*tptr == '?')
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
			if (twidth == 80)
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
	getdev();
	uid = getuid();
	printhdr();
	procp = nl[X_PROC].n_value;
	for (i=0; i<NPROC; i += 8) {
		lseek(kmem, (char *)procp, 0);
		j = NPROC - i;
		if (j > 8)
			j = 8;
		j *= sizeof (struct proc);
		if (read(kmem, (char *)proc, j) != j)
			cantread("proc table", kmemf);
		procp += j;
		for (j = j / sizeof (struct proc) - 1; j >= 0; j--) {
			mproc = &proc[j];
			if (mproc->p_stat == 0 ||
			    mproc->p_pgrp == 0 && xflg == 0)
				continue;
			if (tptr == 0 && gflg == 0 && xflg == 0 &&
			    mproc->p_ppid == 1 && (mproc->p_flag&SDETACH) == 0)
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
		else
			printf(" %.*s", twidth - cmdstart - 2, sp->ap->a_cmdp);
		printf("\n");
	}
	exit(npr == 0);
}

openfiles(argc, argv)
	char **argv;
{

	kmemf = "kmem";
	if (kflg)
		kmemf = argc > 1 ? argv[1] : "/vmcore";
	kmem = open(kmemf, 0);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	if (kflg)  {
		mem = kmem;
		memf = kmemf;
	} else {
		memf = "mem";
		mem = open(memf, 0);
		if (mem < 0) {
			perror(memf);
			exit(1);
		}
	}
	swapf = argc>2 ? argv[2]: "drum";
	swap = open(swapf, 0);
	if (swap < 0) {
		perror(swapf);
		exit(1);
	}
}

getkvars(argc, argv)
	char **argv;
{
	register struct nlist *nlp;

	nlistf = argc > 3 ? argv[3] : "/vmunix";
	nlist(nlistf, nl);
	if (nl[0].n_type == 0) {
		fprintf(stderr, "%s: No namelist\n", nlistf);
		exit(1);
	}
	if (kflg)
		for (nlp = nl; nlp < &nl[sizeof (nl)/sizeof (nl[0])]; nlp++)
			nlp->n_value = clear(nlp->n_value);
	Usrptma = (struct pte *)nl[X_USRPTMA].n_value;
	usrpt = (struct pte *)nl[X_USRPT].n_value;
	lseek(kmem, (long)nl[X_NSWAP].n_value, 0);
	if (read(kmem, &nswap, sizeof (nswap)) != sizeof (nswap)) {
		cantread("nswap", kmemf);
		exit(1);
	}
	lseek(kmem, (long)nl[X_MAXSLP].n_value, 0);
	if (read(kmem, &maxslp, sizeof (maxslp)) != sizeof (maxslp)) {
		cantread("maxslp", kmemf);
		exit(1);
	}
	if (vflg) {
		text = (struct text *)alloc(NTEXT * sizeof (struct text));
		if (text == 0) {
			fprintf(stderr, "no room for text table\n");
			exit(1);
		}
		lseek(kmem, (long)nl[X_TEXT].n_value, 0);
		if (read(kmem, (char *)text, NTEXT * sizeof (struct text))
		    != NTEXT * sizeof (struct text)) {
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
	hdr = lflg ? lhdr : (vflg ? vhdr : (uflg ? uhdr : shdr));
	if (lflg+vflg+uflg+sflg == 0)
		hdr += strlen("SSIZ ");
	cmdstart = strlen(hdr);
	printf("%s COMMAND\n", hdr);
	fflush(stdout);
}

cantread(what, fromwhat)
	char *what, *fromwhat;
{

	fprintf(stderr, "ps: error reading %s from %s", what, fromwhat);
}

struct	direct dbuf;
int	dialbase;

getdev()
{
	register FILE *df;
	register struct ttys *dp;

	dialbase = -1;
	if ((df = fopen(".", "r")) == NULL) {
		fprintf(stderr, "Can't open . in /dev\n");
		exit(1);
	}
	while (fread((char *)&dbuf, sizeof(dbuf), 1, df) == 1) {
		if (dbuf.d_ino == 0)
			continue;
		maybetty(dp);
	}
	fclose(df);
}

/*
 * Attempt to avoid stats by guessing minor device
 * numbers from tty names.  Console is known,
 * know that r(hp|up|mt) are unlikely as are different mem's,
 * floppy, null, tty, etc.
 */
maybetty()
{
	register char *cp = dbuf.d_name;
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
			return (0);
		break;

	case 'f':
		if (!strcmp(cp, "floppy"))
			return (0);
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
				return (0);
		}
		break;

	case 'm':
trymem:
		if (cp[0] == 'm' && cp[1] == 'e' && cp[2] == 'm' && cp[3] == 0)
			return (0);
		break;

	case 'n':
		if (!strcmp(cp, "null"))
			return (0);
		break;

	case 'v':
		if ((cp[1] == 'a' || cp[1] == 'p') && isdigit(cp[2]) &&
		    cp[3] == 0)
			return (0);
		break;
	}
mightbe:
	cp = dbuf.d_name;
	while (cp < &dbuf.d_name[DIRSIZ] && *cp)
		cp++;
	--cp;
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
	if (cp > dbuf.d_name && isdigit(cp[-1]) && isdigit(*cp))
		x += 10 * (cp[-1] - ' ') + cp[0] - '0';
	else if (*cp >= 'a' && *cp <= 'f')
		x += 10 + *cp - 'a';
	else if (isdigit(*cp))
		x += *cp - '0';
	else
		x = -1;
donecand:
	dp = (struct ttys *)alloc(sizeof (struct ttys));
	strncpy(dp->name, dbuf.d_name, DIRSIZ);
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
			if (stat(dp->name, &stb) == 0)
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
	char *ttyp, *cmdp;

	if (mproc->p_stat != SZOMB && getu() == 0)
		return;
	ttyp = gettty();
	if (xflg == 0 && ttyp[0] == '?' || tptr && strcmpn(tptr, ttyp, 2))
		return;
	sp = &savcom[npr];
	cmdp = getcmd();
	if (cmdp == 0)
		return;
	sp->ap = ap = (struct asav *)alloc(sizeof (struct asav));
	sp->ap->a_cmdp = cmdp;
#define e(a,b) ap->a = mproc->b
	e(a_flag, p_flag); e(a_stat, p_stat); e(a_nice, p_nice);
	e(a_uid, p_uid); e(a_pid, p_pid); e(a_rss, p_rssize); e(a_pri, p_pri);
	e(a_slptime, p_slptime); e(a_time, p_time);
#undef e
	ap->a_tty[0] = ttyp[0];
	ap->a_tty[1] = ttyp[1] ? ttyp[1] : ' ';
	if (ap->a_stat == SZOMB) {
		register struct xproc *xp = (struct xproc *)mproc;

		ap->a_cpu = xp->xp_vm.vm_utime + xp->xp_vm.vm_stime;
	} else {
		ap->a_size = mproc->p_dsize + mproc->p_ssize;
		ap->a_ttyd = u.u_ttyd;
		ap->a_cpu = u.u_vm.vm_utime + u.u_vm.vm_stime;
	}
	ap->a_cpu /= HZ;
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
		register struct text *xp;

		sp->sun.vp = vp = (struct vsav *)alloc(sizeof (struct vsav));
#define e(a,b) vp->a = mproc->b
		if (ap->a_stat != SZOMB) {
			e(v_swrss, p_swrss); e(v_aveflt, p_aveflt);
			vp->v_minflt = u.u_vm.vm_minflt;
			vp->v_majflt = u.u_vm.vm_majflt;
			if (mproc->p_textp) {
				xp = &text[mproc->p_textp -
				    (struct text *)nl[X_TEXT].n_value];
				vp->v_tsiz = xp->x_size;
				vp->v_txtrss = xp->x_rssize;
				vp->v_txtswrss = xp->x_swrss;
				vp->v_xccount = xp->x_ccount;
			}
		}
#undef e
	} else if (uflg)
		sp->sun.u_pctcpu = 0.0;
	else if (sflg) {
		if (ap->a_stat != SZOMB) {
			for (cp = (char *)u.u_stack;
			    cp < &user.upages[UPAGES][NBPG]; )
				if (*cp++)
					break;
			sp->sun.s_ssiz = (&user.upages[UPAGES][NBPG] - cp);
		}
	}
	npr++;
}

getu()
{
	struct pte *pteaddr, apte, arguutl[UPAGES+CLSIZE];
	register int i;
	int ncl, size;

	size = sflg ? ctob(UPAGES) : sizeof (struct user);
	if ((mproc->p_flag & SLOAD) == 0) {
		lseek(swap, ctob(mproc->p_swaddr), 0);
		if (read(swap, (char *)&user.user, size) != size) {
			fprintf(stderr, "ps: cant read u for pid %d from %s\n",
			    mproc->p_pid, swapf);
			return (0);
		}
		pcbpf = 0;
		argaddr = 0;
		return (1);
	}
	pteaddr = &Usrptma[btokmx(mproc->p_p0br) + mproc->p_szpt - 1];
	lseek(kmem, kflg ? clear(pteaddr) : (int)pteaddr, 0);
	if (read(kmem, (char *)&apte, sizeof(apte)) != sizeof(apte)) {
		printf("ps: cant read indir pte to get u for pid %d from %s\n",
		    mproc->p_pid, swapf);
		return (0);
	}
	lseek(mem,
	    ctob(apte.pg_pfnum+1) - (UPAGES+CLSIZE) * sizeof (struct pte), 0);
	if (read(mem, (char *)arguutl, sizeof(arguutl)) != sizeof(arguutl)) {
		printf("ps: cant read page table for u of pid %d from %s\n",
		    mproc->p_pid, swapf);
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
		lseek(mem, ctob(arguutl[CLSIZE+i].pg_pfnum), 0);
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

	if (mproc->p_stat == SZOMB || mproc->p_flag&(SSYS|SWEXIT))
		return ("");
	if (cflg) {
		strncpy(cmdbuf, u.u_comm, sizeof (u.u_comm));
		return (savestr(cmdbuf));
	}
	if ((mproc->p_flag & SLOAD) == 0 || argaddr == 0) {
		vstodb(0, CLSIZE, &u.u_smap, &db, 1);
		lseek(swap, ctob(db.db_base), 0);
		if (read(swap, (char *)&argspac, sizeof(argspac))
		    != sizeof(argspac))
			goto bad;
	} else {
		lseek(mem, argaddr, 0);
		if (read(mem, (char *)&argspac, sizeof (argspac))
		    != sizeof (argspac))
			goto bad;
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
	strncpy(cmdbuf, cp, &argspac.argc[CLSIZE*NBPG] - cp);
	if (cp[0] == '-' || cp[0] == '?' || cp[0] <= ' ') {
		strcat(cmdbuf, " (");
		strncat(cmdbuf, u.u_comm, sizeof(u.u_comm));
		strcat(cmdbuf, ")");
	}
/*
	if (xflg == 0 && gflg == 0 && tptr == 0 && cp[0] == '-')
		return (0);
*/
	return (savestr(cmdbuf));

bad:
	fprintf(stderr, "ps: error locating command name for pid %d\n",
	    mproc->p_pid);
retucomm:
	strcpy(cmdbuf, " (");
	strncat(cmdbuf, u.u_comm, sizeof (u.u_comm));
	strcat(cmdbuf, ")");
	return (savestr(cmdbuf));
}

char	*lhdr =
"     F UID   PID  PPID CP PRI NI ADDR  SZ  RSS WCHAN S  TT  TIME";
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
	printf(" %2.2s ", state(ap));
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

	printf("%3ld:%02ld", ap->a_cpu / HZ, ap->a_cpu % HZ);
}

char	*uhdr =
"USER       PID %CPU NICE  SZ  RSS TT F   TIME";
upr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;

	printf("%-8.8s %5u%5.1f%5d%4d%5d",
	    getname(ap->a_uid), ap->a_pid, sp->sun.u_pctcpu, ap->a_nice-NZERO,
	    ap->a_size/2, ap->a_rss/2);
	putchar(' ');
	ptty(ap->a_tty);
	printf(" %2.2s", state(ap));
	ptime(ap);
}

char *vhdr =
"   PID TT S     TIME RES SL MINFLT MAJFLT SIZE  RSS  SRS TSIZ TRS PF";
vpr(sp)
	struct savcom *sp;
{
	register struct vsav *vp = sp->sun.vp;
	register struct asav *ap = sp->ap;

	printf("%6u ", ap->a_pid);
	ptty(ap->a_tty);
	printf(" %4s", state(ap));
	ptime(ap);
	printf("%4d%3d%7d%7d%5d%5d%5d%5d%4d%3d",
	   ap->a_time, ap->a_slptime, vp->v_minflt, vp->v_majflt,
	   ap->a_size/2, ap->a_rss/2, vp->v_swrss/2,
	   vp->v_tsiz/2, vp->v_txtrss/2, vp->v_aveflt);
}

char	*shdr =
"SSIZ   PID TT S   TIME";
spr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;

	if (sflg)
		printf("%4d ", sp->sun.s_ssiz);
	printf("%5u", ap->a_pid);
	putchar(' ');
	ptty(ap->a_tty);
	printf(" %2.2s", state(ap));
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
	load = ap->a_flag & SLOAD ? ' ' : 'W';
	nice = ap->a_nice > NZERO ? 'N' : ' ';
	anom = ap->a_flag & (SANOM|SUANOM) ? 'A' : ' ';
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
		    vp->v_txtrss / (vp->v_xccount ? vp->v_xccount : 1));
	return (vp->v_swrss + (vp->v_xccount ? 0 : vp->v_txtswrss));
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

	if (names[uid][0])
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
		strncpy(names[pw->pw_uid], pw->pw_name, NMAX);
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
		freebase = (char *)sbrk(i = size > 2048 ? size : 2048);
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
	strcpy(dp, cp);
	return (dp);
}
