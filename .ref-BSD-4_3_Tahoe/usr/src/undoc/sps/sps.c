#
/*
 ps - show process status - iiasa version (jek)
	originally from harvard and/or CULC

	flags are single letters
	multiple flags can occur in one argument
	dashes are optional but are needed to delimit lists of things
		multiple lists are present(???)
	flags that imply other arguments read the following arguments
		until the end of the list or until an argument starts with dash
	certain flags read exactly one argument
	initialization (i flag) should be done for:
		new users,
		new kernel
	parameters here must change to indicate:
		new tty devices, max tty lines, tty letter changes
		max users
		new things to wait for
	this program should be changed if:
		proc structure makes this obsolete
		etc
	recompilation should occur if:
		kernel structures and or paramters (nproc etc.) change
		any above changes of course

	flags are:
		a - show all processes that have pgrps except shells
		b - show background (not detached)
		c - show child times instead of process times
		d - show detached processes inherited by init
		e - show the environment with the args
		f - show foreground jobs, connected to tty
		g - show processes in given pgrps
		h - unused
		i - perform initialization (implies 'n')
		j,k - unused
		l - print long format. includes most good stuff
		m - specify different memory file (file is next arg)
		n - show no processes
		o - unused
		p - show only processes whose id's are in list (following args)
		q - unused
		r - repeat indefinitely (number of r's = seconds or r#)
		s - show stopped processes
		t - show only processes associated with ttys in list (following)
		u - show only processes (or ancestors of) for users in list
		v - be verbose - show the most information
		w - wide format, show entire argument list (up to 512 chars)
		x - show unattached processes - no pgrp. a+x gives shells also
		y - unused
		z - show zombies
		A - show ALL information possible
		B - show busy processes
		F - go fast, avoid swap space.
		G - print pgrp
		U - use different UNIX file (next arg)
		S - show size
		T - show tty information
		W - print wait channels in hex
*/

#include	<signal.h>
#include	<h/param.h>
#include	<h/dir.h>
#include	<h/user.h>
#include	<h/proc.h>
#include	<h/pte.h>
#include	<h/vm.h>
#include	<h/inode.h>
#include	<h/file.h>
#include	<h/buf.h>
#include	<h/text.h>
#include	<h/tty.h>
#include	<h/conf.h>
#include	<nlist.h>
#include	<sys/stat.h>
#include	<stdio.h>
#include	<pwd.h>


#define	TRUE	1
#define FALSE	0
#define	INTPPG	(NBPG/sizeof(int))
#define MAXARGPG	5

#define Usrptmap		((struct pte *)info.kaddr[ausrptmap])
#define usrpt		((struct pte *)info.kaddr[ausrpt])
#define cswitch		((struct cdevsw *)info.kaddr[acdevsw])
struct proc *proc, *kproc;
struct text *text, *ktext;
struct buf *buf, *swbuf;
struct inode *inode;
int nproc, ntext, hz, nbuf, ninode, nfile, nswbuf;
union {
	struct user user;
	char	upages[UPAGES][NBPG];
} user;
int pad1;	/* supposedly to aviod hardware problem reading /dev/mem */
struct pte pagetable[UPAGES + MAXARGPG];	/* for users page table */
int pad2;	/* supposedly to aviod hardware problem reading /dev/mem */

#define u	user.user
#define MSPID	2		/* max system pid, not to considered BUSY */

#define	MAXUSERS	256	/* total different users */
#define	UNAMELENGTH	8	/* length of a user name */
#define	NSPEC		15	/* number of specified things (proc, user, tty */
#define MAXTTYS		100

/* definitions to reuse uninteresting proc table entries for linked lists */

struct	procinfo	{		/* structure to use for */
	char	*pi_cmd;		/* attaching a time */
	struct ttyline	*pi_tty;
	long	pi_time;		/* and an arg string to a proc */
};

#define p_next		p_link		/* pointer to next proc in global list */
#define	p_bro		p_rlink		/* next process with same parent */
#define	p_son		p_xlink		/* first child of this process */
#define pinfo(p)	(*((struct procinfo **)&p->p_sig))
#define procsize(p)	((p)->p_tsize + (p)->p_dsize + (p)->p_ssize)
					/* size of process */
#define ABS(x) ((int)(x) & ~0x80000000)	/* clear top bit - type int */
#define K 1024
#define KSHIFT	10

#define msize(x)	(x >> (KSHIFT-PGSHIFT))

#define	USERPAGE	0		/* flag for pread - read user page */
#define	TOPMEM		1		/* flag for pread - read top of mem */

struct	proc	*plist;
int		mypid;			/* pid of this process */

char	Aflag, aflag, Bflag, bflag, cflag, dflag, eflag, fflag, Fflag;
char	Gflag, iflag, lflag, mflag, nflag, rflag, Sflag, sflag, Tflag;
char	Uflag, uflag, vflag, wflag, xflag, nxflag, Wflag, zflag;
int select;		/* flag indicating process selection */

int	ntotal, nbusy, nloaded, nswapped;
int	ktotal, kbusy, kloaded, kswapped;

/* specified users, ttys, pids, pgrps */
union numptr {
	int nm_int;
	char *nm_ptr;
};

union ttyptr {
	struct ttyline *ty_line;
	char *ty_ptr;
};

union numptr	pids[NSPEC], *ppids = pids;	/* specified process ids */
union numptr	grps[NSPEC], *pgrps = grps;	/* specified groups */
union numptr	uids[NSPEC], *puids = uids;	/* specified user ids */
union ttyptr	ttys[NSPEC], *pttys = ttys;	/* specified ttys */

/* files needed by ps */

char	*memf = "/dev/mem";		/* default memory file */
int	mem;				/* memory file descriptor */
char	*kmemf = "/dev/kmem";		/* virtual memory file */
int	kmem;				/* virtual memory file descriptor */
char	*symf = "/vmunix";		/* default symbol file */
char	*swapf = "/dev/swap";		/* default swap file */
int	swap;				/* swap area file descriptor */
char	*infof = "/etc/spsinfo";	/* default info save file */
int	infofd;				/* info file descriptor */

/* variables read from the kernel */

struct	nlist	namelist[] = {
#define aproc		0
	{"_proc"},
#define	aswapdev	1
	{"_swapdev"},
#define	aswplo		2
	{"_swplo"},
#define	answbuf		3
	{"_nswbuf"},
#define	atext		4
	{"_text"},
#define	abuf		5
	{"_buf"},
#define	abfreeli	6
	{"_bfreelist"},
#define	akl11		7
	{"_kl11"},
#define	adh11		8
	{"_dh11"},
#define	alpdt		9
	{"_lp_softc"},
#define	albolt		10
	{"_lbolt"},
#define	atout		11
	{"_tout"},
#define	arunin		12
	{"_runin"},
#define	arunout		13
	{"_runout"},
#define	aipc		14
	{"_ipc"},
#define	afile		15
	{"_file"},
#define	ainode		16
	{"_inode"},
#define	amaplock	17
	{"_maplock"},
#define	acoremap	18
	{"_coremap"},
#define	aswapmap	19
	{"_swapmap"},
#define	au		20
	{"_u"},
#define adz11		21
	{"_dz_tty"},
#define aetext		22
	{"_etext"},
#define ausrptmap	23
	{"_Usrptmap"},
#define	ausrpt		24
	{"_usrpt"},
#define	achtbuf		25
	{"_chtbuf"},
#define	arhtbuf		26
	{"_rhtbuf"},
#define	ahpbuf		27
	{"_hpbuf"},
#define aswbuf		28
	{"_swbuf"},
#define arswbuf		29
	{"_rswbuf"},
#define acons		30
	{"_cons"},
#define ark7		31
	{"_rrk7buf"},
#define achrfclist	32
	{"_Chrfclist"},
#define anproc		33
	{"_nproc"},
#define antext		34
	{"_ntext"},
#define anbuf		35
	{"_nbuf"},
#define ahz		36
	{"_hz"},
#define aninode		37
	{"_ninode"},
#define anfile		38
	{"_nfile"},
#define answap		39
	{"_nswap"},
#define	acdevsw		40
	{"_cdevsw"},
#define	aChconntab	41
	{"_Chconntab"},
#define	MAXSYMBOLS	42
	{"", 0, 0},
};

/* this structure is read from info file or initialized (iflag) */

struct	{
	caddr_t	kaddr[MAXSYMBOLS];		/* useful kernel addresses */
	char	unames[MAXUSERS][UNAMELENGTH];	/* user names */
	struct	ttyline	{
		struct tty	*l_addr;		/* address of ttystruct */
		unsigned	l_pgrp;		/* process group */
		char		l_name[2];	/* name */
		dev_t		l_dev;		/* device number */
	} ttyline[MAXTTYS];	
	
} info;
int	swapdev;			/* major, minor of swap device */
int	swplo;				/* unix swap disk offset */
int	nswap;				/* unix swap space size */

struct ttyline notty = {0, 0, {"- "}};

/* flags for once only activities (once per repeat) */

int heading;
int coreinit, core;
char *topmem;
int arglength;
char *getcore(), *store(), *waitingfor(), *getcmd(), *strcat(), *brk();

main(argc,argv)
char *argv[];
{
	register char *cp, **ap;
	register int i;
	int myuid;
	extern char _sobuf[];

	if ((myuid = getuid()) == 0)
		nice(-100);

	setbuf(stdout, _sobuf);
	select = 0;
	for (ap = &argv[1]; --argc; ap++) {
		for (cp = *ap; *cp;) {
			switch (*cp++) {
			case '-':
				continue;
			case 'A':	/* EVERYTHING */
				Aflag++;
				continue;
			case 'a':	/* all procs attached to ttys */
				bflag++;	/* include background */
				fflag++;	/* include foreground */
				dflag++;	/* include detached */
				aflag++;	/* include shells */
				select++;
				continue;
			case 'b':	/* all background processes */
				bflag++;
				select++;
				continue;
			case 'B':	/* all busy processes */
				Bflag++;
				select++;
				lflag++;
				continue;
			case 'c':
				cflag++;
				lflag++;
				continue;
			case 'd':	/* detached processes */
				dflag++;
				select++;
				continue;
			case 'e':
				eflag++;
				continue;
			case 'f':	/* foreground only */
				fflag++;
				select++;
				continue;
			case 'F':	/* go fast, don't touch swap */
				Fflag++;
				continue;
			case 'G':	/* print pgrp */
				Gflag++;
				continue;
			case 'g':	/* specify process gourp */
				select++;
				while (argc > 1) {
					if (**++ap == '-') {
						ap--;
						break;
					}
					--argc;
					if (pgrps >= &grps[NSPEC])
						prexit("%a: too many groups\n");
					(pgrps++)->nm_int = atoi(*ap);
				}
				if (pgrps == grps)
					(pgrps++)->nm_int = getpgrp();
				continue;
			case 'i':	/* initialize info file */
				if (myuid != 0)	/* must be super user */
					goto def;
				iflag++;
				nflag++;
				Uflag++;
				continue;
			case 'l':	/* long output */
				lflag++;
				continue;
			case 'm':	/* use designated memory file */
				if (myuid != 0)	/* must be super user */
					goto def;
				if (argc-- < 2 || **++ap == '-')
					prexit("%a: missing memory file\n");
				memf = *ap;
				mflag++;
				continue;
			case 'n':
				select++;
				nflag++;
				continue;
			case 'p':	/* only designated processes */
				select++;
				while (argc > 1) {
					if (**++ap == '-') {
						ap--;
						break;
					}
					--argc;
					if (ppids >= &pids[NSPEC])
						prexit("%a: too many pids\n");
					(ppids++)->nm_int = atoi(*ap);
				}
				continue;
			case 'r':	/* repeat every <number> seconds */
				if (myuid != 0)
					goto def;
				rflag++;
				for (i = 0; *cp >= '0' && *cp <= '9'; cp++)
					i = i * 10 + *cp - '0';
				if (i)
					rflag = i;
				continue;
			case 'U':	/* use designated symbol file */
				if (myuid != 0)
					goto def;
				if (argc-- < 2 || **++ap == '-')
					prexit("%a: missing symbol file\n");
				symf = *ap;
				Uflag++;
				continue;
			case 's':
				sflag++;
				select++;
				continue;
			case 'S':
				Sflag++;
				continue;
			case 'T':
				Tflag++;
				continue;
			case 't':	/* on designated tty(s) */
				select++;
				while (argc > 1) {
					if (**++ap == '-') {
						ap--;
						break;
					}
					--argc;
					if (pttys >= &ttys[NSPEC])
						prexit("%a: too many ttys\n");
					(pttys++)->ty_ptr = *ap;
				}
				if (pttys == ttys) {
					char *ttyname();

					if ( (pttys->ty_ptr = ttyname(2)) == 0)
						prexit("%a: unknown tty\n");
					else if (strcmp("/dev/console", pttys->ty_ptr) == 0)
						(pttys++)->ty_ptr = "co";
					else
						(pttys++)->ty_ptr +=
							sizeof("/dev/tty") - 1;
				}
				continue;
			case 'u':		/* specific user name */
				aflag++;
				select++;
				puids = &uids[0];
				while (argc > 1) {
					if (**++ap == '-') {
						ap--;
						break;
					}
					--argc;
					if (puids >= &uids[NSPEC])
						prexit("%a: too many users\n");
					(puids++)->nm_ptr = *ap;
				}
				if (puids == &uids[0])
					(puids++)->nm_int = myuid;
				continue;
			case 'v':	/* most verbose output */
				vflag++;
				lflag++;
				continue;
			case 'W':
				Wflag++;
				continue;
			case 'w':	/* wide form (all arguments) */
				wflag++;
				continue;
			case 'x':	/* include un-owned procs */
				xflag++;
				select++;
				continue;
			case 'z':	/* include only zombies */
				zflag++;
				select++;
				continue;
			def:
			default:
				prexit("%a: unknown switch: %c\n", *--cp);
			}
			break;
		}
	}

/* these lengths are kludgely tuned to make things not exceed 79 columns */
	arglength = 60;
	if (lflag)
		arglength -= 28;
	if (vflag)
		arglength -= 14;
	if ((mem = open(memf, 0)) < 0)
		prexit("%a: cannot read system memory: %s\n", memf);
	if ((kmem = open(kmemf, 0)) < 0)
		prexit("%a: cannot read system virtural memory: %s\n", kmemf);
	if (!Fflag && (swap = open(swapf, 0)) <0)
		prexit("%a: cannot read swap device: %s\n", swapf);

	if (!iflag)
		if ((i = open(infof, 0)) < 0)
			prexit("%a: cannot open info file\n");
		else if (read(i, &info, sizeof info) != sizeof info)
			prexit("%a: cannot read info file\n");
		else
			close(i);
	if (Uflag) {
		struct nlist *np;
		if ((i = open(symf, 0)) < 0)
			prexit("%a: can't read symbol file\n");
		close(i);
		nlist(symf, namelist);
		for (np = namelist; np < &namelist[MAXSYMBOLS]; np++)
			if (np->n_value == 0)
				fprintf(stderr, "%a: can't find symbol: %s\n",
					np->n_name);
		if (namelist[0].n_value == -1)
			prexit("%a: cannot read symbol file: %s\n", symf);
		for (i = 0; i < MAXSYMBOLS; i++)
			info.kaddr[i] = (caddr_t)namelist[i].n_value;
		info.kaddr[aetext] = (caddr_t)( ((unsigned)info.kaddr[aetext] + 63) & ~63);
	}
	if (iflag) {
		readusers();
		ttyinit();
		if ((infofd = creat(infof, 0600)) < 0)
			prexit("%a: cannot create info file\n");
		if ((i = write(infofd, &info, sizeof info)) != sizeof info) {
			if (i == -1)
				perror(0);
			prexit("%a: cannot write info file: %d\n", i);
		}
		close(infofd);
	}
	lseek(kmem, (long)info.kaddr[aswapdev], 0);
	read(kmem, &swapdev, sizeof(swapdev) );
	lseek(kmem, (long)info.kaddr[aswplo], 0);
	read(kmem, &swplo, sizeof(swplo) );
	lseek(kmem, (long)info.kaddr[answap], 0);
	read(kmem, &nswap, sizeof(nswap) );
	lseek(kmem, (long)info.kaddr[anproc], 0);
	read(kmem, &nproc, sizeof(nproc) );
	lseek(kmem, (long)info.kaddr[antext], 0);
	read(kmem, &ntext, sizeof(ntext) );
	lseek(kmem, (long)info.kaddr[anbuf], 0);
	read(kmem, &nbuf, sizeof(nbuf) );
	lseek(kmem, (long)info.kaddr[abuf], 0);
	read(kmem, &buf, sizeof(buf) );
	lseek(kmem, (long)info.kaddr[answbuf], 0);
	read(kmem, &nswbuf, sizeof(nswbuf) );
	lseek(kmem, (long)info.kaddr[aswbuf], 0);
	read(kmem, &swbuf, sizeof(swbuf) );
	lseek(kmem, (long)info.kaddr[aninode], 0);
	read(kmem, &ninode, sizeof(ninode) );
	lseek(kmem, (long)info.kaddr[ainode], 0);
	read(kmem, &inode, sizeof(inode) );
	lseek(kmem, (long)info.kaddr[ahz], 0);
	read(kmem, &hz, sizeof(hz) );
	lseek(kmem, (long)info.kaddr[aproc], 0);
	read(kmem, &kproc, sizeof(kproc));
	lseek(kmem, (long)info.kaddr[atext], 0);
	read(kmem, &ktext, sizeof(ktext));
	proc = (struct proc *)getcore(nproc * sizeof(struct proc));
	text = (struct text *)getcore(ntext * sizeof(struct text));
	if (puids != &uids[0] && uids[0].nm_int != myuid)
		usersetup();
	if (!select) {
		mypid = getpid();
		(puids++)->nm_int = myuid;
		nxflag++;
		select++;
	}
	ttysetup();
	topmem = 0;
	do {
		heading = 0;			/* reset heading flag (for repeat) */
		core = coreinit = 0;		/* reset core flag (for repeat) */
		lseek(kmem, (long)kproc, 0);
		read(kmem, proc, nproc * sizeof(struct proc));
		lseek(kmem, (long)ktext, 0);
		read(kmem, text, ntext * sizeof(struct text));
		needed();
		mktree();
		action (plist, 0);
		printf("%d processes (%dkb), %d busy (%dkb), %d loaded (%dkb)\n",
			ntotal, (ctob(ktotal) + 1023) / 1024,
			nbusy, (ctob(kbusy) + 1023) / 1024,
			nloaded, (ctob(kloaded) + 1023) / 1024);
		fflush(stdout);
	} while (rflag && sleep(rflag) == 0);
	exit(0);
}

/* read the passwd file and fill in the user name arrays */
readusers()
{
	register struct passwd *pw;
	struct passwd *getpwent();

	while((pw = getpwent()) != 0) {
		if(info.unames[pw->pw_uid][0] == '\0')
			strcpyn(info.unames[pw->pw_uid], pw->pw_name, UNAMELENGTH);
	}
	endpwent();
}

/* check for specified user names */

usersetup()
{
	register int i;
	register union numptr *ip;

	for (ip = uids; ip < puids; ip++) {
		for (i = 0; i < MAXUSERS; i++)
			if (equalu(ip->nm_ptr, info.unames[i]))
				goto cont2;
		prexit("%a: unknown user: %s\n", ip->nm_ptr);
	cont2:
		ip->nm_int = i;
	}
}

/* compare a fixed length user name */

equalu(u1, u2)
register char *u1, *u2;
{
	register int i = 0;

	while (*u1++ == *u2)
		if (!*u2++ || ++i == UNAMELENGTH)
			return 1;
	return 0;
}

/*
 * Initialize the tty part of the info structure
 */
ttyinit()
{
	struct direct dir;
	struct stat sbuf;
	int fd;
	register struct ttyline *lp = info.ttyline;

	if ((fd = open("/dev", 0)) < 0)
		prexit("%a: can't open /dev\n");
	chdir("/dev");
	while (read(fd, (char *)&dir, sizeof(dir)) == sizeof(dir)) {
		if (dir.d_ino == 0 ||
		    strncmp("tty", dir.d_name, 3) != 0 &&
		    strcmp("console", dir.d_name) != 0)
			continue;
		if (dir.d_name[sizeof("tty") - 1] == 'C')
			continue;
		if (lp >= &info.ttyline[MAXTTYS])
			prexit("%a: too many ttys in /dev\n");
		if (dir.d_name[0] == 'c') {
			lp->l_name[0] = 'c';
			lp->l_name[1] = 'o';
		} else {
			lp->l_name[0] = dir.d_name[3];
			lp->l_name[1] = dir.d_name[4];
		}
		stat(dir.d_name, &sbuf);
		lp->l_dev = sbuf.st_rdev;
		lseek(kmem, (long)&cswitch[major(sbuf.st_rdev)].d_ttys, 0);
		read(kmem, (char *)&lp->l_addr, sizeof(lp->l_addr));
		lp->l_addr += minor(sbuf.st_rdev);
		lp++;
	}
	close(fd);
}
ttysetup()
{
	register struct ttyline *lp;
	register char *cp;
	union ttyptr *tp;
	struct tty tty;

	for (lp = info.ttyline; lp->l_name[0]; lp++) {
		lseek(kmem, (long)lp->l_addr, 0);
		if (read(kmem, &tty, sizeof tty) != sizeof tty)
			prexit("%a: read error in kmem\n");
		lp->l_pgrp = tty.t_pgrp;
		if (Tflag)
			printf("tty%-.2s: dev:%2d,%2d addr:%6x, rawq:%4d, canq:%d, outq:%4d, pgrp:%5d\n",
				lp->l_name, major(lp->l_dev), minor(lp->l_dev),
				ABS(lp->l_addr), tty.t_rawq.c_cc,
				tty.t_canq.c_cc, tty.t_outq.c_cc,
				tty.t_pgrp);
	}
#ifdef CHAOS
	mkchttys(lp);
#endif
	/* now fix up specified ttys */

	for (tp = &ttys[0]; tp < pttys; tp++) {
		for (lp = info.ttyline; lp->l_name[0]; lp++)
			if (strcmpn(tp->ty_ptr, lp->l_name, 2) == 0) {
				tp->ty_line = lp;
				goto cont2;
			}
		prexit("%a: unknown tty name: %c\n", tp->ty_ptr);
	cont2:;
	}
}

/*
 * Determine which procs are needed for the printout
 * and add these to a list of needed processes (plist)
 */
needed()
{
	register struct proc *p, *pp;
	register struct text *tp;
	struct ttyline *lp;
	int ok;

	plist = 0;
	nswapped = ntotal = nbusy = nloaded = 0;
	kswapped = ktotal = kbusy = kloaded = 0;

	for (tp = text; tp < text + ntext; tp++)
		if (tp->x_count) {
			ktotal += tp->x_size;
			if (!(tp->x_ccount))
				kswapped += tp->x_size;
		}

	for (p = proc; p < proc + nproc; p++) {
		if (!p->p_stat)
			continue;
		if (p->p_textp)
			p->p_textp = &text[p->p_textp - ktext];
		if (p->p_pptr) {
			p->p_pptr = &proc[p->p_pptr - kproc];
			if (p->p_pptr < proc || p->p_pptr >= &proc[nproc]) {
				fprintf(stderr, "proc %d bad pptr\n", p->p_pid);
				p->p_pptr = proc;
			}
		} else
			p->p_pptr = proc;
	}
	for (p = &proc[0]; p < &proc[nproc]; p++) {
		if (!p->p_stat)
			continue;
		ntotal++;
		ktotal += procsize(p);
		if (p->p_flag != SZOMB)
			if (p->p_flag & SLOAD) {
				nloaded++;
				kloaded += procsize(p);
				if ((tp = p->p_textp) && tp->x_count) {
					tp->x_count = 0;
					kloaded += tp->x_size;
				}
			} else {
				nswapped++;
				kswapped += procsize(p);
			}
		ok = FALSE;
		if (p->p_stat == SRUN ||
		    p->p_stat == SSLEEP && (p->p_pri < PZERO && p->p_pid > MSPID)) {
			nbusy++;
			kbusy += procsize(p);
			if ((tp = p->p_textp) && tp->x_ccount) {
				tp->x_ccount = 0;
				kbusy += tp->x_size;
			}
			if (Bflag)
				ok = TRUE;
		}
		if (nflag)
			continue;
		if (zflag && p->p_stat == SZOMB)
			ok = TRUE;
		if (sflag && p->p_stat == SSTOP)
			ok = TRUE;
		if (select == 0 && mypid && p->p_pid == mypid)
			continue;
		if (p->p_pgrp == 0)
			if (xflag)
				ok = TRUE;
			else if (nxflag)
				continue;
		if (dflag && p->p_pgrp != 0 && (p->p_flag & SDETACH) != 0)
			ok = TRUE;
		if (aflag && xflag && p->p_pgrp != 0 && (p->p_flag & SDETACH) == 0 &&
		    p->p_pptr == &proc[1])
			ok = TRUE;
		if (puids != uids) {
			register union numptr *ip;

			for (pp = p; pp > &proc[1]; pp = pp->p_pptr)
 				for (ip = uids; ip < puids; ip++)
					if ((pp->p_uid & 0377) == ip->nm_int){
						ok = TRUE;
						goto uidok;
					}
		}
	uidok:
		if (pgrps != grps) {
			register union numptr *ip;

			for (pp = p; pp > &proc[1]; pp = pp->p_pptr)
 				for (ip = grps; ip < pgrps; ip++)
					if (pp->p_pgrp == ip->nm_int) {
						ok = TRUE;
						goto pgrpok;
					}
		}
	pgrpok:
		if (ppids != pids) {
			register union numptr *ip;

			for (ip = pids; ip < ppids; ip++)
				if (ip->nm_int == p->p_pid) {
					ok = TRUE;
					goto procok;
				}
		}
	procok:
		if (select && pttys == ttys && !fflag && !bflag && !ok)
			continue;
		if (getu(p) == 0) {
			static struct procinfo	fakep = {"--no upage--", &notty, 0};

			if (select && !ok)
				continue;
			pinfo(p) = &fakep;
			goto putonlist;
		}
		if (pttys != ttys && p->p_pgrp != 0) {
			union ttyptr *ip;

			for (ip = ttys; ip < pttys; ip++)
				if (p->p_pgrp && p->p_pgrp == ip->ty_line->l_pgrp ||
				    p->p_stat == SSLEEP &&
				    p->p_wchan >= (char *)ip->ty_line->l_addr &&
				    p->p_wchan < (char *)ip->ty_line->l_addr +
						 sizeof (struct tty) ||
				    u.u_ttyd == ip->ty_line->l_dev) {
					ok = TRUE;
					break;
				}
		}
		if (p->p_pgrp == 0)
			lp = &notty;
		else {
			for (lp = info.ttyline; lp->l_name[0] != 0; lp++)
				if (lp->l_dev == u.u_ttyd)
					break;
			if (lp->l_name[0] == 0)
				lp = &notty;
			else if (p->p_pptr != &proc[1]) {
				if (fflag && p->p_pgrp == lp->l_pgrp)
					ok = TRUE;
				if (bflag && p->p_pgrp != lp->l_pgrp &&
				    (p->p_flag & SDETACH) == 0 &&
				    p->p_stat != SSTOP)
					ok = TRUE;
			}
		}
		if (select && !ok)
			continue;
		pinfo(p) = (struct procinfo *)getcore(sizeof (struct procinfo));
		pinfo(p)->pi_time = u.u_vm.vm_utime + u.u_vm.vm_stime;
		pinfo(p)->pi_tty = lp;
		pinfo(p)->pi_cmd = getcmd(p);
	putonlist:
	/* we have a needed proc! */

		p->p_next = plist;
		plist = p;
		p->p_son = p->p_bro = 0;
	}
}
/*
 * mktree - sort the needed processes by subtree and at the top by user
 */
mktree()
{
	register struct proc *p, *pp, *lp;
	struct proc *op;
	struct proc proot;

	proot.p_bro = 0;

	for (p = plist; p; p = p->p_next) {	/* for all needed processes */
		if (p->p_pptr > &proc[1]) {
			for (pp = plist; pp; pp = pp->p_next)
				if (pp == p->p_pptr) {	/* if my parent */
					if (lp = pp->p_son) {	/* if siblings */
						for (op = 0; lp && lp->p_pid <
							p->p_pid;
							lp = (op = lp)->p_bro)
							;
						if (op) {
							p->p_bro = lp;
							op->p_bro = p;
							break;
						}
					}
					p->p_bro = lp;	/* here if first or only */
					pp->p_son = p;
					break;
				}
			if (pp)		/* if we found the parent */
				continue;
		}

		/* we have a top level process, sort into top level list */

		for (pp = (lp = &proot)->p_bro; pp; pp = (lp = pp)->p_bro)
			if ((p->p_uid & 0377) < (pp->p_uid & 0377) ||
				(p->p_uid & 0377) == (pp->p_uid & 0377) &&
				p->p_pid < pp->p_pid)
				break;
		p->p_bro = lp->p_bro;
		lp->p_bro = p;
	}
	plist = proot.p_bro;
}

action(p, md)
register struct proc *p;
register int md;
{

	if (p) {
		printp(p, md);
		if (p->p_son)
			action(p->p_son, md+1);
		if (p->p_bro)
			action(p->p_bro, md);
	}
}

/*
 * Pretty print the output according to the switches.
 */
printp(p, md)
register struct proc *p;
{
	register char *cp, *cp1;
	char	stat[10];
	static int lastuid;
	static char	*statnames[] = {"Unk ", "Wait", "Wait", "Run ",
				"Init", "Exit", "Stop"};

	if (!heading) {
		heading++;
		printf("Ty User     ");
		if (lflag) {
			printf("Stat");
			if (vflag) printf(" Flgs Nice Pri ");
			else printf(" ");
			printf("Memory-kb  Time Wait?  ");
			}
		if (Aflag)
			printf("Address Proc.  Clock Alarm ");
		if (Sflag)
			printf("Size  ");
		if (Gflag)
			printf("Group ");
		printf("Proc#  Command\n");
	}
	printf("%.2s%c", pinfo(p)->pi_tty->l_name,
		p->p_pgrp == 0 ? ' ' :
		p->p_flag & SDETACH ? '_' :
		p->p_pgrp == pinfo(p)->pi_tty->l_pgrp ? '.' :
		' ');

	if (md == 0) {
		lastuid = p->p_uid & 0377;
		cp = info.unames[lastuid];
		if (*cp)
			printf("%-8.8s ", cp);
		else
			printf("user%-4.4d ", lastuid);
	} else {
		if (md > 8)
			md = 8;
		printf("%*s*", md, "");
		if ((p->p_uid & 0377) != lastuid) {	/* setuid process! */
			lastuid = p->p_uid & 0377;
			cp = info.unames[lastuid];
		} else
			cp = "";
		md = 8 - md;
		printf("%-*.*s", md, md, cp);
	}
	if (lflag) {
		cp = statnames[p->p_stat];
		if (p->p_flag&SLOAD) {
			for (cp1 = stat; *cp1 = *cp; cp1++, cp++)
				if (*cp >= 'a' && *cp <= 'z')
					*cp1 -= 'a' - 'A';
			cp = stat;
			}
		printf("%-4.4s ", cp);
		if (vflag) {
			cp = stat;
			if (p->p_flag & SSYS) *cp++ = 'U';
			if (p->p_flag&SLOCK) *cp++ = 'L';
			if (p->p_flag&STRC) *cp++ = 'T';
			if (p->p_flag&SWTED) *cp++ = 'W';
			if (p->p_flag&SSWAP) *cp++ = 'S';
			while(cp < &stat[5]) *cp++ = ' ';
			*cp = 0;
			printf("%-4.4s ",stat);
			if (p->p_nice != NZERO)
				printf("%4d", p->p_nice - NZERO);
			else
				printf("    ");
			if (p->p_stat != SZOMB)
				printf("%4d ", p->p_pri);
			else
				printf("     ");
		}
		if (p->p_stat != SZOMB) {
			printf("%4d", msize(procsize(p)) );
			if (p->p_textp)
				printf("+%4d ", msize(p->p_textp->x_size));
			else
				printf("      ");
			prcpu(pinfo(p)->pi_time);
		} else
			printf("                ");
		if (p->p_stat != SZOMB && p->p_stat != SRUN && p->p_stat != SSTOP)
			if (!Wflag && (cp = waitingfor(p)))
				printf("%-6.6s ", cp);
			else printf("%6x ", ABS((int)p->p_wchan));
		else printf("       ");
	}
	if (Aflag)
		printf("%6x %6x %6d%6d ", p->p_addr,
			(p - proc) * sizeof (struct proc) + info.kaddr[aproc],
			p->p_time, p->p_clktim);
	if (Sflag)
		printf("%5x ", procsize(p) );
	if (Gflag)
		printf("%5D ", p->p_pgrp);
	printf("%5D  ", p->p_pid);
	if (wflag)
		printf("%s\n", pinfo(p)->pi_cmd);
	else
		printf("%-.*s\n", arglength, pinfo(p)->pi_cmd);
}

/* print cpu time */

prcpu(time)
long time;
{
	register unsigned i;

	if (time < 0)
		printf(" ---- ");
	else if (time < (long)hz * 60 * 10)		/* less than 10 minutes */
		printf("%3d.%1d ",
			(int)(time / hz),
			(int)(time % hz / (hz / 10)));
	else if (time < (long)hz * 60 * 60 * 10)/* less than 10 hours */
		printf("%3d M ",
			(int)((time + (hz * 60) / 2) / (hz * 60)));
	else {
		i = (time + ((long)hz * 60 * 60) / 2) /
					((long)hz * 60 * 60);
		if (i < 1000)
			printf("%3d H ", i);
		else
			printf(" ---- ");
	}
}
/* Determine what a process is waiting for and describe it. */

char *
waitingfor(p)
register struct proc *p;
{
	register caddr_t w;
	register struct ttyline *lp;
	register char *cp;

	w = p->p_wchan;
	if (w == (caddr_t)0)
		return "null";
	if (w >= (char *)kproc && w < (char *)(kproc + nproc))
		return "child";
	if (w >= (char *)swbuf && w < (char *)(swbuf + nswbuf))
		return "swap";
	if (w == info.kaddr[arswbuf])
		return "rswap";
	if (w >= (char *)buf && w < (char *)(buf + nbuf))
		return "diskio";
	if (w >= info.kaddr[afile] && w < info.kaddr[afile] + sizeof(struct file) * nfile)
		return "file";
	if (w >= (char *)inode && w < (char *)(inode + ninode))
		switch((w - (char *)inode) % sizeof(struct inode)) {
		case 1:
			return "wpipe";
		case 2:
			return "rpipe";
		case 3:
			return "mutex";
		case (int)&((struct inode *)0)->i_un.i_group.g_datq:
			return "rmux";
		default:
			return "inode";
		}
	if (w == info.kaddr[achtbuf])
		return "tapecn";
	if (w == info.kaddr[ahpbuf])
		return "rpdisk";
	if (w == info.kaddr[ark7])
		return "rkdisk";
	if (w == info.kaddr[arhtbuf])
		return "tapeio";
	if (w == info.kaddr[alpdt])
		return "printr";
	if (w == info.kaddr[albolt])
		return "lbolt";
	if (w == info.kaddr[arunin])
		return "runin";
	if (w == info.kaddr[arunout])
		return "runout";
	if (w == info.kaddr[atout])
		return "sleep";
	if (w == info.kaddr[aipc])
		return "ptrace";
	if (w == info.kaddr[abfreeli])
		return "buffer";
	if (w == info.kaddr[amaplock])
		return "ubmap";
	if (w == info.kaddr[au])
		return "pause";
	if (w == info.kaddr[achrfclist])
		return "chrfc";
	for (lp = info.ttyline; lp->l_name[0]; lp++)
		if (w >= (char *)lp->l_addr && w < (char *)lp->l_addr + sizeof (struct tty)) {
#define TTY0 ((struct tty *)0)
			switch(w - (char *)lp->l_addr) {
			case (int)&TTY0->t_rawq:
				cp = "rtty??";
				break;
			case (int)&TTY0->t_outq:
				cp = "wtty??";
				break;
			case (int)&TTY0->t_state:
				cp = "otty??";
				break;
			default:
				cp = "?tty??";
			}
			cp[4] = lp->l_name[0];
			cp[5] = lp->l_name[1];
			return cp;
		}
	return 0;
}

getu(mproc)
register struct proc *mproc;
{
	struct pte *pteaddr, apte;
	register int i;
	int ncl, size;

	size = Sflag ? ctob(UPAGES) : sizeof (struct user);
	if ((mproc->p_flag & SLOAD) == 0) {
		lseek(swap, (long)ctob(mproc->p_swaddr), 0);
		if (read(swap, (char *)&user.user, size) != size) {
			fprintf(stderr, "%a: cant read u for pid %d from %s\n",
			    mproc->p_pid, swapf);
			return (0);
		}
		return (1);
	}
	pteaddr = &Usrptmap[btokmx(mproc->p_p0br) + mproc->p_szpt - 1];
	lseek(kmem, (long)(mflag ? ABS(pteaddr) : (int)pteaddr), 0);
	if (read(kmem, (char *)&apte, sizeof(apte)) != sizeof(apte)) {
		printf("%a: cant read indir pte to get u for pid %d from %s\n",
		    mproc->p_pid, swapf);
		return (0);
	}
	lseek(mem, (long)
	    (ctob(apte.pg_pfnum+1) - (UPAGES+MAXARGPG) * sizeof (struct pte)),
	    0);
	if (read(mem, (char *)pagetable, sizeof(pagetable)) != sizeof(pagetable)) {
		printf("%a: cant read page table for u of pid %d from %s\n",
		    mproc->p_pid, swapf);
		return (0);
	}
	ncl = (size + NBPG*CLSIZE - 1) / (NBPG*CLSIZE);
	while (--ncl >= 0) {
		i = ncl * CLSIZE;
		lseek(mem, (long)ctob(pagetable[MAXARGPG+i].pg_pfnum), 0);
		if (read(mem, user.upages[i], CLSIZE*NBPG) != CLSIZE*NBPG) {
			printf("%a: cant read page %d of u of pid %d from %s\n",
			    pagetable[MAXARGPG+i].pg_pfnum, mproc->p_pid, memf);
			return(0);
		}
	}
	return (1);
}
char *
getcmd(p)
register struct proc *p;
{
	struct pte apte;
	char argbuf[MAXARGPG * NBPG], *argptr;
	register int *ip;
	register char *cp, *cp1;
	int cc, nbad, i;

	if (p->p_stat == SZOMB)
		return "--Defunct--";
	if ((p->p_flag&SLOAD) == 0 && Fflag)
		return "--Swapped--";
	if (p->p_flag & SSYS)
		return p->p_pid == 0 ? "UNIX Swapper" :
			p->p_pid == 2 ? "UNIX Pager" : "UNIX";
	for (i = 0; i < MAXARGPG; i++) {
		argptr = &argbuf[(MAXARGPG - 1 - i) * NBPG];
		apte = pagetable[MAXARGPG - 1 - i];
		if ((p->p_flag & SLOAD) && apte.pg_fod == 0 && apte.pg_pfnum ) {
			lseek(mem, (long)ctob(apte.pg_pfnum), 0);
			if (read(mem, argptr, NBPG) != NBPG)
				return "---Mem read error (args)---";
		} else if (Fflag)
			goto cheap;
		else {
			lseek(swap, (long)ctob(u.u_smap.dm_map[0] + DMMIN - 1 - i), 0);
			if (read(swap, argptr, NBPG) != NBPG)
				return "---Swap read error (args)---";
		}
		/* Here block of stack is at argptr */
		ip = (int *)&argptr[NBPG];
		if (i == 0) {
			*--ip = 0;
			ip--;
		}
		while (ip > (int *)argptr && *--ip != 0)
			;
		if (ip > (int *)argptr || *ip == 0)
			break;
	}
	if (i >= MAXARGPG) {
cheap:
		argbuf[0] = '(';
		strncpy(&argbuf[1], u.u_comm, sizeof(u.u_comm));
		strcat(argbuf, ")");
		return store(argbuf);
	}
	cp = (char *)(ip + 1);
	if (*cp == '\0')
		cp++;
	nbad = 0;
	for (cp1 = cp; cp1 < &argbuf[MAXARGPG*NBPG]; cp1++) {
		cc = *cp1 & 0177;
		if (cc == 0)
			*cp1 = ' ';
		else if (cc < ' ' || cc == 0177) {
			if (++nbad >= 5) {
				*cp1++ = ' ';
				break;
			}
			*cp1 = '?';
		} else if (!eflag && cc == '=') {
			*cp1 = 0;
			while (cp1 > cp && *--cp1 != ' ')
				*cp1 = 0;
			break;
		}
	}
	while (*--cp1 == ' ')
		*cp1 = 0;
	if (!wflag && &cp[arglength] < (char *)&argbuf[MAXARGPG*NBPG - 1])
		cp[arglength] = 0;
	return store(cp);
}

/*
 * Store a string in core for later use.
 */
char *
store(cp)
char *cp;
{
	register char *src, *dst, *svdst;

	src = cp;
	while (*src++);
	svdst = getcore(src - cp);
	dst = svdst;
	src = cp;
	while (*dst++ = *src++);
	return(svdst);
}

/*
 * Allocate and return a pointer to the asked for amount of core
 */
char *
getcore(cnt)
register int cnt;
{
	static char *corep;
	register char *ip;
	register int incr;
	char *sbrk();

	if (cnt > core) {
		if (coreinit == 0) {
			coreinit++;
			if (topmem)
				brk(topmem);	/* after repeat!! */
			else
				topmem = sbrk(0);
			corep = topmem;
		}
		incr = cnt > 4096 ? cnt : 4096;
		if (sbrk(incr) == 0)
			prexit("%a: out of memory!\n");
		core += incr;
	}
	ip = corep;
	core -= cnt;
	corep += cnt;
	return(ip);
}
#ifdef CHAOS
#include "chunix/chsys.h"
#include <chaos/chaos.h>

mkchttys(lp)
register struct ttyline *lp;
{
	register struct connection **cnp;
	register int i;
	struct tty tty;
	struct connection *Chconntab[CHNCONNS];
	struct connection conn;

	lseek(kmem, (long)info.kaddr[aChconntab], 0);
	read(kmem, (char *)Chconntab, sizeof(Chconntab));
	for (i = 0, cnp = Chconntab; cnp < &Chconntab[CHNCONNS]; i++, cnp++) {
		if (!*cnp)
			continue;
		lseek(kmem, (long)*cnp, 0);
		read(kmem, (char *)&conn, sizeof(conn));
		if ((conn.cn_flags & CHTTY) == 0)
			continue;
		lseek(kmem, (long)conn.cn_ttyp, 0);
		read(kmem, (char *)&tty, sizeof(tty));
		if (lp >= &info.ttyline[MAXTTYS])
			prexit("%a: too many ttys\n");
		lp->l_addr = conn.cn_ttyp;
		lp->l_pgrp = tty.t_pgrp;
		lp->l_dev = tty.t_dev;
		lp->l_name[0] = 'C';
		lp->l_name[1] = i < 10 ? '0' + i :
			i - 10 <= 'z' - 'a' ? i - 10 + 'a' :
			i - 10 - ('z' - 'a') + 'A';
		if (Tflag)
			printf("tty%-.2s: dev:%2d,%2d addr:%6x, rawq:%4d, canq:%d, outq:%4d, pgrp:%5d\n",
				lp->l_name, major(lp->l_dev), minor(lp->l_dev),
				ABS(lp->l_addr), tty.t_rawq.c_cc,
				tty.t_canq.c_cc, tty.t_outq.c_cc,
				tty.t_pgrp);
		lp++;
	}
}
#endif
