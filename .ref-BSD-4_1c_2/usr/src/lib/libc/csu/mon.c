static	char *sccsid = "@(#)mon.c	4.6 (Berkeley) 3/1/83";

#define ARCDENSITY	5	/* density of routines */
#define MINARCS		50	/* minimum number of counters */
#define	HISTFRACTION	2	/* fraction of text space for histograms */


struct phdr {
	int *lpc;
	int *hpc;
	int ncnt;
};

struct cnt {
	int *pc;
	long ncall;
} *countbase;

int numctrs;

static char		*minsbrk = 0;

#define	MSG "No space for monitor buffer(s)\n"

monstartup(lowpc, highpc)
	char *lowpc;
	char *highpc;
{
	int monsize;
	char *buffer;
	int cntsiz;
	char *sbrk();

	cntsiz = (highpc - lowpc) * ARCDENSITY / 100;
	if (cntsiz < MINARCS)
		cntsiz = MINARCS;
	monsize = (highpc - lowpc + HISTFRACTION - 1) / HISTFRACTION
		+ sizeof(struct phdr) + cntsiz * sizeof(struct cnt);
	monsize = (monsize + 1) & ~1;
	buffer = sbrk(monsize);
	if (buffer == (char *)-1) {
		write(2, MSG, sizeof(MSG));
		return;
	}
	minsbrk = sbrk(0);
	monitor(lowpc, highpc, buffer, monsize, cntsiz);
}

/*
 *	This routine is massaged so that it may be jsb'ed to
 */
asm(".text");
asm("#the beginning of mcount()");
asm(".data");
mcount()
{
	register int *selfpc;	/* r11 */
	register long **cntp;   /* r10 */
	static int profiling = 0;
	static int cntrs = 0;

#ifdef lint
	selfpc = (int *) 0;
#else not lint
	/*
	 * find the return address for mcount,
	 * and address of counter pointer
	 */
	asm("	movl	(sp),r11");	/* selfpc = ... (jsb frame) */
	asm("	movl	r0,r10");	/* address of count local */
#endif not lint
	/*
	 * check that counter is allocated
	 */
	if (*cntp == 0) {
		/*
		 * check that we are profiling
		 */
		if (countbase == 0)
			goto out;
		/*
		 * and that we aren't recursively invoked.
		 */
		if (profiling)
			goto out;
		/*
		 * check that a counter is available
		 */
		if (cntrs++ == numctrs)
			goto overflow;
		profiling = 1;
		countbase->pc = selfpc;
		*cntp = &countbase->ncall;
		countbase++;
		profiling = 0;
	}
	(**cntp)++;
out:
	asm( "	rsb" );

overflow:
#   define	TOLIMIT	"mcount: counter overflow\n"
	write( 2 , TOLIMIT , sizeof( TOLIMIT ) );
	goto out;
}
asm(".text");
asm("#the end of mcount()");
asm(".data");

monitor(lowpc, highpc, buf, bufsiz, cntsiz)
	char *lowpc, *highpc;
	char *buf;
	int bufsiz, cntsiz;
{
	register int o;
	struct phdr *php;
	static int ssiz;
	static char *sbuf;

	if (lowpc == 0) {
		profil(0, 0, 0, 0);
		o = creat("mon.out", 0666);
		write(o, sbuf, ssiz);
		close(o);
		return;
	}
	sbuf = buf;
	ssiz = bufsiz;
	php = (struct phdr *)&buf[0];
	php->lpc = (int *)lowpc;
	php->hpc = (int *)highpc;
	php->ncnt = cntsiz;
	numctrs = cntsiz;
	countbase = (struct cnt *)(buf + sizeof(struct phdr));
	o = sizeof(struct phdr) + cntsiz * sizeof(struct cnt);
	buf += o;
	bufsiz -= o;
	if (bufsiz <= 0)
		return;
	o = (highpc - lowpc);
	if(bufsiz < o)
		o = ((float) bufsiz / o) * 65536;
	else
		o = 65536;
	profil(buf, bufsiz, lowpc, o);
}

/*
 * This is a stub for the "brk" system call, which we want to
 * catch so that it will not deallocate our data space.
 * (of which the program is not aware)
 */
extern char *curbrk;

brk(addr)
	char *addr;
{

	if (addr < minsbrk)
		addr = minsbrk;
	asm("	chmk	$17");
	asm("	jcc	1f");
	asm("	jmp	cerror");
asm("1:");
	curbrk = addr;
	return (0);
}
