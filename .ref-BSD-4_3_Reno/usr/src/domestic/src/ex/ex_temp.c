/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)ex_temp.c	7.7 (Berkeley) 5/11/89";
#endif not lint

#include "ex.h"
#include "ex_temp.h"
#include "ex_vis.h"
#include "ex_tty.h"
#include "pathnames.h"

/*
 * Editor temporary file routines.
 * Very similar to those of ed, except uses 2 input buffers.
 */
#define	READ	0
#define	WRITE	1

#ifndef vms
#define	EPOSITION	7
#else
#define	EPOSITION	13
#endif

char	tfname[40];
char	rfname[40];
int	havetmp;
short	tfile = -1;
short	rfile = -1;

fileinit()
{
	register char *p;
	register int i, j;
	struct stat stbuf;

	if (tline == INCRMT * (HBLKS+2))
		return;
	cleanup(0);
	if (tfile >= 0)
		close(tfile);
	tline = INCRMT * (HBLKS+2);
	blocks[0] = HBLKS;
	blocks[1] = HBLKS+1;
	blocks[2] = -1;
	dirtcnt = 0;
	iblock = -1;
	iblock2 = -1;
	oblock = -1;
	CP(tfname, svalue(DIRECTORY));
#ifndef vms
	if (stat(tfname, &stbuf))
#else
	goto vms_no_check_dir;
#endif
	{
dumbness:
		if (setexit() == 0)
			filioerr(tfname);
		else
			putNFL();
		cleanup(1);
		ex_exit(1);
	}
#ifndef	vms
	if ((stbuf.st_mode & S_IFMT) != S_IFDIR) {
		errno = ENOTDIR;
		goto dumbness;
	}
#else
vms_no_check_dir:
#endif
	ichanged = 0;
	ichang2 = 0;
#ifndef	vms
	ignore(strcat(tfname, "/ExXXXXX"));
#else
	ignore(strcat(tfname, "ExXXXXX"));
#endif
	for (p = strend(tfname), i = 5, j = getpid(); i > 0; i--, j /= 10)
		*--p = j % 10 | '0';
#ifdef vms
	ignore(strcat(tfname, ".txt.1"));
	unlink(tfname);
#endif
	tfile = creat(tfname, 0600);
	if (tfile < 0)
		goto dumbness;
#ifdef VMUNIX
	{
		extern stilinc;		/* see below */
		stilinc = 0;
	}
#endif
	havetmp = 1;
	if (tfile >= 0)
		close(tfile);
	tfile = open(tfname, 2);
	if (tfile < 0)
		goto dumbness;
#ifdef UNIX_SBRK
/* 	brk((char *)fendcore); */
#endif
}

cleanup(all)
	bool all;
{
	if (all) {
		putpad(TE);
		flush();
	}
	if (havetmp) {
		if (tfile >= 0)
			close(tfile);
		unlink(tfname);
	}
	havetmp = 0;
	if (all && rfile >= 0) {
		if (rfile >= 0)
			close(rfile);
		unlink(rfname);
		rfile = -1;
	}
}

getline(tl)
	line tl;
{
	register char *bp, *lp;
	register int nl;

	lp = linebuf;
	bp = getblock(tl, READ);
	nl = nleft;
	tl &= ~OFFMSK;
	while (*lp++ = *bp++)
		if (--nl == 0) {
			bp = getblock(tl += INCRMT, READ);
			nl = nleft;
		}
}

putline()
{
	register char *bp, *lp;
	register int nl;
	line tl;

	dirtcnt++;
	lp = linebuf;
	change();
	tl = tline;
	bp = getblock(tl, WRITE);
	nl = nleft;
	tl &= ~OFFMSK;
	while (*bp = *lp++) {
		if (*bp++ == '\n') {
			*--bp = 0;
			linebp = lp;
			break;
		}
		if (--nl == 0) {
			bp = getblock(tl += INCRMT, WRITE);
			nl = nleft;
		}
	}
	tl = tline;
	tline += (((lp - linebuf) + BNDRY - 1) >> SHFT) & 077776;
	return (tl);
}

int	read();
int	write();

char *
getblock(atl, iof)
	line atl;
	int iof;
{
	register int bno, off;
#ifdef CRYPT
        register char *p1, *p2;
        register int n;
#endif
	
	bno = (atl >> OFFBTS) & BLKMSK;
	off = (atl << SHFT) & LBTMSK;
	if (bno >= NMBLKS)
		error(" Tmp file too large");
	nleft = BUFSIZ - off;
	if (bno == iblock) {
		ichanged |= iof;
		hitin2 = 0;
		return (ibuff + off);
	}
	if (bno == iblock2) {
		ichang2 |= iof;
		hitin2 = 1;
		return (ibuff2 + off);
	}
	if (bno == oblock)
		return (obuff + off);
	if (iof == READ) {
		if (hitin2 == 0) {
			if (ichang2) {
#ifdef CRYPT
				if(xtflag)
					crblock(tperm, ibuff2, CRSIZE, (long)0);
#endif
				blkio(iblock2, ibuff2, write);
			}
			ichang2 = 0;
			iblock2 = bno;
			blkio(bno, ibuff2, read);
#ifdef CRYPT
			if(xtflag)
				crblock(tperm, ibuff2, CRSIZE, (long)0);
#endif
			hitin2 = 1;
			return (ibuff2 + off);
		}
		hitin2 = 0;
		if (ichanged) {
#ifdef CRYPT
			if(xtflag)
				crblock(tperm, ibuff, CRSIZE, (long)0);
#endif
			blkio(iblock, ibuff, write);
		}
		ichanged = 0;
		iblock = bno;
		blkio(bno, ibuff, read);
#ifdef CRYPT
		if(xtflag)
			crblock(tperm, ibuff, CRSIZE, (long)0);
#endif
		return (ibuff + off);
	}
	if (oblock >= 0) {
#ifdef CRYPT
		if(xtflag) {
			/*
			 * Encrypt block before writing, so some devious
			 * person can't look at temp file while editing.
			 */
			p1 = obuff;
			p2 = crbuf;
			n = CRSIZE;
			while(n--)
				*p2++ = *p1++;
			crblock(tperm, crbuf, CRSIZE, (long)0);
			blkio(oblock, crbuf, write);
		} else
#endif
			blkio(oblock, obuff, write);
	}
	oblock = bno;
	return (obuff + off);
}

#ifdef	VMUNIX
#ifdef	vms
#define	INCORB	32
#else
#define	INCORB	64
#endif
char	incorb[INCORB+1][BUFSIZ];
#define	pagrnd(a)	((char *)(((int)a)&~(BUFSIZ-1)))
int	stilinc;	/* up to here not written yet */
#endif

blkio(b, buf, iofcn)
	short b;
	char *buf;
	int (*iofcn)();
{

#ifdef VMUNIX
	if (b < INCORB) {
		if (iofcn == read) {
			bcopy(pagrnd(incorb[b+1]), buf, BUFSIZ);
			return;
		}
		bcopy(buf, pagrnd(incorb[b+1]), BUFSIZ);
		if (laste) {
			if (b >= stilinc)
				stilinc = b + 1;
			return;
		}
	} else if (stilinc)
		tflush();
#endif
	lseek(tfile, (long) (unsigned) b * BUFSIZ, 0);
	if ((*iofcn)(tfile, buf, BUFSIZ) != BUFSIZ)
		filioerr(tfname);
}

#ifdef VMUNIX
tlaste()
{

	if (stilinc)
		dirtcnt = 0;
}

tflush()
{
	int i = stilinc;
	
	stilinc = 0;
	lseek(tfile, (long) 0, 0);
	if (write(tfile, pagrnd(incorb[1]), i * BUFSIZ) != (i * BUFSIZ))
		filioerr(tfname);
}
#endif

/*
 * Synchronize the state of the temporary file in case
 * a crash occurs.
 */
synctmp()
{
	register int cnt;
	register line *a;
	register short *bp;

#ifdef VMUNIX
	if (stilinc)
		return;
#endif
	if (dol == zero)
		return;
	if (ichanged)
		blkio(iblock, ibuff, write);
	ichanged = 0;
	if (ichang2)
		blkio(iblock2, ibuff2, write);
	ichang2 = 0;
	if (oblock != -1)
		blkio(oblock, obuff, write);
	time(&H.Time);
	uid = getuid();
	*zero = (line) H.Time;
	for (a = zero, bp = blocks; a <= dol; a += BUFSIZ / sizeof *a, bp++) {
		if (*bp < 0) {
			tline = (tline + OFFMSK) &~ OFFMSK;
			*bp = ((tline >> OFFBTS) & BLKMSK);
			if (*bp > NMBLKS)
				error(" Tmp file too large");
			tline += INCRMT;
			oblock = *bp + 1;
			bp[1] = -1;
		}
		lseek(tfile, (long) (unsigned) *bp * BUFSIZ, 0);
		cnt = ((dol - a) + 2) * sizeof (line);
		if (cnt > BUFSIZ)
			cnt = BUFSIZ;
		if (write(tfile, (char *) a, cnt) != cnt) {
oops:
			*zero = 0;
			filioerr(tfname);
		}
		*zero = 0;
	}
	flines = lineDOL();
	lseek(tfile, 0l, 0);
	if (write(tfile, (char *) &H, sizeof H) != sizeof H)
		goto oops;
#ifdef notdef
	/*
	 * This will insure that exrecover gets as much
	 * back after a crash as is absolutely possible,
	 * but can result in pregnant pauses between commands
	 * when the TSYNC call is made, so...
	 */
#ifndef vms
	(void) fsync(tfile);
#endif
#endif
}

TSYNC()
{

	if (dirtcnt > MAXDIRT) {	/* mjm: 12 --> MAXDIRT */
#ifdef VMUNIX
		if (stilinc)
			tflush();
#endif
		dirtcnt = 0;
		synctmp();
	}
}

/*
 * Named buffer routines.
 * These are implemented differently than the main buffer.
 * Each named buffer has a chain of blocks in the register file.
 * Each block contains roughly 508 chars of text,
 * and a previous and next block number.  We also have information
 * about which blocks came from deletes of multiple partial lines,
 * e.g. deleting a sentence or a LISP object.
 *
 * We maintain a free map for the temp file.  To free the blocks
 * in a register we must read the blocks to find how they are chained
 * together.
 *
 * BUG:		The default savind of deleted lines in numbered
 *		buffers may be rather inefficient; it hasn't been profiled.
 */
struct	strreg {
	short	rg_flags;
	short	rg_nleft;
	short	rg_first;
	short	rg_last;
} strregs[('z'-'a'+1) + ('9'-'0'+1)], *strp;

struct	rbuf {
	short	rb_prev;
	short	rb_next;
	char	rb_text[BUFSIZ - 2 * sizeof (short)];
} *rbuf, KILLrbuf, putrbuf, YANKrbuf, regrbuf;
#ifdef VMUNIX
short	rused[256];
#else
short	rused[32];
#endif
short	rnleft;
short	rblock;
short	rnext;
char	*rbufcp;

regio(b, iofcn)
	short b;
	int (*iofcn)();
{

	if (rfile == -1) {
		CP(rfname, tfname);
		*(strend(rfname) - EPOSITION) = 'R';
		rfile = creat(rfname, 0600);
		if (rfile < 0)
oops:
			filioerr(rfname);
		else
			close(rfile);
		rfile = open(rfname, 2);
		if (rfile < 0)
			goto oops;
	}
	lseek(rfile, (long) b * BUFSIZ, 0);
	if ((*iofcn)(rfile, rbuf, BUFSIZ) != BUFSIZ)
		goto oops;
	rblock = b;
}

REGblk()
{
	register int i, j, m;

	for (i = 0; i < sizeof rused / sizeof rused[0]; i++) {
		m = (rused[i] ^ 0177777) & 0177777;
		if (i == 0)
			m &= ~1;
		if (m != 0) {
			j = 0;
			while ((m & 1) == 0)
				j++, m >>= 1;
			rused[i] |= (1 << j);
#ifdef RDEBUG
			ex_printf("allocating block %d\n", i * 16 + j);
#endif
			return (i * 16 + j);
		}
	}
	error("Out of register space (ugh)");
	/*NOTREACHED*/
}

struct	strreg *
mapreg(c)
	register int c;
{

	if (isupper(c))
		c = tolower(c);
	return (isdigit(c) ? &strregs[('z'-'a'+1)+(c-'0')] : &strregs[c-'a']);
}

int	shread();

KILLreg(c)
	register int c;
{
	register struct strreg *sp;

	rbuf = &KILLrbuf;
	sp = mapreg(c);
	rblock = sp->rg_first;
	sp->rg_first = sp->rg_last = 0;
	sp->rg_flags = sp->rg_nleft = 0;
	while (rblock != 0) {
#ifdef RDEBUG
		ex_printf("freeing block %d\n", rblock);
#endif
		rused[rblock / 16] &= ~(1 << (rblock % 16));
		regio(rblock, shread);
		rblock = rbuf->rb_next;
	}
}

/*VARARGS*/
shread()
{
	struct front { short a; short b; };

	if (read(rfile, (char *) rbuf, sizeof (struct front)) == sizeof (struct front))
		return (sizeof (struct rbuf));
	return (0);
}

int	getREG();

putreg(c)
	char c;
{
	register line *odot = dot;
	register line *odol = dol;
	register int cnt;

	deletenone();
	appendnone();
	rbuf = &putrbuf;
	rnleft = 0;
	rblock = 0;
	rnext = mapreg(c)->rg_first;
	if (rnext == 0) {
		if (inopen) {
			splitw++;
			vclean();
			vgoto(WECHO, 0);
		}
		vreg = -1;
		error("Nothing in register %c", c);
	}
	if (inopen && partreg(c)) {
		if (!FIXUNDO) {
			splitw++; vclean(); vgoto(WECHO, 0); vreg = -1;
			error("Can't put partial line inside macro");
		}
		squish();
		addr1 = addr2 = dol;
	}
	cnt = append(getREG, addr2);
	if (inopen && partreg(c)) {
		unddol = dol;
		dol = odol;
		dot = odot;
		pragged(0);
	}
	killcnt(cnt);
	notecnt = cnt;
}

partreg(c)
	char c;
{

	return (mapreg(c)->rg_flags);
}

notpart(c)
	register int c;
{

	if (c)
		mapreg(c)->rg_flags = 0;
}

getREG()
{
	register char *lp = linebuf;
	register int c;

	for (;;) {
		if (rnleft == 0) {
			if (rnext == 0)
				return (EOF);
			regio(rnext, read);
			rnext = rbuf->rb_next;
			rbufcp = rbuf->rb_text;
			rnleft = sizeof rbuf->rb_text;
		}
		c = *rbufcp;
		if (c == 0)
			return (EOF);
		rbufcp++, --rnleft;
		if (c == '\n') {
			*lp++ = 0;
			return (0);
		}
		*lp++ = c;
	}
}

YANKreg(c)
	register int c;
{
	register line *addr;
	register struct strreg *sp;
	char savelb[LBSIZE];

	if (isdigit(c))
		kshift();
	if (islower(c))
		KILLreg(c);
	strp = sp = mapreg(c);
	sp->rg_flags = inopen && cursor && wcursor;
	rbuf = &YANKrbuf;
	if (sp->rg_last) {
		regio(sp->rg_last, read);
		rnleft = sp->rg_nleft;
		rbufcp = &rbuf->rb_text[sizeof rbuf->rb_text - rnleft];
	} else {
		rblock = 0;
		rnleft = 0;
	}
	CP(savelb,linebuf);
	for (addr = addr1; addr <= addr2; addr++) {
		getline(*addr);
		if (sp->rg_flags) {
			if (addr == addr2)
				*wcursor = 0;
			if (addr == addr1)
				strcpy(linebuf, cursor);
		}
		YANKline();
	}
	rbflush();
	killed();
	CP(linebuf,savelb);
}

kshift()
{
	register int i;

	KILLreg('9');
	for (i = '8'; i >= '0'; i--)
		copy(mapreg(i+1), mapreg(i), sizeof (struct strreg));
}

YANKline()
{
	register char *lp = linebuf;
	register struct rbuf *rp = rbuf;
	register int c;

	do {
		c = *lp++;
		if (c == 0)
			c = '\n';
		if (rnleft == 0) {
			rp->rb_next = REGblk();
			rbflush();
			rblock = rp->rb_next;
			rp->rb_next = 0;
			rp->rb_prev = rblock;
			rnleft = sizeof rp->rb_text;
			rbufcp = rp->rb_text;
		}
		*rbufcp++ = c;
		--rnleft;
	} while (c != '\n');
	if (rnleft)
		*rbufcp = 0;
}

rbflush()
{
	register struct strreg *sp = strp;

	if (rblock == 0)
		return;
	regio(rblock, write);
	if (sp->rg_first == 0)
		sp->rg_first = rblock;
	sp->rg_last = rblock;
	sp->rg_nleft = rnleft;
}

/* Register c to char buffer buf of size buflen */
regbuf(c, buf, buflen)
char c;
char *buf;
int buflen;
{
	register char *p, *lp;

	rbuf = &regrbuf;
	rnleft = 0;
	rblock = 0;
	rnext = mapreg(c)->rg_first;
	if (rnext==0) {
		*buf = 0;
		error("Nothing in register %c",c);
	}
	p = buf;
	while (getREG()==0) {
		for (lp=linebuf; *lp;) {
			if (p >= &buf[buflen])
				error("Register too long@to fit in memory");
			*p++ = *lp++;
		}
		*p++ = '\n';
	}
	if (partreg(c)) p--;
	*p = '\0';
	getDOT();
}

/*
 * Encryption routines.  These are essentially unmodified from ed.
 */

#ifdef CRYPT
/*
 * crblock: encrypt/decrypt a block of text.
 * buf is the buffer through which the text is both input and
 * output. nchar is the size of the buffer. permp is a work
 * buffer, and startn is the beginning of a sequence.
 */
crblock(permp, buf, nchar, startn)
char *permp;
char *buf;
int nchar;
long startn;
{
	register char *p1;
	int n1;
	int n2;
	register char *t1, *t2, *t3;

	t1 = permp;
	t2 = &permp[256];
	t3 = &permp[512];

	n1 = startn&0377;
	n2 = (startn>>8)&0377;
	p1 = buf;
	while(nchar--) {
		*p1 = t2[(t3[(t1[(*p1+n1)&0377]+n2)&0377]-n2)&0377]-n1;
		n1++;
		if(n1==256){
			n1 = 0;
			n2++;
			if(n2==256) n2 = 0;
		}
		p1++;
	}
}

/*
 * makekey: initialize buffers based on user key a.
 */
makekey(a, b)
char *a, *b;
{
       register int i;
	long t;
	char temp[KSIZE + 1];

	for(i = 0; i < KSIZE; i++)
		temp[i] = *a++;
	time(&t);
	t += getpid();
	for(i = 0; i < 4; i++)
		temp[i] ^= (t>>(8*i))&0377;
	crinit(temp, b);
}

/*
 * crinit: besides initializing the encryption machine, this routine
 * returns 0 if the key is null, and 1 if it is non-null.
 */
crinit(keyp, permp)
char    *keyp, *permp;
{
       register char *t1, *t2, *t3;
	register i;
	int ic, k, temp;
	unsigned random;
	char buf[13];
	long seed;

	t1 = permp;
	t2 = &permp[256];
	t3 = &permp[512];
	if(*keyp == 0)
		return(0);
	strncpy(buf, keyp, 8);
	while (*keyp)
		*keyp++ = '\0';

	buf[8] = buf[0];
	buf[9] = buf[1];
	domakekey(buf);

	seed = 123;
	for (i=0; i<13; i++)
		seed = seed*buf[i] + i;
	for(i=0;i<256;i++){
		t1[i] = i;
		t3[i] = 0;
	}
	for(i=0; i<256; i++) {
		seed = 5*seed + buf[i%13];
		random = seed % 65521;
		k = 256-1 - i;
		ic = (random&0377) % (k+1);
		random >>= 8;
		temp = t1[k];
		t1[k] = t1[ic];
		t1[ic] = temp;
		if(t3[k]!=0) continue;
		ic = (random&0377) % k;
		while(t3[ic]!=0) ic = (ic+1) % k;
		t3[k] = ic;
		t3[ic] = k;
	}
	for(i=0; i<256; i++)
		t2[t1[i]&0377] = i;
	return(1);
}

/*
 * domakekey: the following is the major nonportable part of the encryption
 * mechanism. A 10 character key is supplied in buffer.
 * This string is fed to makekey (an external program) which
 * responds with a 13 character result. This result is placed
 * in buffer.
 */
domakekey(buffer)
char *buffer;
{
       int pf[2];

	if (pipe(pf)<0)
		pf[0] = pf[1] = -1;
	if (fork()==0) {
		close(0);
		close(1);
		dup(pf[0]);
		dup(pf[1]);
		execl(_PATH_MAKEKEY, "-", 0);
		ex_exit(1);
	}
	write(pf[1], buffer, 10);
	if (wait((int *)NULL)==-1 || read(pf[0], buffer, 13)!=13)
		error("crypt: cannot generate key");
	close(pf[0]);
	close(pf[1]);
	/* end of nonportable part */
}
#endif
