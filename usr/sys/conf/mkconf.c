#include <stdio.h>

#define CHAR	01
#define BLOCK	02
#define INTR	04
#define EVEN	010
#define KL	020
#define ROOT	040
#define	SWAP	0100
#define	PIPE	0200

char	*btab[] =
{
	"rk",
	"rp",
	"rf",
	"tm",
	"tc",
	"hs",
	"hp",
	"ht",
	"rl",
	0
};
char	*ctab[] =
{
	"console",
	"pc",
	"lp",
	"dc",
	"dh",
	"dp",
	"dj",
	"dn",
	"mem",
	"rk",
	"rf",
	"rp",
	"tm",
	"hs",
	"hp",
	"ht",
	"du",
	"tty",
	"rl",
	0
};
struct tab
{
	char	*name;
	int	count;
	int	address;
	int	key;
	char	*codea;
	char	*codeb;
	char	*codec;
	char	*coded;
	char	*codee;
	char	*codef;
	char	*codeg;
} table[] =
{
	"console",
	-1, 60, CHAR+INTR+KL,
	"	klin; br4\n	klou; br4\n",
	".globl	_klrint\nklin:	jsr	r0,call; jmp _klrint\n",
	".globl	_klxint\nklou:	jsr	r0,call; jmp _klxint\n",
	"",
	"	klopen, klclose, klread, klwrite, klioctl, nulldev, 0,",
	"",
	"int	klopen(), klclose(), klread(), klwrite(), klioctl();",

	"mem",
	-1, 300, CHAR,
	"",
	"",
	"",
	"",
	"	nulldev, nulldev, mmread, mmwrite, nodev, nulldev, 0, ",
	"",
	"int	mmread(), mmwrite();",

	"pc",
	0, 70, CHAR+INTR,
	"	pcin; br4\n	pcou; br4\n",
	".globl	_pcrint\npcin:	jsr	r0,call; jmp _pcrint\n",
	".globl	_pcpint\npcou:	jsr	r0,call; jmp _pcpint\n",
	"",
	"	pcopen, pcclose, pcread, pcwrite, nodev, nulldev, 0, ",
	"",
	"int	pcopen(), pcclose(), pcread(), pcwrite();",

	"clock",
	-2, 100, INTR,
	"	kwlp; br6\n",
	".globl	_clock\n",
	"kwlp:	jsr	r0,call; jmp _clock\n",
	"",
	"",
	"",
	"",

	"parity",
	-1, 114, INTR,
	"	trap; br7+7.		/ 11/70 parity\n",
	"",
	"",
	"",
	"",
	"",
	"",

/*
 * 110 unused
 * 114 memory parity
 * 120 XY plotter
 * 124 DR11-B
 * 130 AD01 & RL01
*/

	"rl",
	0, 130, BLOCK+CHAR+INTR,
	"	rlio; br5\n",
	".globl	_rlintr\n",
	"rlio:	jsr	r0,call; jmp _rlintr\n",
	"	nulldev, nulldev, rlstrategy, &rltab,",
	"	rlopen, rlclose, rlread, rlwrite, nodev, nulldev, 0,",
	"int	rlstrategy();\nstruct	buf	rltab;",
	"int	rlopen(), rlclose(), rlread(), rlwrite();",

/*
 * 134 AFC11
 * 140 AA11
 * 144 AA11
 * 150-174 unused
 */

	"lp",
	0, 200, CHAR+INTR,
	"	lpou; br4\n",
	"",
	".globl	_lpint\nlpou:	jsr	r0,call; jmp _lpint\n",
	"",
	"	lpopen, lpclose, nodev, lpwrite, nodev, nulldev, 0,",
	"",
	"int	lpopen(), lpclose(), lpwrite();",

	"rf",
	0, 204, BLOCK+CHAR+INTR,
	"	rfio; br5\n",
	".globl	_rfintr\n",
	"rfio:	jsr	r0,call; jmp _rfintr\n",
	"	nulldev, nulldev, rfstrategy, &rftab, ",
	"	nulldev, nulldev, rfread, rfwrite, nodev, nulldev, 0,",
	"int	rfstrategy();\nstruct	buf	rftab;",
	"int	rfread(), rfwrite();",

	"hs",
	0, 204, BLOCK+CHAR+INTR,
	"	hsio; br5\n",
	".globl	_hsintr\n",
	"hsio:	jsr	r0,call; jmp _hsintr\n",
	"	nulldev, nulldev, hsstrategy, &hstab, ",
	"	nulldev, nulldev, hsread, hswrite, nodev, nulldev, 0,",
	"int	hsstrategy();\nstruct	buf	hstab;",
	"int	hsread(), hswrite();",

/*
 * 210 RC
 */

	"tc",
	0, 214, BLOCK+INTR,
	"	tcio; br6\n",
	".globl	_tcintr\n",
	"tcio:	jsr	r0,call; jmp _tcintr\n",
	"	nulldev, tcclose, tcstrategy, &tctab,",
	"",
	"int	tcstrategy(), tcclose();\nstruct	buf	tctab;",
	"",

	"rk",
	0, 220, BLOCK+CHAR+INTR,
	"	rkio; br5\n",
	".globl	_rkintr\n",
	"rkio:	jsr	r0,call; jmp _rkintr\n",
	"	nulldev, nulldev, rkstrategy, &rktab,",
	"	nulldev, nulldev, rkread, rkwrite, nodev, nulldev, 0,",
	"int	rkstrategy();\nstruct	buf	rktab;",
	"int	rkread(), rkwrite();",

	"tm",
	0, 224, BLOCK+CHAR+INTR,
	"	tmio; br5\n",
	".globl	_tmintr\n",
	"tmio:	jsr	r0,call; jmp _tmintr\n",
	"	tmopen, tmclose, tmstrategy, &tmtab, ",
	"	tmopen, tmclose, tmread, tmwrite, nodev, nulldev, 0,",
	"int	tmopen(), tmclose(), tmstrategy();\nstruct	buf	tmtab;",
	"int	tmread(), tmwrite();",

	"ht",
	0, 224, BLOCK+CHAR+INTR,
	"	htio; br5\n",
	".globl	_htintr\n",
	"htio:	jsr	r0,call; jmp _htintr\n",
	"	htopen, htclose, htstrategy, &httab,",
	"	htopen, htclose, htread, htwrite, nodev, nulldev, 0,",
	"int	htopen(), htclose(), htstrategy();\nstruct	buf	httab;",
	"int	htread(), htwrite();",

	"cr",
	0, 230, CHAR+INTR,
	"	crin; br6\n",
	"",
	".globl	_crint\ncrin:	jsr	r0,call; jmp _crint\n",
	"",
	"	cropen, crclose, crread, nodev, nodev, nulldev, 0,",
	"",
	"int	cropen(), crclose(), crread();",

/*
 * 234 UDC11
 */

	"rp",
	0, 254, BLOCK+CHAR+INTR,
	"	rpio; br5\n",
	".globl	_rpintr\n",
	"rpio:	jsr	r0,call; jmp _rpintr\n",
	"	nulldev, nulldev, rpstrategy, &rptab,",
	"	nulldev, nulldev, rpread, rpwrite, nodev, nulldev, 0,",
	"int	rpstrategy();\nstruct	buf	rptab;",
	"int	rpread(), rpwrite();",

	"hp",
	0, 254, BLOCK+CHAR+INTR,
	"	hpio; br5\n",
	".globl	_hpintr\n",
	"hpio:	jsr	r0,call; jmp _hpintr\n",
	"	nulldev, nulldev, hpstrategy, &hptab,",
	"	nulldev, nulldev, hpread, hpwrite, nodev, nulldev, 0,",
	"int	hpstrategy();\nstruct	buf	hptab;",
	"int	hpread(), hpwrite();",

/*
 * 260 TA11
 * 264-274 unused
 */

	"dc",
	0, 308, CHAR+INTR,
	"	dcin; br5+%d.\n	dcou; br5+%d.\n",
	".globl	_dcrint\ndcin:	jsr	r0,call; jmp _dcrint\n",
	".globl	_dcxint\ndcou:	jsr	r0,call; jmp _dcxint\n",
	"",
	"	dcopen, dcclose, dcread, dcwrite, dcioctl, nulldev, dc11,",
	"",
	"int	dcopen(), dcclose(), dcread(), dcwrite(), dcioctl();\nstruct	tty	dc11[];",

	"kl",
	0, 308, INTR+KL,
	"	klin; br4+%d.\n	klou; br4+%d.\n",
	"",
	"",
	"",
	"",
	"",
	"",

	"dp",
	0, 308, CHAR+INTR,
	"	dpin; br6+%d.\n	dpou; br6+%d.\n",
	".globl	_dprint\ndpin:	jsr	r0,call; jmp _dprint\n",
	".globl	_dpxint\ndpou:	jsr	r0,call; jmp _dpxint\n",
	"",
	"	dpopen, dpclose, dpread, dpwrite, nodev, nulldev, 0,",
	"",
	"int	dpopen(), dpclose(), dpread(), dpwrite();",

/*
 * DM11-A
 */

	"dn",
	0, 304, CHAR+INTR,
	"	dnou; br5+%d.\n",
	"",
	".globl	_dnint\ndnou:	jsr	r0,call; jmp _dnint\n",
	"",
	"	dnopen, dnclose, nodev, dnwrite, nodev, nulldev, 0,",
	"",
	"int	dnopen(), dnclose(), dnwrite();",

	"dhdm",
	0, 304, INTR,
	"	dmin; br4+%d.\n",
	"",
	".globl	_dmint\ndmin:	jsr	r0,call; jmp _dmint\n",
	"",
	"",
	"",
	"",

/*
 * DR11-A+
 * DR11-C+
 * PA611+
 * PA611+
 * DT11+
 * DX11+
 */

	"dl",
	0, 308, INTR+KL,
	"	klin; br4+%d.\n	klou; br4+%d.\n",
	"",
	"",
	"",
	"",
	"",
	"",

/*
 * DJ11
 */

	"dh",
	0, 308, CHAR+INTR+EVEN,
	"	dhin; br5+%d.\n	dhou; br5+%d.\n",
	".globl	_dhrint\ndhin:	jsr	r0,call; jmp _dhrint\n",
	".globl	_dhxint\ndhou:	jsr	r0,call; jmp _dhxint\n",
	"",
	"	dhopen, dhclose, dhread, dhwrite, dhioctl, dhstop, dh11,",
	"",
	"int	dhopen(), dhclose(), dhread(), dhwrite(), dhioctl(), dhstop();\nstruct	tty	dh11[];",

/*
 * GT40
 * LPS+
 * DQ11
 * KW11-W
 */

	"du",
	0, 308, CHAR+INTR,
	"	duin; br6+%d.\n	duou; br6+%d.\n",
	".globl	_durint\nduin:	jsr	r0,call; jmp _durint\n",
	".globl	_duxint\nduou:	jsr	r0,call; jmp _duxint\n",
	"",
	"	duopen, duclose, duread, duwrite, nodev, nulldev, 0,",
	"",
	"int	duopen(), duclose(), duread(), duwrite();",

	"tty",
	1, 0, CHAR,
	"",
	"",
	"",
	"",
	"	syopen, nulldev, syread, sywrite, sysioctl, nulldev, 0,",
	"",
	"int	syopen(), syread(), sywrite(), sysioctl();",

	0
};

char	*stra[] =
{
	"/ low core",
	"",
	".data",
	"ZERO:",
	"",
	"br4 = 200",
	"br5 = 240",
	"br6 = 300",
	"br7 = 340",
	"",
	". = ZERO+0",
	"	br	1f",
	"	4",
	"",
	"/ trap vectors",
	"	trap; br7+0.		/ bus error",
	"	trap; br7+1.		/ illegal instruction",
	"	trap; br7+2.		/ bpt-trace trap",
	"	trap; br7+3.		/ iot trap",
	"	trap; br7+4.		/ power fail",
	"	trap; br7+5.		/ emulator trap",
	"	start;br7+6.		/ system  (overlaid by 'trap')",
	"",
	". = ZERO+40",
	".globl	start, dump",
	"1:	jmp	start",
	"	jmp	dump",
	"",
	0,
};

char	*strb[] =
{
	"",
	". = ZERO+240",
	"	trap; br7+7.		/ programmed interrupt",
	"	trap; br7+8.		/ floating point",
	"	trap; br7+9.		/ segmentation violation",
	0
};

char	*strc[] =
{
	"",
	"/ floating vectors",
	". = ZERO+300",
	0,
};

char	*strd[] =
{
	"",
	"//////////////////////////////////////////////////////",
	"/		interface code to C",
	"//////////////////////////////////////////////////////",
	"",
	".text",
	".globl	call, trap",
	0
};

char	*stre[] =
{
	"#include \"../h/param.h\"",
	"#include \"../h/systm.h\"",
	"#include \"../h/buf.h\"",
	"#include \"../h/tty.h\"",
	"#include \"../h/conf.h\"",
	"#include \"../h/proc.h\"",
	"#include \"../h/text.h\"",
	"#include \"../h/dir.h\"",
	"#include \"../h/user.h\"",
	"#include \"../h/file.h\"",
	"#include \"../h/inode.h\"",
	"#include \"../h/acct.h\"",
	"",
	"int	nulldev();",
	"int	nodev();",
	0
};

char	*stre1[] =
{
	"struct	bdevsw	bdevsw[] =",
	"{",
	0,
};

char	*strf[] =
{
	"	0",
	"};",
	"",
	0,
};

char	*strf1[] =
{
	"",
	"struct	cdevsw	cdevsw[] =",
	"{",
	0,
};

char	strg[] =
{
"	0\n\
};\n\
int	rootdev	= makedev(%d, %d);\n\
int	swapdev	= makedev(%d, %d);\n\
int	pipedev = makedev(%d, %d);\n\
int	nldisp = %d;\n\
daddr_t	swplo	= %ld;\n\
int	nswap	= %l;\n\
"};

char	strg1[] =
{
"	\n\
struct	buf	buf[NBUF];\n\
struct	file	file[NFILE];\n\
struct	inode	inode[NINODE];\n"
};

char	*strg1a[] =
{
	"int	mpxchan();",
	"int	(*ldmpx)() = mpxchan;",
	0
};

char	strg2[] =
{
"struct	proc	proc[NPROC];\n\
struct	text	text[NTEXT];\n\
struct	buf	bfreelist;\n\
struct	acct	acctbuf;\n\
struct	inode	*acctp;\n"
};

char	*strh[] =
{
	"	0",
	"};",
	"",
	"int	ttyopen(), ttyclose(), ttread(), ttwrite(), ttyinput(), ttstart();",
	0
};

char	*stri[] =
{
	"int	pkopen(), pkclose(), pkread(), pkwrite(), pkioctl(), pkrint(), pkxint();",
	0
};

char	*strj[] =
{
	"struct	linesw	linesw[] =",
	"{",
	"	ttyopen, nulldev, ttread, ttwrite, nodev, ttyinput, ttstart, /* 0 */",
	0
};

char	*strk[] =
{
	"	pkopen, pkclose, pkread, pkwrite, pkioctl, pkrint, pkxint, /* 1 */",
	0
};

int	pack;
int	mpx;
int	rootmaj = -1;
int	rootmin;
int	swapmaj = -1;
int	swapmin;
int	pipemaj = -1;
int	pipemin;
long	swplo	= 4000;
int	nswap = 872;
int	pack;
int	nldisp = 1;

main()
{
	register struct tab *p;
	register char *q;
	int i, n, ev, nkl;
	int flagf, flagb, dumpht;

	while(input());

/*
 * pass1 -- create interrupt vectors
 */
	nkl = 0;
	flagf = flagb = 1;
	freopen("l.s", "w", stdout);
	puke(stra);
	ev = 0;
	for(p=table; p->name; p++)
	if(p->count != 0 && p->key & INTR) {
		if(p->address>240 && flagb) {
			flagb = 0;
			puke(strb);
		}
		if(p->address >= 300) {
			if(flagf) {
				ev = 0;
				flagf = 0;
				puke(strc);
			}
			if(p->key & EVEN && ev & 07) {
				printf("	.=.+4\n");
				ev += 4;
			}
			printf("/%s %o\n", p->name, 0300+ev);
		} else
			printf("\n. = ZERO+%d\n", p->address);
		n = p->count;
		if(n < 0)
			n = -n;
		for(i=0; i<n; i++) {
			if(p->key & KL) {
				printf(p->codea, nkl, nkl);
				nkl++;
			} else
				printf(p->codea, i, i);
			if (p->address<300)
				fprintf(stderr, "%s at %d\n", p->name, p->address+4*i);
			else
				fprintf(stderr, "%s at %o\n", p->name, 0300+ev);
			ev += p->address - 300;
		}
	}
	if(flagb)
		puke(strb);
	puke(strd);
	for(p=table; p->name; p++)
	if(p->count != 0 && p->key & INTR)
		printf("\n%s%s", p->codeb, p->codec);

/*
 * pass 2 -- create configuration table
 */

	freopen("c.c", "w", stdout);
	/*
	 * declarations
	 */
	puke(stre);
	for (i=0; q=btab[i]; i++) {
		for (p=table; p->name; p++)
		if (equal(q, p->name) &&
		   (p->key&BLOCK) && p->count && *p->codef)
			printf("%s\n", p->codef);
	}
	puke(stre1);
	for(i=0; q=btab[i]; i++) {
		for(p=table; p->name; p++)
		if(equal(q, p->name) &&
		   (p->key&BLOCK) && p->count) {
			printf("%s	/* %s = %d */\n", p->coded, q, i);
			if(p->key & ROOT)
				rootmaj = i;
			if (p->key & SWAP)
				swapmaj = i;
			if (p->key & PIPE)
				pipemaj = i;
			goto newb;
		}
		printf("	nodev, nodev, nodev, 0, /* %s = %d */\n", q, i);
	newb:;
	}
	if (swapmaj == -1) {
		swapmaj = rootmaj;
		swapmin = rootmin;
	}
	if (pipemaj == -1) {
		pipemaj = rootmaj;
		pipemin = rootmin;
	}
	puke(strf);
	for (i=0; q=ctab[i]; i++) {
		for (p=table; p->name; p++)
		if (equal(q, p->name) &&
		   (p->key&CHAR) && p->count && *p->codeg)
			printf("%s\n", p->codeg);
	}
	puke(strf1);
	for(i=0; q=ctab[i]; i++) {
		for(p=table; p->name; p++)
		if(equal(q, p->name) &&
		   (p->key&CHAR) && p->count) {
			printf("%s	/* %s = %d */\n", p->codee, q, i);
			goto newc;
		}
		printf("	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* %s = %d */\n", q, i);
	newc:;
	}
	puke(strh);
	if (pack) {
		nldisp++;
		puke(stri);
	}
	puke(strj);
	if (pack)
		puke(strk);
	printf(strg, rootmaj, rootmin,
		swapmaj, swapmin,
		pipemaj, pipemin,
		nldisp,
		swplo, nswap);
	printf(strg1);
	if (!mpx)
		puke(strg1a);
	printf(strg2);
	if(rootmaj < 0)
		fprintf(stderr, "No root device given\n");
	freopen("mch0.s", "w", stdout);
	dumpht = 0;
	for (i=0; table[i].name; i++) {
		if (equal(table[i].name, "ht") && table[i].count)
			dumpht = 1;
	}
	if (dumpht) {
		printf("HTDUMP = 1\n");
		printf("TUDUMP = 0\n");
	} else {
		printf("HTDUMP = 0\n");
		printf("TUDUMP = 1\n");
	}
}

puke(s, a)
char **s;
{
	char *c;

	while(c = *s++) {
		printf(c, a);
		printf("\n");
	}
}

input()
{
	char line[100];
	register struct tab *q;
	int count, n;
	long num;
	char keyw[32], dev[32];

	if (fgets(line, 100, stdin) == NULL)
		return(0);
	count = -1;
	n = sscanf(line, "%d%s%s%ld", &count, keyw, dev, &num);
	if (count == -1 && n>0) {
		count = 1;
		n++;
	}
	if (n<2)
		goto badl;
	for(q=table; q->name; q++)
	if(equal(q->name, keyw)) {
		if(q->count < 0) {
			fprintf(stderr, "%s: no more, no less\n", keyw);
			return(1);
		}
		q->count += count;
		if(q->address < 300 && q->count > 1) {
			q->count = 1;
			fprintf(stderr, "%s: only one\n", keyw);
		}
		return(1);
	}
	if (equal(keyw, "nswap")) {
		if (n<3)
			goto badl;
		if (sscanf(dev, "%ld", &num) <= 0)
			goto badl;
		nswap = num;
		return(1);
	}
	if (equal(keyw, "swplo")) {
		if (n<3)
			goto badl;
		if (sscanf(dev, "%ld", &num) <= 0)
			goto badl;
		swplo = num;
		return(1);
	}
	if (equal(keyw, "pack")) {
		pack++;
		return(1);
	}
	if (equal(keyw, "mpx")) {
		mpx++;
		return(1);
	}
	if(equal(keyw, "done"))
		return(0);
	if (equal(keyw, "root")) {
		if (n<4)
			goto badl;
		for (q=table; q->name; q++) {
			if (equal(q->name, dev)) {
				q->key |= ROOT;
				rootmin = num;
				return(1);
			}
		}
		fprintf(stderr, "Can't find root\n");
		return(1);
	}
	if (equal(keyw, "swap")) {
		if (n<4)
			goto badl;
		for (q=table; q->name; q++) {
			if (equal(q->name, dev)) {
				q->key |= SWAP;
				swapmin = num;
				return(1);
			}
		}
		fprintf(stderr, "Can't find swap\n");
		return(1);
	}
	if (equal(keyw, "pipe")) {
		if (n<4)
			goto badl;
		for (q=table; q->name; q++) {
			if (equal(q->name, dev)) {
				q->key |= PIPE;
				pipemin = num;
				return(1);
			}
		}
		fprintf(stderr, "Can't find pipe\n");
		return(1);
	}
	fprintf(stderr, "%s: cannot find\n", keyw);
	return(1);
badl:
	fprintf(stderr, "Bad line: %s", line);
	return(1);
}

equal(a, b)
char *a, *b;
{
	return(!strcmp(a, b));
}
