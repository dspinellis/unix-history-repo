#define CHAR	01
#define BLOCK	02
#define INTR	04
#define EVEN	010
#define KL	020
int	tabp[20];
int	tabpp;
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
} table[]
{
	"console",
	-1,	60,	CHAR+INTR+KL,
	"\tklin; br4\n\tklou; br4\n",
	".globl\t_klrint\nklin:\tjsr\tr0,call; _klrint\n",
	".globl\t_klxint\nklou:\tjsr\tr0,call; _klxint\n",
	"",
	"\t&klopen,   &klclose,   &klread,   &klwrite,   &klsgtty,",
	"mem",
	-1,	300,	CHAR,
	"",
	"",
	"",
	"",
	"\t&nulldev,  &nulldev,   &mmread,   &mmwrite,   &nodev,",


	"pc",
	0,	70,	CHAR+INTR,
	"\tpcin; br4\n\tpcou; br4\n",
	".globl\t_pcrint\npcin:\tjsr\tr0,call; _pcrint\n",
	".globl\t_pcpint\npcou:\tjsr\tr0,call; _pcpint\n",
	"",
	"\t&pcopen,   &pcclose,   &pcread,   &pcwrite,   &nodev,",

	"clock",
	-2,	100,	INTR,
	"\tkwlp; br6\n",
	".globl\t_clock\n",
	"kwlp:\tjsr\tr0,call; _clock\n",
	"",
	"",

/*
 * 110 unused
 * 114 memory parity
 * 120 XY plotter
 * 124 DR11-B
 * 130 AD01
 * 134 AFC11
 * 140 AA11
 * 144 AA11
 * 150-174 unused
 */

	"lp",
	0,	200,	CHAR+INTR,
	"\tlpou; br4\n",
	"",
	".globl\t_lpint\nlpou:\tjsr\tr0,call; _lpint\n",
	"",
	"\t&lpopen,   &lpclose,   &nodev,    &lpwrite,   &nodev,",

	"rf",
	0,	204,	BLOCK+CHAR+INTR,
	"\trfio; br5\n",
	".globl\t_rfintr\n",
	"rfio:\tjsr\tr0,call; _rfintr\n",
	"\t&nulldev,\t&nulldev,\t&rfstrategy, \t&rftab,",
	"\t&nulldev,  &nulldev,   &rfread,   &rfwrite,   &nodev,",

/*
 * 210 RC
 */

	"tc",
	0,	214,	BLOCK+INTR,
	"\ttcio; br6\n",
	".globl\t_tcintr\n",
	"tcio:\tjsr\tr0,call; _tcintr\n",
	"\t&nulldev,\t&tcclose,\t&tcstrategy, \t&tctab,",
	"",

	"rk",
	0,	220,	BLOCK+CHAR+INTR,
	"\trkio; br5\n",
	".globl\t_rkintr\n",
	"rkio:\tjsr\tr0,call; _rkintr\n",
	"\t&nulldev,\t&nulldev,\t&rkstrategy, \t&rktab,",
	"\t&nulldev,  &nulldev,   &rkread,   &rkwrite,   &nodev,",

	"tm",
	0,	224,	BLOCK+CHAR+INTR,
	"\ttmio; br5\n",
	".globl\t_tmintr\n",
	"tmio:\tjsr\tr0,call; _tmintr\n",
	"\t&tmopen,\t&tmclose,\t&tmstrategy, \t&tmtab,",
	"\t&tmopen,   &tmclose,   &tmread,   &tmwrite,   &nodev,",

	"cr",
	0,	230,	CHAR+INTR,
	"\tcrin; br6\n",
	"",
	".globl\t_crint\ncrin:\tjsr\tr0,call; _crint\n",
	"",
	"\t&cropen,   &crclose,   &crread,   &nodev,     &nodev,",

/*
 * 234 UDC11
 */

	"rp",
	0,	254,	BLOCK+CHAR+INTR,
	"\trpio; br5\n",
	".globl\t_rpintr\n",
	"rpio:\tjsr\tr0,call; _rpintr\n",
	"\t&nulldev,\t&nulldev,\t&rpstrategy, \t&rptab,",
	"\t&nulldev,  &nulldev,   &rpread,   &rpwrite,   &nodev,",

/*
 * 260 TA11
 * 264-274 unused
 */

	"dc",
	0,	308,	CHAR+INTR,
	"\tdcin; br5+%d.\n\tdcou; br5+%d.\n",
	".globl\t_dcrint\ndcin:\tjsr\tr0,call; _dcrint\n",
	".globl\t_dcxint\ndcou:\tjsr\tr0,call; _dcxint\n",
	"",
	"\t&dcopen,   &dcclose,   &dcread,   &dcwrite,   &dcsgtty,",

	"kl",
	0,	308,	INTR+KL,
	"\tklin; br4+%d.\n\tklou; br4+%d.\n",
	"",
	"",
	"",
	"",

	"dp",
	0,	308,	CHAR+INTR,
	"\tdpin; br6+%d.\n\tdpou; br6+%d.\n",
	".globl\t_dprint\ndpin:\tjsr\tr0,call; _dprint\n",
	".globl\t_dpxint\ndpou:\tjsr\tr0,call; _dpxint\n",
	"",
	"\t&dpopen,   &dpclose,   &dpread,   &dpwrite,   &nodev,",

/*
 * DM11-A
 */

	"dn",
	0,	304,	CHAR+INTR,
	"\tdnou; br5+%d.\n",
	"",
	".globl\t_dnint\ndnou:\tjsr\tr0,call; _dnint\n",
	"",
	"\t&dnopen,   &dnclose,   &nodev,    &dnwrite,   &nodev,",

	"dhdm",
	0,	304,	INTR,
	"\tdmin; br4+%d.\n",
	"",
	".globl\t_dmint\ndmin:\tjsr\tr0,call; _dmint\n",
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
	0,	308,	INTR+KL,
	"\tklin; br4+%d.\n\tklou; br4+%d.\n",
	"",
	"",
	"",
	"",

/*
 * DJ11
 */

	"dh",
	0,	308,	CHAR+INTR+EVEN,
	"\tdhin; br5+%d.\n\tdhou; br5+%d.\n",
	".globl\t_dhrint\ndhin:\tjsr\tr0,call; _dhrint\n",
	".globl\t_dhxint\ndhou:\tjsr\tr0,call; _dhxint\n",
	"",
	"\t&dhopen,   &dhclose,   &dhread,   &dhwrite,   &dhsgtty,",

/*
 * GT40
 * LPS+
 * VT20
 */

	0
};

char	*stra[]
{
	"/ Copyright 1974 Bell Telephone Laboratories Inc",
	"/ low core",
	"",
	"br4 = 200",
	"br5 = 240",
	"br6 = 300",
	"br7 = 340",
	"",
	". = 0^.",
	"\tbr\t1f",
	"\t4",
	"",
	"/ trap vectors",
	"\ttrap; br7+0.\t\t/ bus error",
	"\ttrap; br7+1.\t\t/ illegal instruction",
	"\ttrap; br7+2.\t\t/ bpt-trace trap",
	"\ttrap; br7+3.\t\t/ iot trap",
	"\ttrap; br7+4.\t\t/ power fail",
	"\ttrap; br7+5.\t\t/ emulator trap",
	"\ttrap; br7+6.\t\t/ system entry",
	"",
	". = 40^.",
	".globl\tstart, dump",
	"1:\tjmp\tstart",
	"\tjmp\tdump",
	"",
	0,
};

char	*strb[]
{
	"",
	". = 240^.",
	"\ttrap; br7+7.\t\t/ programmed interrupt",
	"\ttrap; br7+8.\t\t/ floating point",
	"\ttrap; br7+9.\t\t/ segmentation violation",
	0
};

char	*strc[]
{
	"",
	"/ floating vectors",
	". = 300^.",
	0,
};

char	*strd[]
{
	"",
	"//////////////////////////////////////////////////////",
	"/\t\tinterface code to C",
	"//////////////////////////////////////////////////////",
	"",
	".globl\tcall, trap",
	0
};

char	*stre[]
{
	"/*",
	" *\tCopyright 1974 Bell Telephone Laboratories Inc",
	" */",
	"",
	"int\t(*bdevsw[])()",
	"{",
	0,
};

char	*strf[]
{
	"\t0",
	"};",
	"",
	"int\t(*cdevsw[])()",
	"{",
	0,
};

char	*strg[]
{
	"\t0",
	"};",
	"",
	"int\trootdev\t{(0<<8)|0};",
	"int\tswapdev\t{(0<<8)|0};",
	"int\tswplo\t4000;",
	"int\tnswap\t872;",
	0,
};

int	fout;

main()
{
	register struct tab *p;
	int i, n, ev, nkl;
	int flagf, flagb;

	tabp[tabpp++] = table;
	tabp[tabpp++] = table+1;
	while(input());

/*
 * pass1 -- create interrupt vectors
 */
	nkl = 0;
	flagf = flagb = 1;
	fout = creat("l.s", 0666);
	puke(stra);
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
				printf("\t.=.+4\n");
				ev =+ 4;
			}
			ev =+ p->address - 300;
		} else
			printf("\n. = %d^.\n", p->address);
		n = p->count;
		if(n < 0)
			n = -n;
		for(i=0; i<n; i++)
			if(p->key & KL) {
				printf(p->codea, nkl, nkl);
				nkl++;
			} else
			printf(p->codea, i, i);
	}
	if(flagb)
		puke(strb);
	puke(strd);
	for(p=table; p->name; p++)
	if(p->count != 0 && p->key & INTR)
		printf("\n%s%s", p->codeb, p->codec);
	flush();
	close(fout);

/*
 * pass 2 -- create configuration table
 */

	fout = creat("c.c", 0666);
	puke(stre);
	for(i=0; p=tabp[i]; i++)
	if(p->key & BLOCK)
		printf("%s\n", p->coded);
	puke(strf);
	for(i=0; p=tabp[i]; i++)
	if(p->key & CHAR)
		printf("%s\n", p->codee);
	puke(strg);
	flush();
	close(fout);
}

puke(s)
char **s;
{
	char *c;

	while(c = *s++)
		printf("%s\n", c);
}

input()
{
	char line[100];
	register char *p;
	register struct tab *q;
	register n;

	p = line;
	while((n=getchar()) != '\n') {
		if(n == 0)
			return(0);
		if(n == ' ' || n == '\t')
			continue;
		*p++ = n;
	}
	*p++ = 0;
	n = 0;
	p = line;
	while(*p>='0' && *p<='9') {
		n =* 10;
		n =+ *p++ - '0';
	}
	if(n == 0)
		n = 1;
	if(*p == 0)
		return(1);
	for(q=table; q->name; q++)
	if(equal(q->name, p)) {
		if(q->count < 0) {
			printf("%s: no more, no less\n", p);
			return(1);
		}
		q->count =+ n;
		if(q->address < 300 && q->count > 1) {
			q->count = 1;
			printf("%s: only one\n", p);
		}
		tabp[tabpp++] = q;
		return(1);
	}
	printf("%s: cannot find\n", p);
	return(1);
}

equal(a, b)
char *a, *b;
{

	while(*a++ == *b)
		if(*b++ == 0)
			return(1);
	return(0);
}
