#
/*
 * C debugger -- part 2
 */

char	ssymbol[];
int	dotinc;
int	dot;


psymoff(v, lim)
{
	register char *w;

	w = vallook(v);
	if (w > lim) {
		printf("%.1o", v);
		return;
	}
	printf("%.8s", ssymbol);
	if (w)
		printf("+%d", w);
}

#define	ISP	1
#define	DOUBLE	0
#define	SINGLE	1
#define	SINGLW	2
#define	MULDIV	4
#define	BRANCH	5
#define	NOADDR	6
#define	FLTREV	7
#define	FLTNOR	8
#define	SPECL1	9
#define	SPECL2	10
#define	SPECL3	11
#define	SPECL4	12
#define	SPECL5	13
#define	SPECL6	14
#define	SPECL7	15
#define	SPECL8	18
#define	SPECL9	19

struct optab {
	int	mask;
	int	val;
	int	itype;
	char	*iname;
} optab[] {
	0107777, 0010000, DOUBLE, "mov",
	0107777, 0020000, DOUBLE, "cmp",
	0107777, 0030000, DOUBLE, "bit",
	0107777, 0040000, DOUBLE, "bic",
	0107777, 0050000, DOUBLE, "bis",
	0007777, 0060000, DOUBLE, "add",
	0007777, 0160000, DOUBLE, "su",
	0100077, 0005000, SINGLE, "clr",
	0100077, 0005100, SINGLE, "com",
	0100077, 0005200, SINGLE, "inc",
	0100077, 0005300, SINGLE, "dec",
	0100077, 0005400, SINGLE, "neg",
	0100077, 0005500, SINGLE, "adc",
	0100077, 0005600, SINGLE, "sbc",
	0100077, 0005700, SINGLE, "tst",
	0100077, 0006000, SINGLE, "ror",
	0100077, 0006100, SINGLE, "rol",
	0100077, 0006200, SINGLE, "asr",
	0100077, 0006300, SINGLE, "asl",
	0000077, 0000100, SINGLE, "jmp",
	0000077, 0000300, SINGLE, "swab",
	0000077, 0170100, SINGLW, "ldfps",
	0000077, 0170200, SINGLW, "stfps",
	0000077, 0170300, SINGLW, "stst",
	0000077, 0170400, SINGLW, "clrf",
	0000077, 0170500, SINGLW, "tstf",
	0000077, 0170600, SINGLW, "absf",
	0000077, 0170700, SINGLW, "negf",
	0000077, 0006700, SINGLW, "sxt",
	0000077, 0006600, SINGLW, "mtpi",
	0000077, 0106600, SINGLW, "mtpd",
	0000077, 0006500, SINGLW, "mfpi",
	0000077, 0106500, SINGLW, "mfpd",
	0000777, 0070000, MULDIV, "mul",
	0000777, 0071000, MULDIV, "div",
	0000777, 0072000, MULDIV, "ash",
	0000777, 0073000, MULDIV, "ashc",
	0000377, 0000400, BRANCH, "br",
	0000377, 0001000, BRANCH, "bne",
	0000377, 0001400, BRANCH, "beq",
	0000377, 0002000, BRANCH, "bge",
	0000377, 0002400, BRANCH, "blt",
	0000377, 0003000, BRANCH, "bgt",
	0000377, 0003400, BRANCH, "ble",
	0000377, 0100000, BRANCH, "bpl",
	0000377, 0100400, BRANCH, "bmi",
	0000377, 0101000, BRANCH, "bhi",
	0000377, 0101400, BRANCH, "blos",
	0000377, 0102000, BRANCH, "bvc",
	0000377, 0102400, BRANCH, "bvs",
	0000377, 0103000, BRANCH, "bhis",
	0000377, 0103400, BRANCH, "blo",
	0000000, 0000000, NOADDR, "halt",
	0000000, 0000001, NOADDR, "wait",
	0000000, 0000002, NOADDR, "rti",
	0000000, 0000004, NOADDR, "iot",
	0000000, 0000005, NOADDR, "reset",
	0000377, 0171000, FLTREV, "mulf",
	0000377, 0171400, FLTREV, "modf",
	0000377, 0172000, FLTREV, "addf",
	0000377, 0172400, FLTREV, "movf",
	0000377, 0173000, FLTREV, "subf",
	0000377, 0173400, FLTREV, "cmpf",
	0000377, 0174000, FLTNOR, "movf",
	0000377, 0174400, FLTREV, "divf",
	0000377, 0175000, FLTNOR, "movei",
	0000377, 0175400, FLTNOR, "movfi",
	0000377, 0176000, FLTNOR, "movfo",
	0000377, 0176400, FLTREV, "movie",
	0000377, 0177000, FLTREV, "movif",
	0000377, 0177400, FLTREV, "movof",
	0000000, 0170000, NOADDR, "cfcc",
	0000000, 0170001, NOADDR, "setf",
	0000000, 0170002, NOADDR, "seti",
	0000000, 0170011, NOADDR, "setd",
	0000000, 0170012, NOADDR, "setl",
	0000777, 0004000, SPECL1, "jsr",
	0000777, 0074000, SPECL1, "xor",
	0000007, 0000200, SPECL2, "rts",
	0000017, 0000240, SPECL3, "cflg",
	0000017, 0000260, SPECL3, "sflg",
	0000377, 0104000, SPECL4, "emt",
	0000377, 0104400, SPECL5, "sys",
	0000077, 0006400, SPECL7, "mark",
	0000777, 0077000, SPECL8, "sob",
	0000007, 0000230, SPECL9, "spl",
	0177777, 0000000, SPECL6, "oct",
};

struct systab {
	int	argc;
	char	*sname;
} systab[] {
	1, "indir",
	0, "exit",
	0, "fork",
	2, "read",
	2, "write",
	2, "open",
	0, "close",
	0, "wait",
	2, "creat",
	2, "link",
	1, "unlink",
	2, "exec",
	1, "chdir",
	0, "time",
	3, "mknod",
	2, "chmod",
	2, "chown",
	1, "break",
	2, "stat",
	2, "seek",
	0, "getpid",
	3, "mount",
	1, "umount",
	0, "setuid",
	0, "getuid",
	0, "stime",
	3, "ptrace",
	0, "27",
	1, "fstat",
	0, "29",
	1, "smdate",
	1, "stty",
	1, "gtty",
	0, "33",
	0, "nice",
	0, "sleep",
	0, "sync",
	1, "kill",
	0, "switch",
	0, "39",
	0, "40",
	0, "dup",
	0, "pipe",
	1, "times",
	4, "profil",
	0, "45",
	0, "setgid",
	0, "getgid",
	2, "signal",
	0, "49",
	0, "50",
	0, "51",
	0, "52",
	0, "53",
	0, "54",
	0, "55",
	0, "56",
	0, "57",
	0, "58",
	0, "59",
	0, "60",
	0, "61",
	0, "62",
	0, "63",
};

char	*regname[] { "r0", "r1", "r2", "r3", "r4", "r5", "sp", "pc"};

printins(f)
{
	register ins, w;
	register struct optab *p;

	dotinc = 2;
	ins = cget(dot, ISP);
	if (vallook(dot)==0)
		printf("%.8s:", ssymbol);
	printf("\t");
	for (p=optab;; p++)
		if ((ins & ~p->mask) == p->val)
			break;
	printf("%s", p->iname);
	switch (p->itype) {

	/* rts */
	case SPECL2:
		ins =& 07;

	case SINGLE:
		if (ins < 0)
			printf("b");

	case SINGLW:
		printf("\t");
		paddr(ins);
		return;

	case FLTREV:
		ins =& 0377;

	case MULDIV:
		ins = ((ins>>6)&07) | ((ins<<6)&07700);
		goto doub;

	case FLTNOR:
		ins =& 0377;

	/* jsr, xor */
	case SPECL1:
		ins =& 0777;
		goto doub;

	case DOUBLE:
		if (ins<0)
			printf("b");
	doub:
		printf("\t");
		paddr(ins>>6);
		printf(",");
		paddr(ins);
		return;

	case NOADDR:
		return;

	/* sob */
	case SPECL8:
		printf("\t");
		paddr((ins>>6)&07);
		printf(",");
		ins = - (ins&077);
		goto bran;

	case BRANCH:
		printf("\t");
	bran:
		ins =& 0377;
		if (ins&0200)
			ins =| 0177400;
		ins = dot + (ins<<1) + 2;
		psymoff(ins, 010000);
		return;

	/* emt */
	case SPECL4:
		ins =& 0377;
	/* mark */
	case SPECL7:
		ins =& 077;
	/* spl */
	case SPECL9:
		ins =& 07;
		printf("\t%d", ins);
		return;

	/* sys */
	case SPECL5:
		printf("\t%s", systab[ins =& 077].sname);
		if (ins==0 && f==0) {	/* indir */
			w = dot;
			dot = cget(dot+2, ISP);
			printf(" {");
			printins(1);
			printf("}");
			dotinc = 4;
			dot = w;
			return;
		}
		w = systab[ins].argc;
		while (w--) {
			printf("; ");
			psymoff(cget(dot+dotinc, ISP), 010000);
			dotinc =+ 2;
		}
		return;

	default:
		printf("\t%.1o", ins);
	}
}

paddr(aa)
{
	register a, r;

	a = aa;
	r = a&07;
	a =& 070;
	if (r==7 && a&020) {
		if (a&010)
			printf("*");
		if (a&040)
			psymoff(cget(dot+dotinc, ISP)+dot+dotinc+2, 010000);
		else {
			printf("$");
			psymoff(cget(dot+dotinc, ISP), 010000);
		}
		dotinc =+ 2;
		return;
	}
	r = regname[r];
	switch (a) {
	/* r */
	case 000:
		printf("%s", r);
		return;

	/* (r) */
	case 010:
		printf("(%s)", r);
		return;

	/* *(r)+ */
	case 030:
		printf("*");

	/* (r)+ */
	case 020:
		printf("(%s)+", r);
		return;

	/* *-(r) */
	case 050:
		printf("*");

	/* -(r) */
	case 040:
		printf("-(%s)", r);
		return;

	/* *x(r) */
	case 070:
		printf("*");

	/* x(r) */
	case 060:
		psymoff(cget(dot+dotinc, ISP), 010000);
		dotinc =+ 2;
		printf("(%s)", r);
		return;
	}
}
