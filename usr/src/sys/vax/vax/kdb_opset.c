/*
 *	@(#)kdb_opset.c	7.2 (Berkeley) %G%
 */

#include "../kdb/defs.h"

/*
 * Instruction printing.
 */
REGLIST reglist [] = {
	"p1lr",	&pcb.pcb_p1lr,	"p1br",	(int *)&pcb.pcb_p1br,
	"p0lr",	&pcb.pcb_p0lr,	"p0br",	(int *)&pcb.pcb_p0br,
	"ksp",	&pcb.pcb_ksp,	"esp",	&pcb.pcb_esp,
	"ssp",	&pcb.pcb_ssp,	"psl",	&pcb.pcb_psl,
	"pc",	&pcb.pcb_pc,	"usp",	&pcb.pcb_usp,
	"fp",	&pcb.pcb_fp,	"ap",	&pcb.pcb_ap,
	"r11",	&pcb.pcb_r11,	"r10",	&pcb.pcb_r10,
	"r9",	&pcb.pcb_r9,	"r8",	&pcb.pcb_r8,
	"r7",	&pcb.pcb_r7,	"r6",	&pcb.pcb_r6,
	"r5",	&pcb.pcb_r5,	"r4",	&pcb.pcb_r4,
	"r3",	&pcb.pcb_r3,	"r2",	&pcb.pcb_r2,
	"r1",	&pcb.pcb_r1,	"r0",	&pcb.pcb_r0,
};

/*
 * Argument data types
 *
 * If you change these definitions, you must also change the tables
 * in assizetab.c
 */
#define	TYPB		000	/* byte integer */
#define	TYPW		001	/* word integer */
#define	TYPL		002	/* long integer */
#define	TYPQ		003	/* quad integer */
#define	TYPO		004	/* octa integer */
#define	TYPF		005	/* F float */
#define	TYPD		006	/* D float */
#define	TYPG		007	/* G float */
#define	TYPH		010	/* H float */
#define	TYPUNPACKED	011	/* when unpacked into mantissa & exponent */
#define	TYPNONE		012	/* when nothing */
#define	TYPLG		4	/* number of bits the above take up */

#define	TYPMASK	((1<<TYPLG)-1)	/* the mask (assumes 2's comp arith) */
/*
 * Constructors and extractors for argument access kinds and types
 */
#define A_CONS(access, type)	((access) | (type))
#define	A_ACCEXT(consed)	((consed) & (TYPMASK << TYPLG))
#define	A_TYPEXT(consed)	((consed) & TYPMASK)

/*
 * Argument access types used to test validity of operands to operators
 */
#define	ACCR	(1<<TYPLG)			/* read */
#define	ACCW	(2<<TYPLG)			/* write */
#define	ACCB	(4<<TYPLG)			/* branch displacement */
#define	ACCA	(8<<TYPLG)			/* address only */
#define	ACCV	(8<<TYPLG)			/* address only */
#define	ACCM	(ACCR | ACCW)			/* modify */
#define	ACCI	(ACCB | ACCR)			/* XFC code */

#define ACCESSMASK	(ACCA | ACCR | ACCW | ACCB)	/* the mask */

/*
 * Construction of TYPX and ACCX, to make the instrs table
 * easy to use and read.
 */
/*
 * For real memory address
 */
#define	A_AB	A_CONS(ACCA, TYPB)
#define	A_AW	A_CONS(ACCA, TYPW)
#define	A_AL	A_CONS(ACCA, TYPL)
#define	A_AQ	A_CONS(ACCA, TYPQ)
#define	A_AO	A_CONS(ACCA, TYPO)
#define	A_AF	A_CONS(ACCA, TYPF)
#define	A_AD	A_CONS(ACCA, TYPD)
#define	A_AG	A_CONS(ACCA, TYPG)
#define	A_AH	A_CONS(ACCA, TYPH)
/*
 * For real memory addresses, or register addresses [sic]
 *
 * CHEAT! we just call these read access, since
 * registers are allowed. All field instruction, except insv,
 * are are read access fields.
 */
#define	A_VB	A_CONS(ACCR, TYPB)
#define	A_VW	A_CONS(ACCR, TYPW)
#define	A_VL	A_CONS(ACCR, TYPL)
#define	A_VQ	A_CONS(ACCR, TYPQ)
#define	A_VO	A_CONS(ACCR, TYPO)
#define	A_VF	A_CONS(ACCR, TYPF)
#define	A_VD	A_CONS(ACCR, TYPD)
#define	A_VG	A_CONS(ACCR, TYPG)
#define	A_VH	A_CONS(ACCR, TYPH)
/*
 * For branch displacement
 */
#define	A_BB	A_CONS(ACCB, TYPB)
#define	A_BW	A_CONS(ACCB, TYPW)
/*
 * For modification
 */
#define	A_MB	A_CONS(ACCM, TYPB)
#define	A_MW	A_CONS(ACCM, TYPW)
#define	A_ML	A_CONS(ACCM, TYPL)
#define	A_MF	A_CONS(ACCM, TYPF)
#define	A_MD	A_CONS(ACCM, TYPD)
#define	A_MG	A_CONS(ACCM, TYPG)
#define	A_MH	A_CONS(ACCM, TYPH)
/*
 * For reading
 */
#define	A_RB	A_CONS(ACCR, TYPB)
#define	A_RW	A_CONS(ACCR, TYPW)
#define	A_RL	A_CONS(ACCR, TYPL)
#define	A_RQ	A_CONS(ACCR, TYPQ)
#define	A_RO	A_CONS(ACCR, TYPO)
#define	A_RF	A_CONS(ACCR, TYPF)
#define	A_RD	A_CONS(ACCR, TYPD)
#define	A_RG	A_CONS(ACCR, TYPG)
#define	A_RH	A_CONS(ACCR, TYPH)
/*
 * For writing
 */
#define	A_WB	A_CONS(ACCW, TYPB)
#define	A_WW	A_CONS(ACCW, TYPW)
#define	A_WL	A_CONS(ACCW, TYPL)
#define	A_WQ	A_CONS(ACCW, TYPQ)
#define	A_WO	A_CONS(ACCW, TYPO)
#define	A_WF	A_CONS(ACCW, TYPF)
#define	A_WD	A_CONS(ACCW, TYPD)
#define	A_WG	A_CONS(ACCW, TYPG)
#define	A_WH	A_CONS(ACCW, TYPH)

struct insttab {
	char	*iname;
	u_char	eopcode;
	u_char	popcode;
	char	nargs;
	u_char	argtype[6];
};

#define OP(name,eopcode,popdcode,nargs,a1,a2,a3,a4,a5,a6) \
	{name,eopcode,popdcode,nargs,a1,a2,a3,a4,a5,a6}
/*
 * Definitions for the escape bytes
 */
#define	CORE	0
#define	NEW	1
#define	ESCD	0xfd
#define	ESCF	0xff
#define	mapescbyte(b)	((b) == ESCD ? 1 : (b) == ESCF ? 2 : 0)

static	struct insttab insttab[] = {
#include "../vax/kdb_instrs"
0};

/*
 * Convert TYP[BWLQOFDGH] into {1 if relocation not OK}
 */
int	ty_NORELOC[] = {
	0,	/* TYPB */
	0,	/* TYPW */
	0,	/* TYPL */
	1,	/* TYPQ */
	1,	/* TYPO */
	1,	/* TYPF */
	1,	/* TYPD */
	1,	/* TYPG */
	1,	/* TYPH */
	1	/* TYPNONE */
};

/*
 * Convert TYP[BWLQOFDGH] into {1 ... 16}
 */
int	ty_nbyte[] = {
	1,	/* TYPB */
	2,	/* TYPW */
	4,	/* TYPL */
	8,	/* TYPQ */
	16,	/* TYPO */
	4,	/* TYPF */
	8,	/* TYPD */
	8,	/* TYPG */
	16,	/* TYPH */
	0	/* TYPNONE */
};

static	char *regname[] = {
	"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
	"r8", "r9", "r10","r11","ap", "fp", "sp", "pc"
};
static	char *fltimm[] = {
"0.5", "0.5625", "0.625", "0.6875", "0.75", "0.8125", "0.875", "0.9375",
"1.0", "1.125", "1.25", "1.375", "1.5", "1.625", "1.75", "1.875",
"2.0", "2.25", "2.5", "2.75", "3.0", "3.25", "3.5", "3.75",
"4.0", "4.5", "5.0", "5.5", "6.0", "6.5", "7.0", "7.5",
"8.0", "9.0", "10.0", "11.0", "12.0", "13.0", "14.0", "15.0",
"16.0", "18.0", "20.0", "22.0", "24.0", "26.0", "28.0", "30.0",
"32.0", "36.0", "40.0", "44.0", "48.0", "52.0", "56.0", "60.0",
"64.0", "72.0", "80.0", "88.0", "96.0", "104.0", "112.0", "120.0"
};

static	int type, space, incp;
static	long insoutvar[36];
/*
 * Definitions for registers and for operand classes
 */
static	char *insregname();	/* how to print a register */

#define	R_PC		0xF

#define	OC_IMM0		0x0
#define	OC_IMM1		0x1
#define	OC_IMM2		0x2
#define	OC_IMM3		0x3
#define	OC_INDEX	0x4
#define	OC_REG		0x5
#define	OC_DREG		0x6
#define	OC_ADREG	0x7
#define	OC_AIREG	0x8
#define	OC_DAIREG	0x9

#define	OC_BDISP	0xA
#define	OC_DBDISP	0xB
#define	OC_WDISP	0xC
#define	OC_DWDISP	0xD
#define	OC_LDISP	0xE
#define	OC_DLDISP	0xF

#define	OC_SHIFT	4
#define	OC_CONS(oc,reg)	(((oc & 0xF) << OC_SHIFT) | (reg & 0xF))
#define	OC_AMEXT(x)	(((x) >> OC_SHIFT) & 0xF)
#define	OC_REGEXT(x)	((x) & 0xF)

/*
 * Definitions for large numbers
 */
#include "asnumber.h"
typedef	struct	as_number	*numberp;
static	numberp snarf();
static	numberp snarfreloc();
/*
 * Definitions for special instructions
 */
#define	CASEB	0x8F
#define	CASEW	0xAF
#define	CASEL	0xCF

/* two level 1-based index by opcode into insttab */
static	short ioptab[3][256];

kdbsetup()
{
	register struct insttab *p;
		int	mapchar;

	for(p = insttab; p->iname; p++){
		mapchar = mapescbyte(p->eopcode);
		if (ioptab[mapchar][p->popcode])
			continue;
		ioptab[mapchar][p->popcode] = (p - insttab) + 1;
	}
}

static	u_char snarfuchar();
/*
 * Global variables for communicating with the minions and printins
 */
static	int	idsp;
static	short	argno;		/* which argument one is working on */
static	char	insoutfmt[2];	/* how to format the relocated symbols */

static savevar(val)
	long	val;
{
	var[argno] = val;
	insoutvar[argno] = val;
}

printins(fmt, Idsp, ins)
	char	fmt;
	u_char	ins;
	int	Idsp;
{
		u_char	mode;		/* mode */
		u_char	ins2;
		char	*indexreg;	/* print of which register indexes */
		char	*indexed;	/* we indexed */
		char	*operandout();
	register u_char 	*ap;
	register struct insttab *ip;
		u_char	optype;
		int	mapchar;

	idsp = Idsp;
	type = DSYM;
	space = idsp;
	insoutfmt[0] = 0;

	incp = 1;
	if ((mapchar = mapescbyte(ins)) != 0){
		ins2 = snarfuchar();
		if (ioptab[mapchar][ins2] == 0){
			/*
			 *	Oops; not a defined instruction;
			 *	back over this escape byte.
			 */
			incp -= 1;
			mapchar = 0;
		} else {
			ins = ins2;
		}
	}
	if (ioptab[mapchar][ins] == 0){
		printf("<undefined operator byte>: %x", ins);
		goto ret;
	}
	ip = &insttab[ioptab[mapchar][ins] - 1];
	printf("%s\t", ip->iname);

	for (ap = ip->argtype, argno = 0; argno < ip->nargs; argno++, ap++) {
		savevar(0x80000000);	/* an illegal symbol */
		optype = *ap;
		if (argno != 0)
			printc(',');
		indexreg = 0;
		indexed = 0;
		do{
			if (A_ACCEXT(optype) & ACCB){
				switch(A_TYPEXT(optype)){
				case TYPB:
					mode = OC_CONS(OC_BDISP, R_PC);
					break;
				case TYPW:
					mode = OC_CONS(OC_WDISP, R_PC);
					break;
				}
			} else {
				mode = snarfuchar();
			}
			indexreg = operandout(mode, optype);
			if (indexed)
				printf("[%s]", indexed);
			indexed = indexreg;
		} while(indexed);
	}
	if (mapchar == 0){
		switch(ins){
		case CASEB:
		case CASEW:
		case CASEL:
			casebody(insoutvar[1], insoutvar[2]);
			break;
		default:
			break;
		}
	}
   ret: ;

	dotinc = incp;
}

casebody(base, limit)
	long	base;
	long	limit;
{
	int	i;
	u_int	baseincp;
	u_int	advincp;
	struct	as_number	*valuep;
#define	OSIZE (sizeof(short))
	argno = 0;
	baseincp = incp;
	for (i = 0; i <= limit; i++) {
		printc(EOR);
		printf("    %R:  ", i + base);
		valuep = snarfreloc(OSIZE, 0);
		advincp = incp;
		incp = baseincp;
		dispaddress(valuep, OC_CONS(OC_WDISP, R_PC));
		incp = advincp;
	}
}

/*
 * magic values to mung an offset to a register into
 * something that psymoff can understand.. all magic
 */
			      /* 0	1	2	3	4 */
static long magic_masks[5] =	{0,	0x80,	0x8000,	0,	0};	
static long magic_compl[5] =	{0,	0x100,	0x10000,0,	0};
/*
 * Snarf up some bytes, and put in the magic relocation flags
 */
static numberp snarfreloc(nbytes)
	int	nbytes;
{
	numberp	back;
	back = snarf(nbytes);
	if (back->num_ulong[0] & magic_masks[nbytes])
		back->num_ulong[0] -= magic_compl[nbytes];
	return(back);
}
/*
 * The following code is NOT portable from the PDP 11 to the VAX
 * because of the byte ordering problem.
 */
static numberp snarf(nbytes)
	int	nbytes;
{
	register	int	i;

	static	struct	as_number	backnumber;
	static	struct	as_number	znumber;	/* init'ed to 0 */

	backnumber = znumber;
	for (i = 0; i < nbytes; i++)
		backnumber.num_uchar[i] = snarfuchar();
	return(&backnumber);
}

/*
 * Read one single character, and advance the dot
 */
static u_char
snarfuchar()
{
	u_char	back;
	/*
	 *	assert: bchkget and inkdot don't have side effects
	 */
	back = (u_char)bchkget(inkdot(incp), idsp);
	incp += 1;
	return(back);
}

/*
 * normal operand; return non zero pointer to register
 * name if this is an index instruction.
 */
char *operandout(mode, optype)
	u_char	mode;
	u_char	optype;
{
	char	*r;
	int	regnumber;
	int	nbytes;

	regnumber = OC_REGEXT(mode);
	r = insregname(regnumber);
	switch (OC_AMEXT(mode)){
	case OC_IMM0:
	case OC_IMM1:
	case OC_IMM2:
	case OC_IMM3:
		shortliteral(mode, optype);
		return(0);
	case OC_INDEX:
		return(r);		/* will be printed later */
	case OC_REG:
		printf("%s", r);
		return(0);
	case OC_DREG:
		printf("(%s)", r);
		return(0);
	case OC_ADREG:
		printf("-(%s)", r);
		return(0);
	case OC_DAIREG:
		printc('*');
	case OC_AIREG:
		if (regnumber == R_PC){
			pcimmediate(mode, optype);
		} else {
			printf("(%s)+", r);
		}
		return(0);
	case OC_DBDISP:
		printc('*');
	case OC_BDISP:
		nbytes = 1;
		break;
	case OC_DWDISP:
		printc('*');
	case OC_WDISP:
		nbytes = 2;
		break;
	case OC_DLDISP:
		printc('*');
	case OC_LDISP:
		nbytes = 4;
		break;
	}
	dispaddress(snarfreloc(nbytes), mode);
	return(0);
}

dispaddress(valuep, mode)
	numberp	valuep;
	u_char	mode;
{
	int	regnumber = OC_REGEXT(mode);

	switch(OC_AMEXT(mode)){
	case OC_BDISP:
	case OC_DBDISP:
	case OC_WDISP:
	case OC_DWDISP:
	case OC_LDISP:
	case OC_DLDISP:
		if (regnumber == R_PC){
			/* PC offset addressing */
			valuep->num_ulong[0] += inkdot(incp);
		}
	}
	if (regnumber == R_PC)
		psymoff(valuep->num_ulong[0], type, &insoutfmt[0]);
	else {				/* } */
		printf(LPRMODE, valuep->num_ulong[0]);
		printf(insoutfmt);
		printf("(%s)", insregname(regnumber));
	}
	savevar((long)valuep->num_ulong[0]);
}

/*
 * get a register name
 */
static char *
insregname(regnumber)
	int	regnumber;
{
	char	*r;
	r = regname[regnumber];
	return(r);
}

/*
 * print out a short literal
 */
shortliteral(mode, optype)
	u_char	mode;
	u_char	optype;
{
	savevar((long)mode);
	switch(A_TYPEXT(optype)){
	case TYPF:
	case TYPD:
	case TYPG:
	case TYPH:
		printf("$%s", fltimm[mode]);
		break;
	default:
		printf("$%r", mode);
		break;
	}
}

pcimmediate(mode, optype)
	u_char	mode;
	u_char	optype;
{
	int	nbytes;

	printc('$');
	if (mode == OC_CONS(OC_DAIREG, R_PC)){	/* PC absolute, always 4 bytes*/
		dispaddress(snarfreloc(4), mode);
		return;
	}
	nbytes = ty_nbyte[A_TYPEXT(optype)];
	if (! ty_NORELOC[A_TYPEXT(optype)]){
		dispaddress(snarfreloc(nbytes), mode);
		return;
	}
	bignumprint(nbytes, optype);
}

bignumprint(nbytes, optype)
	int	nbytes;
	u_char	optype;
{
	numberp	valuep;
	int	leading_zero = 1;
	register int	bindex;
	register int	nindex;
	register int	ch;

	valuep = snarf(nbytes);
	switch(A_TYPEXT(optype)){
	case TYPF:	
		printf("0f%f", valuep->num_num.numFf_float.Ff_value);
		break;
	case TYPD:
		printf("0d%f", valuep->num_num.numFd_float.Fd_value);
		break;
	case TYPG:
		printf("0g::"); goto qprint;
	case TYPH:
		printf("0h::"); goto qprint;
	case TYPQ:
	case TYPO:
	qprint:
		for (bindex = nbytes - 1; bindex >= 0; --bindex){
			for (nindex = 4; nindex >= 0; nindex -= 4){
				ch = (valuep->num_uchar[bindex] >> nindex);
				ch &= 0x0F;
				if ( ! (leading_zero &= (ch == 0) ) ){
					if (ch <= 0x09)
						printc(ch + '0');
					else
						printc(ch - 0x0A + 'a');
				}
			}
		}
		break;
	}
}
