#ifndef lint
static	char sccsid[] = "@(#)opset.c 4.2 10/27/82";
#endif lint
/*
 *	UNIX debugger
 *	Instruction printing routines.
 *	MACHINE DEPENDENT
 */

#ifdef ADB
#include "defs.h"
#endif ADB
#ifdef SDB
#include "head.h"
#endif SDB

L_INT		dot;
INT		dotinc;
L_INT		insoutvar[36];
#ifdef ADB
L_INT		var[36];
#endif ADB

#undef	INSTTAB
#include "instrs.h"

STRING	regname[];
STRING	fltimm[];
POS	type, space, incp;
/*
 *	Definitions for registers and for operand classes
 */
char	*insregname();	/* how to print a register */

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
 *	Definitions for large numbers
 */
#include "asnumber.h"
typedef	struct	as_number	*numberp;
numberp snarf();
numberp snarfreloc();
/*
 *	Definitions for special instructions
 */
#define	CASEB	0x8F
#define	CASEW	0xAF
#define	CASEL	0xCF
/*
 *	Definitions for converting TYP's into numbers, booleans, etc.
 *	These are shared with the assembler.
 */
extern	int	ty_NORELOC[];
extern	int	ty_float[];
extern	int	ty_nbyte[];
extern	int	ty_nlg[];
extern	char	*ty_string[];

short ioptab[3][256];	/* two level index by opcode into insttab */

int mapescbyte(byte)
	u_char	byte;
{
	switch(byte){
	default:	return(0);
	case ESCD:	return(1);
	case ESCF:	return(2);
	}
}

mkioptab()
{
	REG	struct insttab *p;
		int	mapchar;

	for(p = insttab; p->iname; p++){
		mapchar = mapescbyte(p->eopcode);
		if (ioptab[mapchar][p->popcode])
			continue;
		ioptab[mapchar][p->popcode] = p - insttab;
	}
}

u_char snarfuchar();
/*
 *	Global variables for communicating with the minions and printins
 */
static	int	idsp;
static	short	argno;		/* which argument one is working on */
static	char	insoutfmt[2];	/* how to format the relocated symbols */
#ifdef SDB
static	struct	proct	*procp;
#endif SDB

static savevar(val)
	long	val;
{
	var[argno] = val;
	insoutvar[argno] = val;
}

printins(fmt, Idsp, ins)
	char	fmt;
#ifndef vax
	u_char	ins;
#else
	u_char	ins;
#endif
	int	Idsp;
{
		u_char	mode;		/* mode */
		u_char	ins2;
		char	*indexreg;	/* print of which register indexes */
		char	*indexed;	/* we indexed */
		char	*operandout();
	REG	u_char 	*ap;
	REG	struct insttab *ip;
		u_char	optype;
		int	mapchar;

	idsp = Idsp;
	type = DSYM;
	space = idsp;
#ifdef SDB
	procp = adrtoprocp(dot);
	if (procp->paddr == dot){
		printf("0x%04.4x", ins);
		incp = 2;
		goto ret;
	}
#endif SDB

#ifdef ADB
	insoutfmt[0] = 0;
#endif ADB
#ifdef SDB
	insoutfmt[0] = fmt;
#endif SDB

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
	ip = &insttab[ioptab[mapchar][ins]];
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

#ifdef SDB
	oincr = incp;
#endif SDB
#ifdef ADB
	dotinc = incp;
#endif ADB
}

casebody(base, limit)
	long	base;
	long	limit;
{
	int	i;
	POS	baseincp;
	POS	advincp;
	struct	as_number	*valuep;
#define	OSIZE (sizeof(short))
	argno = 0;
	baseincp = incp;
	for (i = 0; i <= limit; i++) {
		printc(EOR);
#ifdef SDB
		printf("    %d:  ", i + base);
#endif SDB
#ifdef ADB
		printf("    %R:  ", i + base);
#endif ADB
		valuep = snarfreloc(OSIZE, 0);
		advincp = incp;
		incp = baseincp;
		dispaddress(valuep, OC_CONS(OC_WDISP, R_PC));
		incp = advincp;
	}
}

/*
 *	magic values to mung an offset to a register into
 *	something that psymoff can understand.. all magic
 */
			      /* 0	1	2	3	4 */
static long magic_masks[5] =	{0,	0x80,	0x8000,	0,	0};	
static long magic_compl[5] =	{0,	0x100,	0x10000,0,	0};
/*
 *	Snarf up some bytes, and put in the magic relocation flags
 */
numberp snarfreloc(nbytes)
	int	nbytes;
{
	numberp	back;
	back = snarf(nbytes);
	if (back->num_ulong[0] & magic_masks[nbytes])
		back->num_ulong[0] -= magic_compl[nbytes];
	return(back);
}
/*
 *	The following code is NOT portable from the PDP 11 to the VAX
 *	because of the byte ordering problem.
 */
numberp snarf(nbytes)
	int	nbytes;
{
	REG	int	i;

	static	struct	as_number	backnumber;
	static	struct	as_number	znumber;	/* init'ed to 0 */

	backnumber = znumber;
	for (i = 0; i < nbytes; i++)
		backnumber.num_uchar[i] = snarfuchar();
	return(&backnumber);
}
/*
 *	Read one single character, and advance the dot
 */
u_char snarfuchar()
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
 *	normal operand; return non zero pointer to register
 *	name if this is an index instruction.
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
#ifdef ADB
	psymoff(valuep->num_ulong[0], type, &insoutfmt[0]);
	if (regnumber != R_PC){		/* } */
#endif ADB
#ifdef SDB
	if(psymoff(valuep->num_ulong[0], regnumber, &insoutfmt[0])
	   && (regnumber != R_PC)){
#endif SDB
		printf("(%s)", insregname(regnumber));
	}
	savevar((long)valuep->num_ulong[0]);
}
/*
 *	get a register name
 */
char *insregname(regnumber)
	int	regnumber;
{
	char	*r;
	r = regname[regnumber];
#ifdef SDB
	if (   (insoutfmt[0] == 'i')
	    && (regnumber >= 6)
	    && (regnumber <= 11)
	    && (adrtoregvar(regnumber, procp) != -1)) {
		r = sl_name;
	}
#endif SDB
	return(r);
}
/*
 *	print out a short literal
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
#ifdef ADB
		printf("$%r", mode);
#endif ADB
#ifdef SDB
		printf("$%d", mode);
#endif SDB
		break;
	}
}

pcimmediate(mode, optype)
	u_char	mode;
	u_char	optype;
{
	int	nbytes;

	printc('$');
	if (mode == OC_DAIREG){	/* PC absolute, always 4 bytes*/
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
	REG	int	bindex;
	REG	int	nindex;
	REG	int	ch;

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
#ifdef SDB

L_INT inkdot(incr)
	int	incr;
{
	L_INT		newdot;

	newdot = dot + incr;
	return(newdot);
}

printc(c)
	char c;
{
	printf("%c", c);
}

psymoff(v, regnumber, fmt)
	L_INT	v;
	char	*fmt;
{
	struct	proct	*procp;
	REG	int diff;
	if (fmt[0] == 'i') {
		switch(regnumber){
		case 12:	/* parameter */
			if ((diff = adrtoparam((ADDR) v, adrtoprocp(dot)))
					!= -1) {
				printf("%s", sl_name);
				prdiff(diff);
				return(0);
			}
			break;
		case 13:	/* local */
			if ((diff = adrtolocal((ADDR) -v, adrtoprocp(dot))
					) != -1) {
				printf("%s", sl_name);
				prdiff(diff);
				return(0);
			}
			break;
		default:
			break;
		}
		if (v < firstdata) {
			if ((procp = adrtoprocp((ADDR) v)) != badproc) {
				prlnoff(procp, v);
				return(0);
			}
		} else {
			if ((diff = adrtoext((ADDR) v)) != -1) {
				printf("%s", sl_name);
				prdiff(diff);
				return(0);
			}
		}
	}
	prhex(v);
	return(1);
}

prdiff(diff)
{
	if (diff) {
		printf("+");
		prhex(diff);
	}
}

#endif SDB
