/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)opset.c	4.9 (Berkeley) %G%";
#endif /* not lint */

/*
 * adb - instruction printing routines: VAX version
 */

#include "defs.h"

/*
 * Get assembler definitions; declare tables that appear in optab.c.
 */
#define	ADB
#undef	INSTTAB
#include "instrs.h"

extern struct insttab insttab[];
extern char *regname[];
extern char *fltimm[];

/* these are shared with the assembler: */
extern int ty_NORELOC[];
extern int ty_nbyte[];
#ifdef notyet
extern int ty_float[];		/* must update assizetab.c */
#endif

/*
 * Definitions for registers and for operand classes.
 */
#define	R_PC		0xF

#define	OC_IMM0		0x0		/* literal, aka immediate */
#define	OC_IMM1		0x1
#define	OC_IMM2		0x2
#define	OC_IMM3		0x3
#define	OC_INDEX	0x4		/*   [rN]  */
#define	OC_REG		0x5		/*    rN   */
#define	OC_DREG		0x6		/*   (rN)  */
#define	OC_ADREG	0x7		/*  -(rN)  */
#define	OC_AIREG	0x8		/*   (rN)+ */
#define	OC_DAIREG	0x9		/*  *(rN)+ */
#define	OC_BDISP	0xA		/*  b(rN)  */
#define	OC_DBDISP	0xB		/* *b(rN)  */
#define	OC_WDISP	0xC		/*  w(rN)  */
#define	OC_DWDISP	0xD		/* *w(rN)  */
#define	OC_LDISP	0xE		/*  l(rN)  */
#define	OC_DLDISP	0xF		/* *l(rN)  */

#define	OC_SHIFT	4
#define	OC_CONS(oc,reg)	(((oc & 0xF) << OC_SHIFT) | (reg & 0xF))
#define	OC_AMEXT(x)	(((x) >> OC_SHIFT) & 0xF)
#define	OC_REGEXT(x)	((x) & 0xF)

/*
 * Definitions for special instructions.
 */
#define	CASEB	0x8F
#define	CASEW	0xAF
#define	CASEL	0xCF
#define	CHMK	0xBC

/*
 * ioptab is a two level 1-based index by opcode into insttab.
 * The first level into ioptab is given by mapescbyte().
 * Since ioptab is 1-based, references would be expected to
 * be of the form
 *
 *	ptr = &insttab[ioptab[a][b] - 1];
 *
 * but the form
 *
 *	ptr = &(insttab - 1)[ioptab[a][b]]
 *
 * is equivalent and generates less code (!) (time to work on the
 * compiler again...).
 */
static short ioptab[3][256];
#define	mapescbyte(b)	((b) == ESCD ? 1 : (b) == ESCF ? 2 : 0)

mkioptab()
{
	register struct insttab *p;
	register int mapchar;
	register short *iop;

	/*
	 * The idea here is that whenever two opcodes have the same
	 * codes, but different mnemonics, we want to prefer the one
	 * with the `simpler' type.  Here lower numbers make simpler
	 * types.  This seems (likely) to work reasonably well.
	 *
	 * At present, this affects the following opcodes:
	 *
	 *  7c	clrq   | clrd   | clrg
	 *  7e	movaq  | movad  | movag
	 *  7f	pushaq | pushad | pushag
	 *  d4	clrl   | clrf
	 *  de	moval  | movaf
	 *  df	pushal | pushaf
	 *
	 * In each case, the leftmost mnemonics are preferred.
	 */
#define PREFER(a, b) (A_TYPEXT((a)->argtype[0]) < A_TYPEXT((b)->argtype[0]))

	for (p = insttab; p->iname != NULL; p++) {
		mapchar = mapescbyte(p->eopcode);
		iop = &ioptab[mapchar][p->popcode];
		if (*iop == 0 || PREFER(p, &(insttab - 1)[*iop]))
			*iop = p - (insttab - 1);
	}
#undef PREFER
}

/*
 * Global variables for communication between the minions and printins.
 */
static int idsp;		/* which space we are in (INSTR or DATA) */
static int argno;		/* which argument we are working on */
static int dotoff;		/* offset from dot for this arg */
static int vset[7];		/* set by savevar, cleared by clrvar */

#define	savevar(v)	(vset[argno] = 1, var[argno] = v)
#define	clrvar(v)	(vset[argno] = 0, var[argno] = 0x80000000)

/*
 * Read some bytes, checking for errors, and updating the offset.
 */
#define	getsomebytes(ptr, nbytes) \
	(void) adbread(idsp, inkdot(dotoff), ptr, nbytes); \
	checkerr(); \
	dotoff += (nbytes)

/*
 * Read one byte, and advance the offset.
 */
static int
getbyte()
{
	u_char c;

	getsomebytes(&c, sizeof(c));
	return (c);
}

/*
 * adb's view: printins() prints one instruction, and sets dotinc.
 */
printins(space)
	int space;
{
	register u_char *ap;
	register struct insttab *ip;
	int ins, mode, optype, mapchar, t;
	char *lastix, *ixreg;
	char *operandout();

	/*
	 * Set up the module variables, pick up the instruction, and
	 * find its table entry.
	 */
	idsp = space;
	dotoff = 0;
	ins = idsp == SP_NONE ? (u_char)dot : getbyte();
	if ((mapchar = mapescbyte(ins)) != 0) {
		t = getbyte();
		if (ioptab[mapchar][t] == 0) {
			/*
			 * Oops; not a defined instruction; back over this
			 * escape byte. 
			 */
			dotoff--;
			mapchar = 0;
		} else
			ins = t;
	}
	if ((t = ioptab[mapchar][ins]) == 0) {
		adbprintf("<undefined operator byte>: %x", ins);
		dotinc = 1;
		return;
	}
	ip = &(insttab - 1)[t];
	adbprintf("%s%8t", ip->iname);

	/*
	 * For each argument, decode that argument.
	 * We set t if we notice something fishy.
	 */
	t = 0;
	for (ap = ip->argtype, argno = 0; argno < ip->nargs; argno++) {
		optype = *ap++;
		clrvar();
		if (argno != 0)
			printc(',');
		/*
		 * lastix and ixreg track the register indexed addressing
		 * mode, which is written as <stuff>[reg] but encoded as
		 * [reg]<stuff>.  Only one [reg] is legal.
		 */
		lastix = NULL;
		do {
			/* check for special pc-relative (branch) */
			if (A_ACCEXT(optype) & ACCB) {
				switch (A_TYPEXT(optype)) {
				case TYPB:
					mode = OC_CONS(OC_BDISP, R_PC);
					break;
				case TYPW:
					mode = OC_CONS(OC_WDISP, R_PC);
					break;
				}
			} else
				mode = getbyte();
			ixreg = operandout(mode, optype, ins == CHMK);
			if (lastix) {
				adbprintf("[%s]", lastix);
				if (ixreg)
					t = 1;
			}
		} while ((lastix = ixreg) != NULL);
	}
	if (t)
		adbprintf("%4t# not code? illegal arguments detected  ");
	switch (ins) {
	case CASEB:
	case CASEW:
	case CASEL:
		if (mapchar == 0 && vset[1] && vset[2])
			casebody(var[1], var[2]);
		else
			adbprintf("\n%4t# not code? non-constant cases  ");
	}
	dotinc = dotoff;
}

/*
 * Print out the locations to which each of the cases branch.
 * This routine carefully allows expressions such as
 *
 *	casel	<val>,$<const>,$0x7fffffff
 *
 * even though they do not fit on a VAX.
 */
static
casebody(base, limit)
	register expr_t base, limit;
{
	register expr_t i = -1;
	register addr_t a, baseaddr = inkdot(dotoff);
	short displ;

	argno = 0;
	do {
		i++;
		adbprintf("\n    %R:  ", base++);
		getsomebytes(&displ, sizeof(displ));
		a = displ + baseaddr;
		psymoff("%R", a, SP_DATA, maxoff, "");
		savevar(a);
	} while (i != limit);
}

/*
 * Handle a normal operand.  Return pointer to register
 * name if this is an index instruction, else return NULL.
 */
static char *
operandout(mode, optype, ischmk)
	register int mode;
	int optype, ischmk;
{
	register char *r;
	register int regnumber, nbytes, n;
	union {
		char b;
		short w;
		int l;
	} displ;
	extern char *syscalls[];
	extern int nsys;

	regnumber = OC_REGEXT(mode);
	r = regname[regnumber];
	switch (OC_AMEXT(mode)) {

	case OC_IMM0: case OC_IMM1:
	case OC_IMM2: case OC_IMM3:
		savevar(mode);
		printc('$');
#ifdef notyet
		if (ty_float[A_TYPEXT(optype)])
			prints(fltimm[mode]);
		else if (ischmk && (u_int)mode < nsys && syscalls[mode])
			prints(syscalls[mode]);
		else
			adbprintf("%V", mode);
#else
		switch (A_TYPEXT(optype)) {

		case TYPF:
		case TYPD:
		case TYPG:
		case TYPH:
			prints(fltimm[mode]);
			break;

		default:
			if (ischmk && (u_int)mode < nsys && syscalls[mode])
				prints(syscalls[mode]);
			else
				adbprintf("%V", mode);
			break;
		}
#endif
		return (0);

	case OC_INDEX:
		return (r);	/* will be printed later */

	case OC_REG:
		adbprintf("%s", r);
		return (0);

	case OC_DREG:
		adbprintf("(%s)", r);
		return (0);

	case OC_ADREG:
		adbprintf("-(%s)", r);
		return (0);

	case OC_DAIREG:
		printc('*');
		/* FALLTHROUGH */

	case OC_AIREG:
		if (regnumber != R_PC) {
			adbprintf("(%s)+", r);
			return (0);
		}
		/* PC immediate */
		printc('$');
		if (mode == OC_CONS(OC_DAIREG, R_PC))
			/* PC absolute, always 4 bytes */
			nbytes = 4;
		else {
			nbytes = ty_nbyte[A_TYPEXT(optype)];
			if (ty_NORELOC[A_TYPEXT(optype)]) {
				bignumprint(nbytes, optype);
				return (0);
			}
		}
		break;

	case OC_DBDISP:
		printc('*');
		/* FALLTHROUGH */

	case OC_BDISP:
		nbytes = 1;
		break;

	case OC_DWDISP:
		printc('*');
		/* FALLTHROUGH */

	case OC_WDISP:
		nbytes = 2;
		break;

	case OC_DLDISP:
		printc('*');
		/* FALLTHROUGH */

	case OC_LDISP:
		nbytes = 4;
		break;

	default:
		panic("operandout 1");
		/* NOTREACHED */
	}

	/*
	 * Print a displacement format.
	 */
	getsomebytes(&displ, nbytes);
	switch (nbytes) {
	case 1:
		n = displ.b;
		break;
	case 2:
		n = displ.w;
		break;
	case 4:
		n = displ.l;
		break;
	default:
		panic("operandout 2");
		/* NOTREACHED */
	}
	if (regnumber == R_PC) {
		switch (OC_AMEXT(mode)) {

		case OC_DAIREG:
			if (ischmk && (u_int)n < nsys && syscalls[n]) {
				prints(syscalls[n]);
				return (0);
			}
			break;

		case OC_BDISP: case OC_DBDISP:
		case OC_WDISP: case OC_DWDISP:
		case OC_LDISP: case OC_DLDISP:
			/* PC offset */
			n += dot + dotoff;
		}
		psymoff("%V", (addr_t)n, SP_DATA, maxoff, "");
	} else
		adbprintf("%V(%s)", (expr_t)n, regname[regnumber]);
	savevar(n);
	return (0);
}

/*
 * Print an F-float, D-float, G-float, H-float, quadword, or octaword.
 * F- and D-floating values are printed as themselves, unless they are
 * reserved operand bit patterns; these, and the others, are printed
 * instead in hex, with leading zeroes suppressed.
 */
static
bignumprint(nbytes, optype)
	int nbytes, optype;
{
	register char *p;
	register int i;
	union {
		float	f;	/* if f-floating */
		double	d;	/* if d-floating */
		u_char	c[16];	/* if G, H, Q, or O */
	} n;
	char expbuf[4*8+1];	/* max 4 8-character hex ints */
	static char tohex[] = "0123456789abcdef";

	/*
	 * Read in the number, then figure out how to print it.
	 */
	getsomebytes(&n, nbytes);
	switch (A_TYPEXT(optype)) {

	case TYPF:
		if ((p = checkfloat((caddr_t)&n.f, 0)) == NULL) {
			adbprintf("0f%f", n.f);
			return;
		}
		adbprintf("%s 0f::", p);
		break;

	case TYPD:
		if ((p = checkfloat((caddr_t)&n.d, 1)) == NULL) {
			adbprintf("0d%f", n.d);
			return;
		}
		adbprintf("%s 0d::", p);
		break;

	case TYPG:
		adbprintf("0g::");
		break;

	case TYPH:
		adbprintf("0h::");
		break;

	case TYPQ:
	case TYPO:
		break;

	default:
		panic("bignumprint");
	}

	/*
	 * Expand the number into expbuf, then skip leading zeroes.
	 * Be careful not to skip the entire number.
	 */
	for (p = expbuf, i = nbytes; --i >= 0;) {
		*p++ = tohex[n.c[i] >> 4];
		*p++ = tohex[n.c[i] & 15];
	}
	for (p = expbuf; *p == '0'; p++)
		/* void */;
	prints(*p ? p : p - 1);
}
