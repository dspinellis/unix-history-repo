#ifndef	lint
static char sccsid[] = "	dofloat.c	4.3	88/04/24	";
#endif

/* From Lou Salkind: compat/RCS/dofloat.c,v 1.2 84/01/31 13:33:53 */

/*
 * Partial PDP-11 floating-point simulator.  Always in double mode,
 * chop mode.  All arithmetic done in double-precision. Storing longs
 * into or taking longs from general registers doesn't work.
 * Overflow is never detected.
 */

#include <stdio.h>
#include "defs.h"

#define TRUE 1
#define FALSE 0

#define ABSD	0170600
#define ADDD	0172000
#define CFCC	0170000
#define CLRD	0170400
#define CMPD	0173400
#define DIVD	0174400
#define LDCFD	0177400
#define LDCLD	0177000
#define LDD	0172400
#define LDEXP	0176400
#define MODD	0171400
#define MULD	0171000
#define NEGD	0170700
#define SETD	0170011
#define SETI	0170002
#define SETL	0170012
#define STCDL	0175400
#define STCDF	0176000
#define STD	0174000
#define STEXP	0175000
#define SUBD	0173000
#define TSTD	0170500

static struct {
	unsigned fc :1;
	unsigned fv :1;
	unsigned fz :1;
	unsigned fn :1;
	unsigned fmm :1;
	unsigned ft :1;
	unsigned fl :1;
	unsigned fd :1;
} fps;

#define FZ fps.fz
#define FN fps.fn
#define FL fps.fl
#define FD fps.fd

#define LMODE FL
#define IMODE (!LMODE)

static double fregs[6];

dofloat(instr)
unsigned int instr;
{
	int mode, reg, ac;
	unsigned short * x, * resolve();
	long fliplong();
#define DOUBLE (*((double *)x))
#define FLOAT (*(float *)x)
#define LONG (*(long *)x)
#define SHORT (*(short *)x)
#define GETDOUBLE (x = resolve(mode, reg, 8, TRUE))
#define GETFLOAT (x = resolve(mode, reg, 4, TRUE))
#define GETLONG (x = resolve(mode, reg, 4, FALSE))
#define GETSHORT (x = resolve(mode, reg, 2, FALSE))
#define FREG fregs[ac]
	double temp;
	union {
		double d;
		short s;
	} bits;

	switch (instr & 0170000) {
	case 0170000:
		break;
	default:
		fprintf(stderr, "Unrecognized instr in dofloat %0o\n", instr);
		return (-1);
	}

	switch (instr & 07000) {
	case 0:
		switch (instr & 0700) {
		case 0:
			switch (instr) {
			case CFCC:
				psl &= ~017;
				if (FN) {
					psl |= 010;
				}
				if (FZ) {
					psl |= 04;
				}
				return (0);
			case SETD:
				FD = TRUE;
				return (0);
			case SETI:
				FL = FALSE;
				return (0);
			case SETL:
				FL = TRUE;
				return (0);
			default:
				fprintf(stderr, "Unrecognized instr in dofloat %0o\n", instr);
				return (-1);
			}
		default:
			break;
		}

		mode = (instr & 070) >> 3;
		reg = instr & 07;

		switch (instr & 0177700) {
		case ABSD:
			GETDOUBLE;
			if (DOUBLE < 0.0) {
				DOUBLE = -DOUBLE;
			}
			FZ = (DOUBLE == 0.0);
			FN = (DOUBLE < 0.0);
			return (0);
		case CLRD:
			GETDOUBLE;
			DOUBLE = 0.0;
			FZ = TRUE;
			FN = FALSE;
			return (0);
		case NEGD:
			GETDOUBLE;
			DOUBLE = -DOUBLE;
			FZ = (DOUBLE == 0.0);
			FN = (DOUBLE < 0.0);
			return (0);
		case TSTD:
			GETDOUBLE;
			FZ = (DOUBLE == 0.0);
			FN = (DOUBLE < 0.0);
			return (0);
		default:
			fprintf(stderr, "Unrecognized instr in dofloat %0o\n", instr);
			return (-1);
		}
	default:
		break;
	}

	ac = (instr & 0300) >> 6;
	mode = (instr & 070) >> 3;
	reg = instr & 07;

	switch (instr & 0177400) {
	case ADDD:
		GETDOUBLE;
		FREG += DOUBLE;
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	case CMPD:
		GETDOUBLE;
		FZ = (DOUBLE == FREG);
		FN = (DOUBLE < FREG);
		return (0);
	case DIVD:
		GETDOUBLE;
		FREG /= DOUBLE;
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	case LDCFD:
		GETFLOAT;
		FREG = FLOAT;
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	case LDCLD:
		if (IMODE) {
			GETSHORT;
			FREG = SHORT;
		} else {
			GETLONG;
			FREG = fliplong(LONG);
		}
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	case LDD:
		GETDOUBLE;
		FREG = DOUBLE;
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	case LDEXP:
		GETSHORT;
		bits.d = FREG;
		bits.s &= ~077600;
		bits.s |= (SHORT + 0200) << 7;
		FREG = bits.d;
		FZ = (SHORT == 0);
		FN = (FREG < 0.0);
		return (0);
	case MODD:
		GETDOUBLE;
		temp = FREG * DOUBLE;
		fregs[ac|1] = (long) temp;
		FREG = temp - (long) temp;
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	case MULD:
		GETDOUBLE;
		FREG = FREG * DOUBLE;
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	case STCDF:
		GETFLOAT;
		FLOAT = FREG;
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	case STCDL:
		if (IMODE) {
			GETSHORT;
			SHORT = FREG;
			psl &= ~017;
			if (SHORT == 0) {
				psl |= 04;
			}
			if (SHORT < 0) {
				psl |= 010;
			}
		} else {
			GETLONG;
			LONG = fliplong((long) FREG);
			psl &= ~017;
			if (fliplong(LONG) == 0) {
				psl |= 04;
			}
			if (fliplong(LONG) < 0) {
				psl |= 010;
			}
		}
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	case STD:
		GETDOUBLE;
		DOUBLE = FREG;
		return (0);
	case STEXP:
		GETSHORT;
		bits.d = FREG;
		SHORT = ((bits.s & 077600) >> 7) - 0200;
		FZ = (SHORT == 0);
		FN = (SHORT < 0);
		psl &= ~017;
		if (FZ) {
			psl |= 04;
		}
		if (FN) {
			psl |= 010;
		}
		return (0);
	case SUBD:
		GETDOUBLE;
		FREG -= DOUBLE;
		FZ = (FREG == 0.0);
		FN = (FREG < 0.0);
		return (0);
	default:
		fprintf(stderr, "Unrecognized instr in dofloat %0o\n", instr);
		return (-1);
	}
}

unsigned short *
resolve(mode, reg, bytes, floating)
{
	static unsigned short *x;
	static union {
		double d;
		unsigned short s;
	} bits;

	switch (mode) {
	case 0:
		if (floating) {
			if (bytes != 8) {
				fprintf(stderr, "Bad length in dofloat\n");
				return ((unsigned short *) -1);
			}
			x = (unsigned short *) &fregs[reg];
		} else {
			if (bytes != 2) {
				fprintf(stderr, "Bad length in dofloat\n");
				return ((unsigned short *) -1);
			}
			x = (unsigned short *) &regs[reg];
		}
		break;
	case 1:
		x = (unsigned short *) regs[reg];
		break;
	case 2:
		if (reg == 7 && floating) {
			bits.d = 0.0;
			bits.s = *(unsigned short *) regs[7];
			x = (unsigned short *) &bits;
			regs[7] += 2;
			pc = (unsigned short *) regs[7];
		} else {
			x = (unsigned short *) regs[reg];
			regs[reg] += bytes;
			if (reg == 7) {
				if (bytes != 2) {
					return((unsigned short *) -1);
				}
				pc = (unsigned short *) regs[7];
			}
		}
		break;
	case 3:
		x = (unsigned short *) regs[reg];
		x = (unsigned short *) *x;
		regs[reg] += 2;
		if (reg == 7) {
			pc = (unsigned short *) regs[7];
		}
		break;
	case 4:
		regs[reg] -= bytes;
		if (reg == 7) {
			pc = (unsigned short *) regs[7];
		}
		x = (unsigned short *) regs[reg];
		break;
	case 5:
		regs[reg] -= 2;
		if (reg == 7) {
			pc = (unsigned short *) regs[7];
		}
		x = (unsigned short *) regs[reg];
		x = (unsigned short *) *x;
		break;
	case 6:
		x = (unsigned short *) (unsigned short) (regs[reg] + *(pc++));
		if (reg == 7) {
			++x;
		}
		break;
	case 7:
		x = (unsigned short *) (unsigned short) (regs[reg] + *(pc++));
		if (reg == 7) {
			++x;
		}
		x = (unsigned short *) *x;
		break;
	}

	return (x);
}

long
fliplong(l)
long l;
{
	union {
		long l;
		short s[2];
	} bits[2];

	bits[0].l = l;
	bits[1].s[1] = bits[0].s[0];
	bits[1].s[0] = bits[0].s[1];
	return (bits[1].l);
}
