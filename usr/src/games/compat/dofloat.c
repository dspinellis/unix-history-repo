#
static char sccsid[] = "	dofloat.c	4.1	82/05/12	";
/*
 *	Simulate pdp11 floating point for compatability mode programs.
 *	Quick and dirty with no big effort at speed since it takes so
 *	much overhead to get here in the first place.
 *	I make no claims on the completeness of this simulation.
 *	Art Wetzel 3/16/80
 */
#ifndef NOFPSIM
#ifdef DEBUG
#include <stdio.h>
#endif
#include "defs.h"
/* output codes */
#define	NONE	0
#define	SHORT	01
#define	LONG	02
#define	FLOAT	04
#define	DOUBLE	010
#define	OUTPUT	020
/* parts of fps */
#define	FD	0200
#define	FL	0100
#define	FN	010
#define	FZ	04
#define	FV	02
#define	FC	01
/* fis instructions */
#define	FADD	075000
#define	FSUB	075010
#define	FMUL	075020
#define	FDIV	075030
/* fpu instructions */
#define	ABSD	0170600
#define	ABSF	0170600
#define	ADDD	0172000
#define	ADDF	0172000
#define	CFCC	0170000
#define	CLRD	0170400
#define	CLRF	0170400
#define	CMPD	0173400
#define	CMPF	0173400
#define	DIVD	0174400
#define	DIVF	0174400
#define	LDCFD	0177400
#define	LDCFF	0177400
#define	LDCLD	0177000
#define	LDCLF	0177000
#define	LDCIF	0177000
#define	LDCID	0177000
#define	LDEXP	0176400
#define	LDD	0172400
#define	LDF	0172400
#define	LDFPS	0170100
#define	MODD	0171400
#define	MODF	0171400
#define	MULD	0171000
#define	MULF	0171000
#define	NEGD	0170700
#define	NEGF	0170700
#define	SETF	0170001
#define	SETD	0170011
#define	SETI	0170002
#define	SETL	0170012
#define	STCDF	0176000
#define	STCFD	0176000
#define	STCDL	0175400
#define	STCDI	0175400
#define	STCFL	0175400
#define	STCFI	0175400
#define	STEXP	0175000
#define	STD	0174000
#define	STF	0174000
#define	STFPS	0170200
#define	STST	0170300
#define	SUBD	0173000
#define	SUBF	0173000
#define	TSTD	0170500
#define	TSTF	0170500
union	alltypes	{
	double	d;
	float	f;
	long	l;
	short	s;
	unsigned short p[4];
};
/* static storage for floating registers */
static union	alltypes	fregs[6];
static union	alltypes	srcdst;
int	fps = FD|FL;
int	dbl = 0;
int	lng = 0;
#endif
dofloat(instr) unsigned int instr; {
#ifdef NOFPSIM
	return(-1);
#else
	register unsigned short *wptr;
	register unsigned int opcode, ac, mode, fac, adjust, output, ccset;
	unsigned short *locate();
	/* indicate what condition codes will be changed by op - assume none */
	ccset = 0;
	/* type of memory output - assume none */
	output = NONE;
	/* default adjust to type */
	if(dbl)
		adjust = DOUBLE;
	else
		adjust = FLOAT;
	/* chop up instruction to get relevent parts */
	opcode = instr & 0177700;
	fac = (instr>>6) & 03;
	mode = (instr>>3) & 07;
	ac = instr & 07;
	/* if the instruction uses a src/dst construct ptr and fetch */
	switch(opcode) {
	case	FADD:
	case	CFCC:
		break;
	default:
		wptr = locate(mode, ac);
		/* special case for mode 0 */
		if(mode == 0) switch(opcode & 0177400) {
		/* special instructions to use cpu regs */
		case	LDEXP:
		case	STEXP:
			wptr = &regs[ac];
			break;
		case	STCDL:
			wptr = &regs[ac];
		default:
			break;
		}
		if(dbl)
			srcdst.d = *(double *)wptr;
		else
			srcdst.f = *(float *)wptr;
		/* immediate fetches are 16 bits */
		if(ac == 7 && (mode == 2)) {
			srcdst.p[1] = 0;
			srcdst.p[2] = 0;
			srcdst.p[3] = 0;
		}
		break;
	}
#ifdef	DEBUG
fprintf(stderr,"pc %o sp %o instr %o srcdst %o mode %o reg %o fac %o\n", pc-1,regs[6],instr,srcdst.s,mode,ac,fac);
#endif
	switch(opcode) {
	case	FADD:
		/* catches all fis instructions */
		/* last 3 bits are stack pointer register */
		ac = instr & 07;
		/* get pointer to stack words */
		wptr = (unsigned short *)regs[ac];
		/* getch floating value from stack */
		srcdst.f = *(float *)wptr;
		/* shorten stack */
		wptr += 2;
		/* do appropriate operation */
		switch(instr & 0177770) {
		case	FADD:
			srcdst.f += *(float *)wptr;
			break;
		case	FSUB:
			srcdst.f = *(float *)wptr - srcdst.f;
			break;
		case	FMUL:
			srcdst.f *= *(float *)wptr;
			break;
		case	FDIV:
			srcdst.f = *(float *)wptr / srcdst.f;
			break;
		default:
			return(-1);
		}
		/* copy out result */
		*(float *)wptr = srcdst.f;
		/* set up condition codes */
		psl &= ~017;
		if(srcdst.f == 0.) psl |= FZ;
		if(srcdst.f < 0.) psl |= FN;
		/* adjust register to reflect stack change */
		regs[ac] = (unsigned short)(int)wptr;
		return(0);
	case	CFCC:
		switch(instr) {
		case	SETF:
			dbl = 0;
			break;
		case	SETD:
			dbl = 1;
			break;
		case	SETI:
			lng = 0;
			break;
		case	SETL:
			lng = 1;
			break;
		case	CFCC:
			psl &= ~017;
			psl |= (fps & 017);
#ifdef DEBUG
fprintf(stderr,"CFCC %o\n",psl);
#endif
			break;
		default:
			return(-1);
		}
		return(0);
	case	ABSD:
		if(srcdst.d < 0.0 ) srcdst.d = -srcdst.d;
		ccset = FZ;
		if(dbl) 
			output = DOUBLE;
		else
			output = FLOAT;
		break;
	case	CLRD:
		srcdst.d =0.0;
		ccset = FZ;
		if(dbl)
			output = DOUBLE;
		else
			output = FLOAT;
		break;
	case	LDFPS:
		adjust = SHORT;
		fps = srcdst.s;
		if(fps & FD)
			dbl = 1;
		else
			dbl = 0;
		if(fps & FL )
			lng = 1;
		else
			lng = 0;
		break;
	case	NEGD:
		srcdst.d = -srcdst.d;
		ccset = FZ|FN;
		if(dbl)
			output = DOUBLE;
		else
			output = FLOAT;
		break;
	case	STFPS:
		srcdst.s = fps;
		adjust = output = SHORT;
		break;
	case	STST:
		return(0);
		break;
	case	TSTD:
		ccset = FZ|FN;
		break;
	default:
		opcode = instr & 0177400;
		switch(opcode) {
		case	STD:
			srcdst.d = fregs[fac].d;
#ifdef DEBUG
fprintf(stderr,"STD %o\n",srcdst.s);
#endif
			if(dbl)
				output = DOUBLE;
			else
				output = FLOAT;
			break;
		case	LDD:
#ifdef DEBUG
fprintf(stderr,"LDD %o\n",srcdst.s);
#endif
			fregs[fac].d = srcdst.d;
			ccset = FZ|FN;
			break;
		case	ADDD:
			fregs[fac].d += srcdst.d;
			ccset = FZ|FN;
			break;
		case	SUBD:
			fregs[fac].d -= srcdst.d;
			ccset = FZ|FN;
			break;
		case	MULD:
			fregs[fac].d *= srcdst.d;
			ccset = FZ|FN;
			break;
		case	DIVD:
#ifdef DEBUG
fprintf(stderr,"DIVD %f by %f gives ",fregs[fac].d,srcdst.d);
#endif
			fregs[fac].d /= srcdst.d;
#ifdef DEBUG
fprintf(stderr,"-> %f\n",fregs[fac].d);
#endif
			ccset = FZ|FN;
			break;
		case	STCDF:
			adjust = output = FLOAT;
			ccset = FZ|FN;
			break;
		case	LDCFD:
			adjust = FLOAT;
			ccset = FZ|FN;
			break;
		case	LDCLD:
			if(lng) {
				adjust = LONG;
				srcdst.d = srcdst.l;
			} else {
				adjust = SHORT;
				srcdst.d = srcdst.s;
			}
			ccset = FZ|FN;
			break;
		case	CMPD:
			srcdst.d -= fregs[fac].d;
			ccset = FZ|FN;
			break;
		case	LDEXP:
			srcdst.d = 0.0;
			srcdst.s = *wptr;
			srcdst.s <<= 7;
			srcdst.s += 0200;
			adjust = SHORT;
			ccset = FZ|FN;
#ifdef	DEBUG
fprintf(stderr,"LDEXP %o gives %o\n",*wptr,srcdst.s);
#endif
			break;
		case	MODD:
			srcdst.d *= fregs[fac].d;
			fregs[fac].d = (double)(long)srcdst.d;
			if(~fac & 1) fregs[fac + 1].d = fregs[fac].d;
			srcdst.d -= fregs[fac].d;
			ccset = FN|FZ;
			fregs[fac].d = srcdst.d;
#ifdef DEBUG
fprintf(stderr,"MODD %o %o\n",fregs[fac].s,fregs[fac+1].s);
#endif
			break;
		case	STCDL:
			if(lng)
				adjust = output = LONG;
			else
				adjust = output = SHORT;
			if(mode == 0) output = SHORT;
			srcdst.l = fregs[fac].d;
#ifdef DEBUG
fprintf(stderr,"STCDL %o\n",srcdst.l);
#endif
			ccset = FZ|FN;
			break;
		case	STEXP:
#ifdef DEBUG
fprintf(stderr,"STEXP of %o gives ",srcdst.s);
#endif
			srcdst.s &= 077600;
			srcdst.s >>= 7;
			srcdst.s -= 0200;
			adjust = output = SHORT;
			ccset = FZ|FN;
#ifdef DEBUG
fprintf(stderr,"%o\n",srcdst.s);
#endif
			break;
		default:
			return(-1);
		}
	}
	if(ccset & FZ) {
		fps &= ~FZ;
		if(srcdst.d == 0.0) fps |= FZ;
		if(!dbl && srcdst.f == 0.0) fps |= FZ;
	}
	if(ccset & FN) {
		fps &= ~FN;
		if(srcdst.f < 0.0) fps |= FN;
	}
	switch(instr & 0177400) {
	case	STCDL:
	case	STEXP:
		psl &= ~017;
		psl |= (fps & 017);
		break;
	default:
		break;
	}
	switch(output) {
	case	NONE:
		break;
	case	SHORT:
		*((short *)wptr) = srcdst.s;
		srcdst.d = 0.0;
		break;
	case	LONG:
		if(mode == 4) wptr--;
		*((long *)wptr) = longrev(srcdst.l);
		break;
	case	FLOAT:
		if(mode == 4) wptr--;
		*((float *)wptr) = srcdst.f;
		break;
	case	DOUBLE:
		if(mode == 4) wptr -= 3;
		*((double *)wptr) = srcdst.d;
		break;
	}
	switch(mode) {
	case	0:
	case	1:
		break;
	case	2:
		switch(adjust) {
		case	SHORT:
			regs[ac] += 2;
			break;
		case	LONG:
		case	FLOAT:
			regs[ac] += 4;
			break;
		case	DOUBLE:
			regs[ac] += 8;
			break;
		case	NONE:
			break;
		}
		if(ac == 7) pc++;
		break;
	case	3:
		regs[ac] += 2;
		if(ac == 7) pc++;
		break;
	case	4:
		switch(adjust) {
		case	SHORT:
			regs[ac] -= 2;
			break;
		case	LONG:
		case	FLOAT:
			regs[ac] -= 4;
			break;
		case	DOUBLE:
			regs[ac] -= 8;
			break;
		case	NONE:
			break;
		}
		break;
	case	5:
		regs[ac] -= 2;
		break;
	case	6:
	case	7:
		pc++;
		break;
	}
	return(0);
#endif
}
#ifndef NOFPSIM
unsigned short *locate(mode, ac) {
	register unsigned short *wptr;
	switch(mode) {
	case	0:
		/* mode 0 normally implies fregs */
		wptr = (unsigned short *)&fregs[ac];
		break;
	case	1:
		break;
	case	2:
		wptr = (unsigned short *)(int)regs[ac];
		break;
	case	3:
		wptr = (unsigned short *)regs[ac];
		wptr = (unsigned short *)*wptr;
		break;
	case	4:
		wptr = (unsigned short *)regs[ac];
		wptr--;
		break;
	case	5:
		wptr = (unsigned short *)regs[ac];
		wptr--;
		wptr = (unsigned short *)*wptr;
		break;
	case	6:
		wptr = (unsigned short *)((regs[ac] + *pc) & 0177776);
		if(ac == 7) wptr++;
		break;
	case	7:
		wptr = (unsigned short *)((regs[ac] + *pc) & 0177776);
		if(ac == 7) wptr++;
		wptr = (unsigned short *)*wptr;
		break;
	}
	return(wptr);
}
#endif
