/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
#include "as.h"
#include "assyms.h"

insout(op, ap, nact)
	struct arg *ap;
{
	int jxxflg;

	op &= 0xFF;
	jxxflg = nact;
	if (nact < 0)
		nact = -nact;
	if (passno!=2) {
		register struct arg 	*ap2;
		register struct instab 	*ip;
		int 			i,nexp;
		ip = itab[op];
		nexp = ip->nargs;
		if (nact < nexp)
			yyerror("Too few arguments");
		if (nact > nexp) {
			yyerror("Too many arguments");
			nact = nexp;
		}
		/*
		 *	Check argument compatability with instruction template
		 */
		for (ap2 = ap+nact, i = nact; --i >= 0;)
			argcompat(--ap2, ip->argtype[i], i);
	}
	if (jxxflg < 0)
		ijxout(op, ap, nact);
	else putins(op, ap, nact);
}

argcompat(act, exp, i)
	struct arg *act;
	int exp,i;
{
	register 	at,atm;

	at = act->atype;
	atm = at & AMASK;

	if ((exp & ACCA) && (atm == AREG)) {
		yyerror("arg %d, addressing a register",i);
		return;
	}
	if ((exp&ACCW) && (atm==AIMM) && !(at&ASTAR)) {
		yyerror("arg %d, modifying a constant",i);
		return;
	}
	if (at & AINDX) {
		if (act->areg2==017) {
			yyerror("arg %d, PC used as index",i);
			return;
		}
		if (atm==AREG) {
			yyerror("arg %d, indexing the register file",i);
			return;
		}
		if (atm==AIMM) {
			yyerror("arg %d, indexing a constant",i);
			return;
		}
		if (((atm==ADECR) || (atm==AINCR)) && (act->areg1==act->areg2)) {
			yyerror("arg %d, indexing with modified register",i);
			return;
		}
	}
}

int d124 = 	{4};
int len124[] = 	{0,LEN1,LEN2,0,LEN4};
char mod124[] = {0,0x00,0x20,0,0x40};

putins(op, ap, n)
	/*
	 *	n had better be positive
	 */
	register struct arg *ap;
{
	register struct exp 	*xp;
	register int 		a;
	int 			i,xtrab;

	if (passno!=2) {
		dotp->xvalue += n+1;	/* 1 for the opcode, at least 1 per arg */
		for (i=0; i<n; i++,ap++) {/* some args take more than 1 byte */
			a=ap->atype;
			if (a & AINDX)
				dotp->xvalue++;
			switch (a&~(AINDX|ASTAR)) {
				case AEXP: {
					a = itab[op]->argtype[i];
					if (a == ACCB+TYPB)
						break;
					if (a==ACCB+TYPW){
						dotp->xvalue++;
						break;
					}
					dotp->xvalue += ap->dispsize;
					break;
				}
				case ADISP: {
					xp=ap->xp;
					if ((xp->xtype&XTYPE)!=XABS || xp->xtype&XFORW){
						dotp->xvalue += ap->dispsize;
						break;
					}
					if (xp->xvalue==0 && !(a&ASTAR))
						break;
					dotp->xvalue++;
					if ((xp->xvalue<MINBYTE) || (xp->xvalue>MAXBYTE))
						dotp->xvalue++;
					if ((xp->xvalue<MINWORD) || (xp->xvalue>MAXWORD))
						dotp->xvalue += 2;
					break;
				}
				case AIMM: {
					if (ap->atype&ASTAR) a=TYPL;
					else {
						xp = ap->xp;
						if ((xp->xtype&XTYPE)==XABS && !(xp->xtype&XFORW)
							&& xp->xvalue>=0 && xp->xvalue<=63) 
								break;
						a = itab[op]->argtype[i];
						if (a&ACCA)
							a = TYPL;
						else
							a &= TYPMASK;
					}
					switch (a) {
						case TYPD:
						case TYPF:
							if (slitflt(xp))
								break;
							if (a==TYPF)
								dotp->xvalue -= 4;
						case TYPQ: 
							dotp->xvalue += 4;
						case TYPL:
							dotp->xvalue += 2;
						case TYPW: 
							dotp->xvalue++;
						case TYPB: 
							dotp->xvalue++;
					}	/*end of the switch on a*/
				}	/*end of case AIMM*/
			}	/*end of the switch on the type*/
		}	/*end of looping for all arguments*/
		return;
	}	/*end of it being time for pass 1*/
	/*
	 *	PASS2 HERE
	 */

	outb(op); /* the opcode */
	for (i=0; i<n; i++,ap++) {/* now for the arguments */
		a=ap->atype;
		xp=ap->xp;
		xtrab=0;
		if (a&AINDX) {
			{ outb(0x40 | ap->areg2); }
			a &= ~AINDX;
		}
		if (a&ASTAR) {
			ap->areg1 |= 0x10;
			a &= ~ASTAR;
		}
		switch (a) {
			case AREG:		/* %r */
				ap->areg1 |= 0x50;
				break; 
			case ABASE:		/* (%r) */
				ap->areg1 |= 0x60;
				break; 
			case ADECR: 		/* -(%r) */
				ap->areg1 |= 0x70;
				break; 
			case AINCR:		/* (%r) */
				ap->areg1 |= 0x80;
				break;
			case AEXP: {/* expr */
				a = itab[op]->argtype[i];
				if (a == ACCB+TYPB) {
					ap->areg1 = a = 
						xp->xvalue - (dotp->xvalue + 1);
					if (a<MINBYTE || a>MAXBYTE)
						yyerror("Branch too far"); break;
				}
				if (a == ACCB+TYPW) {
					ap->areg1 = a = xp->xvalue
						-= dotp->xvalue + 2;
					xp->xtype = XABS;
					if (a<MINWORD || a>MAXWORD) 
						yyerror("Branch too far");
					xp->xvalue = a>>8;
					xtrab = LEN1;
					break;
				}
				/* reduces to expr(pc) mode */
				ap->areg1 |= (0xAF + mod124[ap->dispsize]);
				xtrab = len124[ap->dispsize]+PCREL;
				break;
			}
			case ADISP: {/* expr(%r) */
				ap->areg1 |= 0xA0;
				if ((xp->xtype&XTYPE)!=XABS || xp->xtype&XFORW){
					ap->areg1 += mod124[ap->dispsize];
					xtrab=len124[ap->dispsize];
					break;
				}
				if (xp->xvalue==0 && !(ap->areg1&0x10)) {
					ap->areg1 ^= 0xC0;
					break;
				}
				xtrab=LEN1;
				if ((xp->xvalue<MINBYTE) || (xp->xvalue>MAXBYTE)){
					ap->areg1 += 0x20;
					xtrab=LEN2;
				}
				if ((xp->xvalue<MINWORD) || (xp->xvalue>MAXWORD)){
					ap->areg1 += 0x20;
					xtrab=LEN4;
				}
				break;
			}
			case AIMM: { /* $expr */
				if (ap->atype&ASTAR)
					a=TYPL;
				else {
					if (    ( (xp->xtype&XTYPE) == XABS) 
					    && !(xp->xtype&XFORW)
					    &&  (xp->xvalue >= 0)
					    &&  (xp->xvalue <= 63) ) {
						ap->areg1 = xp->xvalue;
						break;
					}
					a = itab[op]->argtype[i];
					if (a&ACCA)
						a=TYPL;
					else
						a &= TYPMASK;
				}
				ap->areg1 |= 0x8F;
				switch (a) {
					case TYPD:
					case TYPF:
						if (slitflt(xp)){
							ap->areg1=extlitflt(xp);
							break;
						}
						if (a==TYPF) { 
							xtrab = LEN4;
							break;
						}
					case TYPQ: xtrab = LEN8; break;
					case TYPL: xtrab = LEN4; break;
					case TYPW: xtrab = LEN2; break;
					case TYPB: xtrab = LEN1; break;
				}
			}	/*end of the switch on AIMM*/
		}	/*end of the switch on a*/
		/*
		 *	use the first byte to describe the argument
		 */
		outb(ap->areg1);
		if (xtrab) 
			outrel(&xp->xvalue, xtrab, xp->xtype, xp->xname);
	}	/*end of the for to pick up all arguments*/
}
