/* Copyright (c) 1980 Regents of the University of California */
static	char sccsid[] = "@(#)ascode.c 4.1 %G%";
#include <stdio.h>
#include <sys/types.h>
#include <a.out.h>
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
	if (passno == 1) {
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
			argcompat(--ap2, ip->argtype[i], i+1);
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

	if ( (exp & ACCB) && (!((atm == AEXP) || (atm == AIMM))) ){
		yyerror("arg %d, branch displacement must be an expression",i);
		return;
	}
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

extern	int d124;
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

#ifdef DEBUG
	fflush(stdout);
#endif
	if (passno == 2)
		goto PASS2;

	dotp->xvalue += n+1;	/* 1 for the opcode, at least 1 per arg */
	for (i=0; i<n; i++,ap++) {/* some args take more than 1 byte */
	    xtrab = 0;
	    a=ap->atype;
	    if (a & AINDX)
		dotp->xvalue++;
	    switch (a&~(AINDX|ASTAR)) {
		case AEXP: 
			a = itab[op]->argtype[i];
			if (a == ACCB+TYPB)
				break;
			if (a==ACCB+TYPW){
				dotp->xvalue++;
				break;
			}
			/*
			 *	Reduces to PC relative
			 */
			xtrab++;
			dotp->xvalue += ap->dispsize;
			break;
		
		case ADISP: 
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

		case AIMM: 
			if (ap->atype&ASTAR) a=TYPL;
			else {
				a = itab[op]->argtype[i];
				if (a&ACCA)
					a = TYPL;
				else
					a &= TYPMASK;
				xp = ap->xp;
				if (   ((xp->xtype&XTYPE)==XABS)
				    && (!(xp->xtype&XFORW))
				    && (xp->xvalue>=0)
				    && (xp->xvalue<=63) 
				    && (xp->yvalue == 0)
				    && (a != TYPD)
				    && (a != TYPF)
				)
						break;
			}
			switch (a) {
			case TYPD:
			case TYPF:
				if (   !(((xp->xtype&XTYPE)==XABS)
				    && (!(xp->xtype&XFORW))
				    && (slitflt(xp)))
				){
				/* it is NOT short */
					dotp->xvalue += ((a==TYPF)?
						4 : 8);
				}
				break;
			case TYPQ: 
				dotp->xvalue += 8;break;
			case TYPL:
				dotp->xvalue += 4;break;
			case TYPW: 
				dotp->xvalue += 2;break;
			case TYPB: 
				dotp->xvalue += 1;break;
			}	/*end of the switch on a*/
	    }	/*end of the switch on the type*/
	}	/*end of looping for all arguments*/
	return;

PASS2:

#ifdef UNIX
	outb(op); /* the opcode */
#endif UNIX
#ifdef VMS
	*vms_obj_ptr++ = -1; *vms_obj_ptr++ = (char)op;
	dotp->xvalue += 1;
#endif VMS

	for (i=0; i<n; i++,ap++) {/* now for the arguments */
		a=ap->atype;
		xp=ap->xp;
		xtrab=0;
		if (a&AINDX) {
#ifdef UNIX
			{ outb(0x40 | ap->areg2); }
#endif UNIX
#ifdef VMS
			{ *vms_obj_ptr++ = -1;
			  *vms_obj_ptr++ = (0x40 | ap->areg2);
			  dotp->xvalue += 1; }
#endif VMS
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
		case AEXP: /* expr */
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
		
		case ADISP: /* expr(%r) */
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
		
		case AIMM:  /* $expr */
			if (ap->atype&ASTAR)
				a=TYPL;
			else {
				a = itab[op]->argtype[i];
				if (a&ACCA)
					a=TYPL;
				else
					a &= TYPMASK;
				if (    ( (xp->xtype&XTYPE) == XABS) 
				    && !(xp->xtype&XFORW)
				    &&  (xp->xvalue >= 0)
				    &&  (xp->xvalue <= 63)
				    &&  (xp->yvalue == 0)
				    &&  (a != TYPF)
				    &&  (a != TYPD) ) {
					ap->areg1 = xp->xvalue;
					break;
				}
			}
			ap->areg1 |= 0x8F;
			switch (a) {
			case TYPD:
			case TYPF:
				if (   ((xp->xtype&XTYPE)==XABS)
				    && (!(xp->xtype&XFORW))
				    && (slitflt(xp))
				){
					ap->areg1=extlitflt(xp);
				} else {
					xtrab = (a==TYPF) ? LEN4: LEN8;
				}
				break;
			case TYPQ: xtrab = LEN8; break;
			case TYPL: xtrab = LEN4; break;
			case TYPW: xtrab = LEN2; break;
			case TYPB: xtrab = LEN1; break;
			}	
			break;
		
		}	/*end of the switch on a*/
		/*
		 *	use the first byte to describe the argument
		 */
#ifdef UNIX
		outb(ap->areg1);
#endif UNIX
#ifdef VMS
		*vms_obj_ptr++ = -1; *vms_obj_ptr++ = (char)(ap->areg1);
		dotp->xvalue += 1;
		if ((vms_obj_ptr-sobuf) > 400) {
			write(objfil,sobuf,vms_obj_ptr-sobuf);
			vms_obj_ptr=sobuf+1;
		}
#endif VMS
		if (xtrab) 
			/*
			 *	Floating point numbers are written to a.out
			 *	by outrel; they require that the least significant
			 *	4 bytes of an 8 byte double precision number
			 *	immediately follow the field xvalue, which
			 *	they do.
			 */
			outrel(&xp->xvalue, xtrab, xp->xtype, xp->xname);
	}	/*end of the for to pick up all arguments*/
}
