/* Copyright (c) 1980 Regents of the University of California */
static	char sccsid[] = "@(#)ascode.c 4.4 %G%";
#include <stdio.h>
#include "as.h"
#include "assyms.h"

insout(op, ap, nact)
	struct arg *ap;
{
	int		jxxflg;
	register	struct	instab	*ip;		/* the instruction */
	register	struct	arg	*ap_walk;	/* actual param walk */
	register	int	i;
	register	int	ap_type;		/* actual param type */
	register	int	ap_type_mask;		/* masked actual param */
	op &= 0xFF;
	jxxflg = nact;
	if (nact < 0)
		nact = -nact;
	if (passno == 1) {
	    ip = itab[op];
	    if (nact < ip->i_nargs)
		yyerror("Too few arguments");
	    if (nact > ip->i_nargs) {
		yyerror("Too many arguments");
		nact = ip->i_nargs;
	    }
	    /*
	     *	Check argument compatability with instruction template
	     */
	    for (ap_walk = ap, i = 1; i <= nact; ap_walk++, i++){
		ap_type = ap_walk->a_type;
		ap_type_mask = ap_type & AMASK;
		switch( (fetcharg(ip, i-1)) & AMASK){	/* type of fp */
		case ACCB:
			if ( !((ap_type_mask == AEXP) || (ap_type_mask == AIMM)) ){
				yyerror("arg %d, branch displacement must be an expression",i);
				return;
			}
			break;
		case ACCA:
			switch(ap_type_mask){
			case AREG:	yyerror("arg %d, addressing a register",i);
					return;
			case AIMM:	if ( !(ap_type & ASTAR) ){
					 yyerror("arg %d, addressing an immediate operand",i);
					 return;
					}
			}
			break;
		case ACCM:
		case ACCW:
			switch(ap_type_mask){
			case AIMM:	if (!(ap_type&ASTAR)) {
					 yyerror("arg %d, modifying a constant",i);
					 return;
					}
			}
			break;
		}	/* end of the switch on fp_type */
		if (ap_type & AINDX) {
			if (ap_walk->a_areg2==0xF) {
				yyerror("arg %d, PC used as index",i);
				return;
			}
			switch(ap_type_mask){
			case AREG:	yyerror("arg %d, indexing the register file",i);
					return;
			case AIMM:	yyerror("arg %d, indexing a constant",i);
					return;
			case DECR:
			case INCR:	if (ap_walk->a_areg1==ap_walk->a_areg2) {
						yyerror("arg %d, indexing with modified register",i);
						return;
					}
					break;
			}	/* end of switch on ap_type_mask */
		} /* end of AINDX */
	   }
	} /* both passes here */
	if (jxxflg < 0)
		ijxout(op, ap, nact);
	else putins(op, ap, nact);
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

	dotp->e_xvalue += n+1;		/* 1 for the opcode, at least 1 per arg */
	for (i=0; i<n; i++,ap++) {	/* some args take more than 1 byte */
	    a = ap->a_atype;
	    if (a & AINDX)
		dotp->e_xvalue++;
	    switch (a&~(AINDX|ASTAR)) {
		case AEXP: 
			a = fetcharg(itab[op], i);
			if (a == ACCB+TYPB)
				break;
			if (a==ACCB+TYPW){
				dotp->e_xvalue++;
				break;
			}
			/*
			 *	Reduces to PC relative
			 */
			dotp->e_xvalue += ap->a_dispsize;
			break;
		
		case ADISP: 
			xp=ap->a_xp;
			if ((xp->e_xtype&XTYPE)!=XABS || xp->e_xtype&XFORW){
				dotp->e_xvalue += ap->a_dispsize;
				break;
			}
			if (xp->e_xvalue==0 && !(a&ASTAR))
				break;
			dotp->e_xvalue++;
			if ((xp->e_xvalue<MINBYTE) || (xp->e_xvalue>MAXBYTE))
				dotp->e_xvalue++;
			if ((xp->e_xvalue<MINWORD) || (xp->e_xvalue>MAXWORD))
				dotp->e_xvalue += 2;
			break;

		case AIMM: 
			if (ap->a_atype&ASTAR) a=TYPL;
			else {
				a = fetcharg(itab[op], i);
				if (a&ACCA)
					a = TYPL;
				else
					a &= TYPMASK;
				xp = ap->a_xp;
				if (   ((xp->e_xtype&XTYPE)==XABS)
				    && (!(xp->e_xtype&XFORW))
				    && (xp->e_xvalue>=0)
				    && (xp->e_xvalue<=63) 
				    && (xp->e_yvalue == 0)
				    && (a != TYPD)
				    && (a != TYPF)
				)
						break;
			}
			switch (a) {
			case TYPD:
			case TYPF:
				if (   !(((xp->e_xtype&XTYPE)==XABS)
				    && (!(xp->e_xtype&XFORW))
				    && (slitflt(xp)))
				){
				/* it is NOT short */
					dotp->e_xvalue += ((a==TYPF)?
						4 : 8);
				}
				break;
			case TYPQ: 
				dotp->e_xvalue += 8;break;
			case TYPL:
				dotp->e_xvalue += 4;break;
			case TYPW: 
				dotp->e_xvalue += 2;break;
			case TYPB: 
				dotp->e_xvalue += 1;break;
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
	dotp->e_xvalue += 1;
#endif VMS

	for (i=0; i<n; i++,ap++) {/* now for the arguments */
		a=ap->a_atype;
		xp=ap->a_xp;
		xtrab=0;
		if (a&AINDX) {
#ifdef UNIX
			{ outb(0x40 | ap->a_areg2); }
#endif UNIX
#ifdef VMS
			{ *vms_obj_ptr++ = -1;
			  *vms_obj_ptr++ = (0x40 | ap->a_areg2);
			  dotp->e_xvalue += 1; }
#endif VMS
			a &= ~AINDX;
		}
		if (a&ASTAR) {
			ap->a_areg1 |= 0x10;
			a &= ~ASTAR;
		}
		switch (a) {
		case AREG:		/* %r */
			ap->a_areg1 |= 0x50;
			break; 
		case ABASE:		/* (%r) */
			ap->a_areg1 |= 0x60;
			break; 
		case ADECR: 		/* -(%r) */
			ap->a_areg1 |= 0x70;
			break; 
		case AINCR:		/* (%r)+ */
			ap->a_areg1 |= 0x80;
			break;
		case AEXP: /* expr */
			a = fetcharg(itab[op], i);
			if (a == ACCB+TYPB) {
				ap->a_areg1 = a = 
					xp->e_xvalue - (dotp->e_xvalue + 1);
				if (a<MINBYTE || a>MAXBYTE)
					yyerror("Branch too far"); break;
			}
			if (a == ACCB+TYPW) {
				ap->a_areg1 = a = xp->e_xvalue
					-= dotp->e_xvalue + 2;
				xp->e_xtype = XABS;
				if (a<MINWORD || a>MAXWORD) 
					yyerror("Branch too far");
				xp->e_xvalue = a>>8;
				xtrab = LEN1;
				break;
			}
			/* reduces to expr(pc) mode */
			ap->a_areg1 |= (0xAF + mod124[ap->a_dispsize]);
			xtrab = len124[ap->a_dispsize]+PCREL;
			break;
		
		case ADISP: /* expr(%r) */
			ap->a_areg1 |= 0xA0;
			if ((xp->e_xtype&XTYPE)!=XABS || xp->e_xtype&XFORW){
				ap->a_areg1 += mod124[ap->a_dispsize];
				xtrab=len124[ap->a_dispsize];
				break;
			}
			if (xp->e_xvalue==0 && !(ap->a_areg1&0x10)) {
				ap->a_areg1 ^= 0xC0;
				break;
			}
			xtrab=LEN1;
			if ((xp->e_xvalue<MINBYTE) || (xp->e_xvalue>MAXBYTE)){
				ap->a_areg1 += 0x20;
				xtrab=LEN2;
			}
			if ((xp->e_xvalue<MINWORD) || (xp->e_xvalue>MAXWORD)){
				ap->a_areg1 += 0x20;
				xtrab=LEN4;
			}
			break;
		
		case AIMM:  /* $expr */
			if (ap->a_atype&ASTAR)
				a=TYPL;
			else {
				a = fetcharg(itab[op], i);
				if (a&ACCA)
					a=TYPL;
				else
					a &= TYPMASK;
				if (    ( (xp->e_xtype&XTYPE) == XABS) 
				    && !(xp->e_xtype&XFORW)
				    &&  (xp->e_xvalue >= 0)
				    &&  (xp->e_xvalue <= 63)
				    &&  (xp->e_yvalue == 0)
				    &&  (a != TYPF)
				    &&  (a != TYPD) ) {
					ap->a_areg1 = xp->e_xvalue;
					break;
				}
			}
			ap->a_areg1 |= 0x8F;
			switch (a) {
			case TYPD:
			case TYPF:
				if (   ((xp->e_xtype&XTYPE)==XABS)
				    && (!(xp->e_xtype&XFORW))
				    && (slitflt(xp))
				){
					ap->a_areg1=extlitflt(xp);
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
		outb(ap->a_areg1);
#endif UNIX
#ifdef VMS
		*vms_obj_ptr++ = -1; *vms_obj_ptr++ = (char)(ap->a_areg1);
		dotp->e_xvalue += 1;
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
			outrel(&xp->e_xvalue, xtrab, xp->e_xtype, xp->e_xname);
	}	/*end of the for to pick up all arguments*/
}
