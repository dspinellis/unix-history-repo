/* Copyright (c) 1980 Regents of the University of California */
static	char sccsid[] = "@(#)ascode.c 4.7 11/5/80";
#include <stdio.h>
#include "as.h"
#include "assyms.h"

/*
 *	Loader reference types  (plust PCREL) to bytes and lg bytes
 */
/*		LEN1	LEN1+PC	LEN2	LEN2+PC	LEN4	LEN4+PC	LEN8	LEN8+PC*/
int	reflen[] = 	/* {LEN*+PCREL} ==> number of bytes */
{0,	0,	1,	1,	2,	2,	4,	4,	8,	8};	
int	lgreflen[] =	/* {LEN*+PCREL} ==> lg number of bytes */ 
{-1,	-1,	0,	0,	1,	1,	2,	2,	3,	3};

/*
 *	Sizes to Loader reference types and type flags
 */
/*0	1	2	3	4	5	6	7	8*/
int	len124[] = 	/* {1,2,4,8} ==> {LEN1, LEN2, LEN4, LEN8} */
{0,	LEN1,	LEN2,	0,	LEN4,	0,	0,	0,	LEN8};
char	mod124[] = 	/* {1,2,4,8} ==> {bits to construct operands */
{0,	0x00,	0x20,	0,	0x40,	0,	0,	0,	0};
int	type_124[] =	/* {1,2,4,8} ==> {TYPB, TYPW, TYPL, TYPQ} */
{0,	 TYPB,	TYPW,	 0,	 TYPL,	 0,	 0,	 0,	 TYPQ};

/*
 *	type flags to Loader reference and byte lengths
 */
/*TYPB	TYPW	TYPL	TYPQ	TYPF	TYPD*/
int	ty_NORELOC[] =	/* {TYPB..TYPD} ==> {1 if relocation not OK */
{0,	0,	0,	1,	1,	1};
int	ty_LEN[] =	/* {TYPB..TYPD} ==> {LEN1..LEN8} */
{LEN1,	LEN2,	LEN4,	LEN8,	LEN4,	LEN8};
int	ty_nbyte[] =	/* {TYPB..TYPD} ==> {1,2,4,8} */
{1,	2,	4,	8,	4,	8};
int	ty_nlg[] =	/* {TYPB..TYPD} ==> lg{1,2,4,8} */
{0,	1,	2,	3,	2,	3};

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
		ap_type = ap_walk->a_atype;
		ap_type_mask = ap_type & AMASK;
		/*
		 *	The switch value is >> by 3 so that the switch
		 *	code is dense, not implemented as a sequence
		 *	of branches but implemented as a casel.
		 *	In addition, cases ACCI and ACCR are added to force
		 *	dense switch code.
		 */
		switch( ((fetcharg(ip, i-1)) & ACCESSMASK)>>3){	/* type of fp */
		case ACCI >> 3:
		case ACCR >> 3:
			break;
		case ACCB >> 3:
			if ( !((ap_type_mask == AEXP) || (ap_type_mask == AIMM)) ){
				yyerror("arg %d, branch displacement must be an expression",i);
				return;
			}
			break;
		case ACCA >> 3:
			switch(ap_type_mask){
			case AREG:	yyerror("arg %d, addressing a register",i);
					return;
			case AIMM:	if ( !(ap_type & ASTAR) ){
					 yyerror("arg %d, addressing an immediate operand",i);
					 return;
					}
			}
			break;
		case ACCM >> 3:
		case ACCW >> 3:
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
			case ADECR:
			case AINCR:	if (ap_walk->a_areg1==ap_walk->a_areg2) {
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

putins(op, ap, n)
	/*
	 *	n had better be positive
	 */
	register struct arg *ap;
{
	register struct exp 	*xp;
	register int 		argtype;
	int 			i;
	int			reloc_how;

#ifdef DEBUG
	fflush(stdout);
#endif
	if (passno == 2)
		goto PASS2;

	dotp->e_xvalue += n+1;		/* 1 for the opcode, at least 1 per arg */
	for (i=0; i<n; i++,ap++) {	/* some args take more than 1 byte */
	    argtype = ap->a_atype;
	    if (argtype & AINDX)
		dotp->e_xvalue++;
	    /*
	     *	This switch has been fixed by enumerating the no action
	     *	alternatives (those that have 1 one byte of code)
	     *	so that a casel instruction is emitted.
	     */
	    switch (argtype&~(AINDX|ASTAR)) {
		case AREG:
		case ABASE:
		case ADECR:
		case AINCR:
			break;
		case AEXP: 
			argtype = fetcharg(itab[op], i);
			if (argtype == ACCB+TYPB)
				break;
			if (argtype==ACCB+TYPW){
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
			if (xp->e_xvalue==0 && !(argtype&ASTAR))
				break;
			dotp->e_xvalue++;
			if ((xp->e_xvalue<MINBYTE) || (xp->e_xvalue>MAXBYTE))
				dotp->e_xvalue++;
			if ((xp->e_xvalue<MINWORD) || (xp->e_xvalue>MAXWORD))
				dotp->e_xvalue += 2;
			break;

		case AIMM: 
			if (ap->a_atype&ASTAR) argtype=TYPL;
			else {
				argtype = fetcharg(itab[op], i);
				if (argtype&ACCA)
					argtype = TYPL;
				else
					argtype &= TYPMASK;
				xp = ap->a_xp;
				if (   ((xp->e_xtype&XTYPE)==XABS)
				    && (!(xp->e_xtype&XFORW))
				    && (xp->e_xvalue>=0)
				    && (xp->e_xvalue<=63) 
				    && (xp->e_yvalue == 0)
				    && (argtype != TYPD)
				    && (argtype != TYPF)
				)
						break;
			}
			switch (argtype) {
			case TYPD:
			case TYPF:
				if (   !(((xp->e_xtype&XTYPE)==XABS)
				    && (!(xp->e_xtype&XFORW))
				    && (slitflt(xp)))
				){
				/* it is NOT short */
					dotp->e_xvalue += ((argtype==TYPF)?
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
			}	/*end of the switch on argtype*/
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
		argtype=ap->a_atype;
		xp=ap->a_xp;
		reloc_how = TYPNONE;
		if (argtype&AINDX) {
#ifdef UNIX
			{ outb(0x40 | ap->a_areg2); }
#endif UNIX
#ifdef VMS
			{ *vms_obj_ptr++ = -1;
			  *vms_obj_ptr++ = (0x40 | ap->a_areg2);
			  dotp->e_xvalue += 1; }
#endif VMS
			argtype &= ~AINDX;
		}
		if (argtype&ASTAR) {
			ap->a_areg1 |= 0x10;
			argtype &= ~ASTAR;
		}
		switch (argtype) {
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
			argtype = fetcharg(itab[op], i);
			if (argtype == ACCB+TYPB) {
				ap->a_areg1 = argtype = 
					xp->e_xvalue - (dotp->e_xvalue + 1);
				if (argtype<MINBYTE || argtype>MAXBYTE)
					yyerror("Branch too far"); break;
			}
			if (argtype == ACCB+TYPW) {
				ap->a_areg1 = argtype = xp->e_xvalue
					-= dotp->e_xvalue + 2;
				xp->e_xtype = XABS;
				if (argtype<MINWORD || argtype>MAXWORD) 
					yyerror("Branch too far");
				xp->e_xvalue = argtype>>8;
				reloc_how = TYPB;
				break;
			}
			/* reduces to expr(pc) mode */
			ap->a_areg1 |= (0xAF + mod124[ap->a_dispsize]);
			reloc_how = type_124[ap->a_dispsize] + RELOC_PCREL;
			break;
		
		case ADISP: /* expr(%r) */
			ap->a_areg1 |= 0xA0;
			if ((xp->e_xtype&XTYPE)!=XABS || xp->e_xtype&XFORW){
				ap->a_areg1 += mod124[ap->a_dispsize];
				reloc_how = type_124[ap->a_dispsize];
				break;
			}
			if (xp->e_xvalue==0 && !(ap->a_areg1&0x10)) {
				ap->a_areg1 ^= 0xC0;
				break;
			}
			reloc_how = TYPB;
			if ((xp->e_xvalue<MINBYTE) || (xp->e_xvalue>MAXBYTE)){
				ap->a_areg1 += 0x20;
				reloc_how = TYPW;
			}
			if ((xp->e_xvalue<MINWORD) || (xp->e_xvalue>MAXWORD)){
				ap->a_areg1 += 0x20;
				reloc_how = TYPL;
			}
			break;
		
		case AIMM:  /* $expr */
			if (ap->a_atype&ASTAR)
				argtype=TYPL;
			else {
				argtype = fetcharg(itab[op], i);
				if (argtype&ACCA)
					argtype=TYPL;
				else
					argtype &= TYPMASK;
				if (    ( (xp->e_xtype&XTYPE) == XABS) 
				    && !(xp->e_xtype&XFORW)
				    &&  (xp->e_xvalue >= 0)
				    &&  (xp->e_xvalue <= 63)
				    &&  (xp->e_yvalue == 0)
				    &&  (argtype != TYPF)
				    &&  (argtype != TYPD) ) {
					ap->a_areg1 = xp->e_xvalue;
					break;
				}
			}
			ap->a_areg1 |= 0x8F;
			reloc_how = argtype;
			if (reloc_how == TYPD || reloc_how == TYPF){
				if (   ((xp->e_xtype&XTYPE)==XABS)
				    && (!(xp->e_xtype&XFORW))
				    && (slitflt(xp))
				){
					reloc_how = TYPNONE;
					ap->a_areg1=extlitflt(xp);
				}
			}	
			break;
		
		}	/*end of the switch on argtype*/
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
		if (reloc_how != TYPNONE) 
			outrel(xp, reloc_how);
	}	/*end of the for to pick up all arguments*/
}
