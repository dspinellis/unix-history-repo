/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ascode.c	5.1 (Berkeley) 4/24/85";
#endif not lint

#include <stdio.h>
#include "as.h"
#include "assyms.h"

insout(opcode, ap, nact)
	struct	Opcode	opcode;
	struct	arg	*ap;
	int	nact;
{
	int	jxxflg;
	reg	struct	instab	*ip;		/* the instruction */
	reg	struct	arg	*ap_walk;	/* actual param walk */
	reg	int	i;
	reg	int	ap_type;		/* actual param type */
	reg	int	ap_type_mask;		/* masked actual param */

	jxxflg = nact;
	if (nact < 0)
		nact = -nact;
	if (passno == 1) {
		if (!(ITABCHECK(opcode)))
			panic("Botched reference into itab");
		ip = ITABFETCH(opcode);
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
		 *	The switch value is >> by TYPLG so that the switch
		 *	code is dense, not implemented as a sequence
		 *	of branches but implemented as a casel.
		 *	In addition, cases ACCI and ACCR are added to force
		 *	dense switch code.
		 *	switch on the type of fp
		 */
		switch( ((fetcharg(ip, i-1)) & ACCESSMASK) >> TYPLG){
		case ACCI >> TYPLG:
		case ACCR >> TYPLG:
			break;
		case ACCB >> TYPLG:
			if ( !((ap_type_mask == AEXP) || (ap_type_mask == AIMM)) ){
				yyerror("arg %d, branch displacement must be an expression",i);
				return;
			}
			break;
		case ACCA >> TYPLG:
			switch(ap_type_mask){
			case AREG:	yyerror("arg %d, addressing a register",i);
					return;
			case AIMM:	if ( !(ap_type & ASTAR) ){
					 yyerror("arg %d, addressing an immediate operand",i);
					 return;
					}
			}
			break;
		case ACCM >> TYPLG:
		case ACCW >> TYPLG:
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
		ijxout(opcode, ap, nact);
	else
		putins(opcode, ap, nact);
}

extern	int d124;

putins(opcode, ap, n)
	struct	Opcode	opcode;
	register struct arg *ap;
	int	n;			/* Must be positive */
{
	reg	struct exp 	*xp;
	reg	int 	argtype;
		int 	i;
		int	reloc_how;
		int	value;

#ifdef DEBUG
	fflush(stdout);
#endif
	if (passno == 2)
		goto PASS2;

	dotp->e_xvalue += n;		/* at least one byte per arg */
	switch(opcode.Op_eopcode){
	case NEW:
	case CORE:
		dotp->e_xvalue += 1;	/* 1 byte opcode */
		break;
	case ESCD:
	case ESCF:
		dotp->e_xvalue += 2;	/* 2 byte opcode */
		break;
	default:
		panic("Bad escape opcode");
	}

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
			argtype = fetcharg(ITABFETCH(opcode), i);
			if (argtype == A_BB)
				break;
			if (argtype == A_BW){
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
			dotp->e_xvalue += 1;
			if (ISBYTE(xp->e_xvalue))
				break;
			dotp->e_xvalue += 1;
			if (ISWORD(xp->e_xvalue))
				break;
			dotp->e_xvalue += 2;
			break;

		case AIMM: 
			if (ap->a_atype&ASTAR) {
				argtype=TYPL;
			} else {
				argtype = fetcharg(ITABFETCH(opcode), i);
				if (argtype&ACCA)
					argtype = TYPL;
				else
					argtype &= TYPMASK;
				xp = ap->a_xp;
				if (immconstant(ap->a_xp, argtype, &value))
					break;
			}
			dotp->e_xvalue += ty_nbyte[argtype];
	    }	/*end of the switch on the type*/
	}	/*end of looping for all arguments*/
	return;

PASS2:
	/*
	 *	Output the opcode
	 */
	switch(opcode.Op_eopcode){
	case NEW:
		nnewopcodes++;
		break;
	case ESCD:
	case ESCF:
		nGHopcodes++;
		Outb(opcode.Op_eopcode);
		break;
	case CORE:
		break;
	default:
		panic("Bad escape opcode");
	}
	Outb(opcode.Op_popcode);

	for (i=0; i<n; i++,ap++) {/* now for the arguments */
		argtype=ap->a_atype;
		xp=ap->a_xp;
		reloc_how = TYPNONE;
		if (argtype&AINDX) {
			{ Outb(0x40 | ap->a_areg2); }
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
			argtype = fetcharg(ITABFETCH(opcode), i);
			if (argtype == A_BB) {
				ap->a_areg1 = argtype = 
					xp->e_xvalue - (dotp->e_xvalue + 1);
				if ((xp->e_xtype & XTYPE) == XUNDEF)
					yywarning("%s: destination label is external",
						FETCHNAME(ITABFETCH(opcode)));
				if (!ISBYTE(argtype))
					yyerror("%s: Branch too far(%db): try -J flag",
						FETCHNAME(ITABFETCH(opcode)),
						argtype);
				break;
			}
			if (argtype == A_BW) {
				ap->a_areg1 = argtype = xp->e_xvalue
					-= dotp->e_xvalue + 2;
				if ((xp->e_xtype & XTYPE) == XUNDEF)
					yywarning("%s: destination label is external",
						FETCHNAME(ITABFETCH(opcode)));
				xp->e_xtype = XABS;
				if (!ISWORD(argtype))
					yyerror("%s: Branch too far(%db): try -J flag",
						FETCHNAME(ITABFETCH(opcode)),
						argtype);
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
			if (ISBYTE(xp->e_xvalue))
				break;
			ap->a_areg1 += 0x20;
			reloc_how = TYPW;
			if (ISWORD(xp->e_xvalue))
				break;
			ap->a_areg1 += 0x20;
			reloc_how = TYPL;
			break;
		
		case AIMM:  /* $expr */
			if (ap->a_atype&ASTAR) {
				argtype=TYPL;
			} else {
				argtype = fetcharg(ITABFETCH(opcode), i);
				if (argtype&ACCA)
					argtype = TYPL;
				else
					argtype &= TYPMASK;
				if (immconstant(xp, argtype, &value)){
					reloc_how = TYPNONE;
					ap->a_areg1 = value;
					break;
				}
			}
			ap->a_areg1 |= 0x8F;
			reloc_how = argtype;
			break;
		
		}	/*end of the switch on argtype*/
		/*
		 *	use the first byte to describe the argument
		 */
		Outb(ap->a_areg1);
		if (reloc_how != TYPNONE) 
			outrel(xp, reloc_how);
	}	/*end of the for to pick up all arguments*/
}
/*
 *	Is xp an immediate constant?
 *	argtype: how the instruction will interpret the bytes
 *	xp->e_number.num_tag ("numtype"): the kind of number given
 *
 *	Use the following table:
 *	float: TYPF, TYPD, TYPG, TYPH
 *	quad: TYPQ, TYPO
 *	int: TYPG, TYPW, TYPL
 *
 *				numtype
 *	argtype		float	quad	int
 *	
 *	float		slitflt	slitflt	slitflt
 *	quad		0	0	0
 *	int		0..63	0	0..63
 *
 *	Where the table entry implies the predicate to return.
 */
#define	IMMFLT	1		/* these flags are not used by anybody (yet) */
#define	IMMINT	2

int immconstant(xp, argtype, valuep)
	reg	struct	exp	*xp;
		int	argtype;
		int	*valuep;
{
	reg	int	back = 0;
		int	numtype;
	reg	int	fits;

	if ((xp->e_xtype & XTYPE) != XABS)
		return(0);
	if ((xp->e_xtype & XFORW) != 0)
		return(0);
	numtype = xp->e_number.num_tag;

	fits = 1;
	if (passno == 2) switch(argtype){
	case TYPB:
		switch(numtype){
		default:	fits = 0; break;
		case TYPB:	fits = 1; break;
		case TYPW:	
		case TYPL:
			fits = ISBYTE(xp->e_xvalue) || ISUBYTE(xp->e_xvalue);
			break;
		}
		break;
	case TYPW:
		switch(numtype){
		default:	fits = 0; break;
		case TYPB:
		case TYPW:	fits = 1; break;
		case TYPL:
			fits = ISWORD(xp->e_xvalue) || ISUWORD(xp->e_xvalue);
			break;
		}
		break;
	case TYPF:
		if (numtype == TYPD){	/* same format for first 32 bits */
			fits = 1;
			break;
		}
		/*FALLTHROUGH*/
	default:
		fits = ty_nbyte[argtype] >= ty_nbyte[numtype];
	}
	if (!fits){
	  yywarning("Immediate constant type %s mismatches instruction type %s",
		ty_string[numtype],
		ty_string[argtype]);
	}

	switch(argtype){
	case TYPF:
	case TYPG:
	case TYPD:
	case TYPH:
		back = slitflt(xp->e_number, argtype, valuep);
		break;
	case TYPO:
	case TYPQ:
		back = 0;
		break;
	case TYPB:
	case TYPW:
	case TYPL:
		switch(numtype){
		case TYPO:
		case TYPQ:
			back = 0;
			break;
		default:
			*valuep = xp->e_xvalue;
			back = ISLIT(xp->e_xvalue);
			break;
		}
		break;
	}
	return(back);
}
