/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)ascode.c 4.11 6/30/83";
#endif not lint

#include <stdio.h>
#include "as.h"
#include "assyms.h"

insout(opcode, ap, nact)
	u_char	opcode;
	struct	arg	*ap;
	int	nact;
{
	int	jxxflg;
	reg	struct	instab	*ip;		/* the instruction */
	reg	struct	arg	*ap_walk;	/* actual param walk */
	reg	int	i;
	reg	int	ap_type;		/* actual param type */
	reg	int	ap_type_mask;		/* masked actual param */
	reg	int	argtype;

	jxxflg = nact;
	if (nact < 0)
		nact = -nact;
	if (passno == 1) {
		ip = ITABFETCH(opcode & 0xFF);
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
		argtype = fetcharg(ip, i-1);
		switch( (argtype & ACCESSMASK) >> TYPLG){
		case ACCI >> TYPLG:
			break;
		case ACCR >> TYPLG:
			if ((argtype == A_RQ) && 
			   (ap_type_mask == AREG) &&
			   (ap_walk->a_areg1 & 0x01)) {
				yyerror("arg %d, register must be even",i);
				return;
			}
			if (ap_type_mask == ADECR)
			{
				 yyerror("arg %d, a source arg cant be auto decrement",i);
				 return;
			}
			if (argtype == A_RD)
			{
				if (ap_type_mask == AIMM) 
				   if ( !(ap_type&ASTAR))
				{
					 yyerror("arg %d, cant be immediate data",i);
					 return;
				}
				if ((argtype == A_RD) && 
				   ((ap_type_mask == AINCR) || 
				   (ap_type_mask == ADECR)))
				{
					 yyerror("arg %d, cant be auto increment/decrement",i);
					 return;
				}
				if ((argtype == A_RD) && 
				   (ap_type_mask == AREG) &&
				   (ap_walk->a_areg1 & 0x01)) {
					yyerror("arg %d, register must be even",i);
					return;
				}
			}
			break;
		case ACCB >> TYPLG:
			if ( ap_type_mask != AEXP)
			{
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
			if ((argtype == A_WQ) && 
			   (ap_type_mask == AREG) &&
			   (ap_walk->a_areg1 & 0x01)) {
				yyerror("arg %d, register must be even",i);
				return;
			}
			switch(ap_type_mask){
			case AIMM:	if (!(ap_type&ASTAR)) {
					 yyerror("arg %d, modifying a constant",i);
					 return;
					}
			case AINCR:
				 yyerror("arg %d, a destination arg cant be auto increment",i);
				 return;
			}
			break;
		}	/* end of the switch on fp_type */
		if (ap_type & AINDX) {
			if (ap_walk->a_areg2==0xF) {
				yyerror("arg %d, PC used as index",i);
				return;
			}
			if (ap_walk->a_areg2==0xE) {
				yyerror("arg %d, SP used as index",i);
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
	u_char	opcode;
	register struct arg *ap;
	int	n;			/* Must be positive */
{
	reg	struct exp 	*xp;
	reg	int 	argtype;
		int 	i;
		int	reloc_how;

#ifdef DEBUG
	fflush(stdout);
#endif
	if (passno == 2)
		goto PASS2;

	dotp->e_xvalue += n+1;		/* at least one byte per arg */

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
			dotp->e_xvalue ++;
			if (ISBYTE(xp->e_xvalue))
				break;
			dotp->e_xvalue ++;
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
				if (   ((xp->e_xtype&XTYPE)==XABS)
				    && (!(xp->e_xtype&XFORW))
				    && (argtype != TYPD)
				    && (argtype != TYPF))
				{
				    if ((xp->e_xvalue>=0)
				    && (xp->e_xvalue<=63))
						break;
				    if (ISBYTE(xp->e_xvalue)) 
					argtype = TYPB;
				    else
					if (ISWORD(xp->e_xvalue))
						argtype = TYPW;
					else
						argtype = TYPL;
				}
				else
					argtype = TYPL;
			}
			switch (argtype) {
			case TYPD:
			case TYPF:
				    if (!(slitflt(xp)))
					return;
			case TYPQ: 
			case TYPL:
			case TYPW: 
			case TYPB: 
					dotp->e_xvalue += ty_nbyte[argtype];
					break;
			}	/*end of the switch on argtype*/
	    }	/*end of the switch on the type*/
	}	/*end of looping for all arguments*/
	return;

PASS2:
	/*
	 *	Output the opcode
	 */
	Outb(opcode);
	if ((passno == 2) && liston)
	{
		byte_out (opcode);
		*layoutpos++ = ' ';
	}

	for (i=0; i<n; i++,ap++) {/* now for the arguments */
		argtype=ap->a_atype;
		xp=ap->a_xp;
		reloc_how = TYPNONE;
		if (argtype&AINDX) {
			{ Outb(0x40 | ap->a_areg2); 
			  if ((passno == 2) && liston)
			  	byte_out (0x40 | ap->a_areg2); }
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
				if (xp->e_xtype & XXTRN)
					yywarning("%s: destination label is external",
						FETCHNAME(ITABFETCH(opcode)));
				if (!ISBYTE(argtype) && !jxxxJUMP)
					yyerror("%s: Branch too far(%db): try -J flag",
						FETCHNAME(ITABFETCH(opcode)),
						argtype);
				break;
			}
			if (argtype == A_BW) {
				ap->a_areg1 = argtype = xp->e_xvalue
					-= dotp->e_xvalue + 2;
				if (xp->e_xtype & XXTRN)
					yywarning("%s: destination label is external",
						FETCHNAME(ITABFETCH(opcode)));
				xp->e_xtype = XABS;
				if (!ISWORD(argtype) && !jxxxJUMP)
					yyerror("%s: Branch too far(%db): try -J flag",
						FETCHNAME(ITABFETCH(opcode)),
						argtype);
				reloc_how = TYPB;
				ap->a_areg1 = argtype>>8;
				xp->e_xvalue = argtype;
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
				ap->a_areg1 |= 0x8F;
			} else {
				argtype = fetcharg(ITABFETCH(opcode), i);
				if (argtype&ACCA)
					argtype = TYPL;
				else
					argtype &= TYPMASK;
				if (    ( (xp->e_xtype&XTYPE) == XABS) 
				    && !(xp->e_xtype&XFORW)
				    &&  (argtype != TYPF)
				    &&  (argtype != TYPD)
 				  )
				{
				    if  ((xp->e_xvalue <= 63)
				    &&  (xp->e_xvalue >= 0))
				     {
					ap->a_areg1 = xp->e_xvalue;
					break;
				     }	
			else
				if (ISBYTE (xp->e_xvalue))
				{
					ap->a_areg1 = 0x88;
					argtype = TYPB;
				}
				else
					if (ISWORD (xp->e_xvalue))
					{
						ap->a_areg1 = 0x89;
						argtype = TYPW;
			 		}
					else
					{
						ap->a_areg1 = 0x8F;
						argtype = TYPL;
			 		}
			}
			else
			{
				ap->a_areg1 = 0x8F;
				argtype = TYPL;
			 }
			}
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
		Outb(ap->a_areg1);
		if ((passno == 2) && liston)
			byte_out (ap->a_areg1);
		if (reloc_how != TYPNONE) 
			outrel(xp, reloc_how);
		if ((passno == 2) && liston)
			*layoutpos++ = ' ';
	}	/*end of the for to pick up all arguments*/
}
