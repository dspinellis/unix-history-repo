/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)fdec.c 1.13 1/24/81";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "align.h"

/*
 * this array keeps the pxp counters associated with
 * functions and procedures, so that they can be output
 * when their bodies are encountered
 */
int	bodycnts[ DSPLYSZ ];

#ifdef PC
#   include "pc.h"
#   include "pcops.h"
#endif PC

#ifdef OBJ
int	cntpatch;
int	nfppatch;
#endif OBJ

/*
 * Funchdr inserts
 * declaration of a the
 * prog/proc/func into the
 * namelist. It also handles
 * the arguments and puts out
 * a transfer which defines
 * the entry point of a procedure.
 */

struct nl *
funchdr(r)
	int *r;
{
	register struct nl *p;
	register *il, **rl;
	int *rll;
	struct nl *cp, *dp, *sp;
	int w, s, o, *pp;

	if (inpflist(r[2])) {
		opush('l');
		yyretrieve();	/* kludge */
	}
	pfcnt++;
	parts[ cbn ] |= RPRT;
	line = r[1];
	if (r[3] == NIL && (p=lookup1(r[2])) != NIL && bn == cbn) {
		/*
		 * Symbol already defined
		 * in this block. it is either
		 * a redeclared symbol (error)
		 * a forward declaration,
		 * or an external declaration.
		 */
		if ((p->class == FUNC || p->class == PROC) && (p->nl_flags & NFORWD) != 0) {
			/*
			 * Grammar doesnt forbid
			 * types on a resolution
			 * of a forward function
			 * declaration.
			 */
			if (p->class == FUNC && r[4])
				error("Function type should be given only in forward declaration");
			/*
			 * get another counter for the actual
			 */
			if ( monflg ) {
			    bodycnts[ cbn ] = getcnt();
			}
#			ifdef PC
			    enclosing[ cbn ] = p -> symbol;
#			endif PC
#			ifdef PTREE
				/*
				 *	mark this proc/func as forward
				 *	in the pTree.
				 */
			    pDEF( p -> inTree ).PorFForward = TRUE;
#			endif PTREE
			return (p);
		}
	}

	/* if a routine segment is being compiled,
	 * do level one processing.
	 */

	 if ((r[0] != T_PROG) && (!progseen))
		level1();


	/*
	 * Declare the prog/proc/func
	 */
	switch (r[0]) {
	    case T_PROG:
		    progseen = TRUE;
		    if (opt('z'))
			    monflg = TRUE;
		    program = p = defnl(r[2], PROG, 0, 0);
		    p->value[3] = r[1];
		    break;
	    case T_PDEC:
		    if (r[4] != NIL)
			    error("Procedures do not have types, only functions do");
		    p = enter(defnl(r[2], PROC, 0, 0));
		    p->nl_flags |= NMOD;
#		    ifdef PC
			enclosing[ cbn ] = r[2];
#		    endif PC
		    break;
	    case T_FDEC:
		    il = r[4];
		    if (il == NIL)
			    error("Function type must be specified");
		    else if (il[0] != T_TYID) {
			    il = NIL;
			    error("Function type can be specified only by using a type identifier");
		    } else
			    il = gtype(il);
		    p = enter(defnl(r[2], FUNC, il, NIL));
		    p->nl_flags |= NMOD;
		    /*
		     * An arbitrary restriction
		     */
		    switch (o = classify(p->type)) {
			    case TFILE:
			    case TARY:
			    case TREC:
			    case TSET:
			    case TSTR:
				    warning();
				    if (opt('s')) {
					    standard();
				    }
				    error("Functions should not return %ss", clnames[o]);
		    }
#		    ifdef PC
			enclosing[ cbn ] = r[2];
#		    endif PC
		    break;
	    default:
		    panic("funchdr");
	}
	if (r[0] != T_PROG) {
		/*
		 * Mark this proc/func as
		 * being forward declared
		 */
		p->nl_flags |= NFORWD;
		/*
		 * Enter the parameters
		 * in the next block for
		 * the time being
		 */
		if (++cbn >= DSPLYSZ) {
			error("Procedure/function nesting too deep");
			pexit(ERRS);
		}
		/*
		 * For functions, the function variable
		 */
		if (p->class == FUNC) {
#			ifdef OBJ
			    cp = defnl(r[2], FVAR, p->type, 0);
#			endif OBJ
#			ifdef PC
				/*
				 * fvars used to be allocated and deallocated
				 * by the caller right before the arguments.
				 * the offset of the fvar was kept in
				 * value[NL_OFFS] of function (very wierd,
				 * but see asgnop).
				 * now, they are locals to the function
				 * with the offset kept in the fvar.
				 */

			    cp = defnl(r[2], FVAR, p->type,
				    -(roundup((int)(DPOFF1+lwidth(p->type)),
					(long)align(p->type))));
#			endif PC
			cp->chain = p;
			p->ptr[NL_FVAR] = cp;
		}
		/*
		 * Enter the parameters
		 * and compute total size
		 */
		cp = sp = p;

#		ifdef OBJ
		    o = 0;
#		endif OBJ
#		ifdef PC
			/*
			 * parameters used to be allocated backwards,
			 * then fixed.  for pc, they are allocated correctly.
			 * also, they are aligned.
			 */
		o = DPOFF2;
#		endif PC
		for (rl = r[3]; rl != NIL; rl = rl[2]) {
			p = NIL;
			if (rl[1] == NIL)
				continue;
			/*
			 * Parametric procedures
			 * don't have types !?!
			 */
			if (rl[1][0] != T_PPROC) {
				rll = rl[1][2];
				if (rll[0] != T_TYID) {
					error("Types for arguments can be specified only by using type identifiers");
					p = NIL;
				} else
					p = gtype(rll);
			}
			for (il = rl[1][1]; il != NIL; il = il[2]) {
				switch (rl[1][0]) {
				    default:
					    panic("funchdr2");
				    case T_PVAL:
					    if (p != NIL) {
						    if (p->class == FILET)
							    error("Files cannot be passed by value");
						    else if (p->nl_flags & NFILES)
							    error("Files cannot be a component of %ss passed by value",
								    nameof(p));
					    }
#					    ifdef OBJ
						w = width(p);
						o -= even(w);
#						ifdef DEC11
						    dp = defnl(il[1], VAR, p, o);
#						else
						    dp = defnl(il[1], VAR, p,
							(w < 2) ? o + 1 : o);
#						endif DEC11
#					    endif OBJ
#					    ifdef PC
						dp = defnl( il[1] , VAR , p 
							, o = roundup( o , (long)A_STACK ) );
						o += width( p );
#					    endif PC
					    dp->nl_flags |= NMOD;
					    break;
				    case T_PVAR:
#					    ifdef OBJ
						dp = defnl(il[1], REF, p, o -= sizeof ( int * ) );
#					    endif OBJ
#					    ifdef PC
						dp = defnl( il[1] , REF , p
							, o = roundup( o , (long)A_STACK ) );
						o += sizeof(char *);
#					    endif PC
					    break;
				    case T_PFUNC:
#					    ifdef OBJ
						dp = defnl(il[1], FFUNC, p, o -= sizeof ( int * ) );
#					    endif OBJ
#					    ifdef PC
						dp = defnl( il[1] , FFUNC , p
							, o = roundup( o , (long)A_STACK ) );
						o += sizeof(char *);
#					    endif PC
					    dp -> nl_flags |= NMOD;
					    break;
				    case T_PPROC:
#					    ifdef OBJ
						dp = defnl(il[1], FPROC, p, o -= sizeof ( int * ) );
#					    endif OBJ
#					    ifdef PC
						dp = defnl( il[1] , FPROC , p
							, o = roundup( o , (long)A_STACK ) );
						o += sizeof(char *);
#					    endif PC
					    dp -> nl_flags |= NMOD;
					    break;
				    }
				if (dp != NIL) {
					cp->chain = dp;
					cp = dp;
				}
			}
		}
		cbn--;
		p = sp;
#		ifdef OBJ
		    p->value[NL_OFFS] = -o+DPOFF2;
			/*
			 * Correct the naivete (naievity)
			 * of our above code to
			 * calculate offsets
			 */
		    for (il = p->chain; il != NIL; il = il->chain)
			    il->value[NL_OFFS] += p->value[NL_OFFS];
#		endif OBJ
#		ifdef PC
		    p -> value[ NL_OFFS ] = roundup( o , (long)A_STACK );
#		endif PC
	} else { 
		/*
		 * The wonderful
		 * program statement!
		 */
#		ifdef OBJ
		    if (monflg) {
			    put(1, O_PXPBUF);
			    cntpatch = put(2, O_CASE4, (long)0);
			    nfppatch = put(2, O_CASE4, (long)0);
		    }
#		endif OBJ
		cp = p;
		for (rl = r[3]; rl; rl = rl[2]) {
			if (rl[1] == NIL)
				continue;
			dp = defnl(rl[1], VAR, 0, 0);
			cp->chain = dp;
			cp = dp;
		}
	}
	/*
	 * Define a branch at
	 * the "entry point" of
	 * the prog/proc/func.
	 */
	p->entloc = getlab();
	if (monflg) {
		bodycnts[ cbn ] = getcnt();
		p->value[ NL_CNTR ] = 0;
	}
#	ifdef OBJ
	    put(2, O_TRA4, (long)p->entloc);
#	endif OBJ
#	ifdef PTREE
	    {
		pPointer	PF = tCopy( r );

		pSeize( PorFHeader[ nesting ] );
		if ( r[0] != T_PROG ) {
			pPointer	*PFs;

			PFs = &( pDEF( PorFHeader[ nesting ] ).PorFPFs );
			*PFs = ListAppend( *PFs , PF );
		} else {
			pDEF( PorFHeader[ nesting ] ).GlobProg = PF;
		}
		pRelease( PorFHeader[ nesting ] );
	    }
#	endif PTREE
	return (p);
}

funcfwd(fp)
	struct nl *fp;
{

	    /*
	     *	save the counter for this function
	     */
	if ( monflg ) {
	    fp -> value[ NL_CNTR ] = bodycnts[ cbn ];
	}
	return (fp);
}

/*
 * Funcext marks the procedure or
 * function external in the symbol
 * table. Funcext should only be
 * called if PC, and is an error
 * otherwise.
 */

funcext(fp)
	struct nl *fp;
{

#ifdef PC
 	if (opt('s')) {
		standard();
		error("External procedures and functions are not standard");
	} else {
		if (cbn == 1) {
			fp->ext_flags |= NEXTERN;
			stabefunc( fp -> symbol , fp -> class , line );
		}
		else
			error("External procedures and functions can only be declared at the outermost level.");
	}
#endif PC
#ifdef OBJ
	error("Procedures or functions cannot be declared external.");
#endif OBJ

	return(fp);
}

/*
 * Funcbody is called
 * when the actual (resolved)
 * declaration of a procedure is
 * encountered. It puts the names
 * of the (function) and parameters
 * into the symbol table.
 */
funcbody(fp)
	struct nl *fp;
{
	register struct nl *q, *p;

	cbn++;
	if (cbn >= DSPLYSZ) {
		error("Too many levels of function/procedure nesting");
		pexit(ERRS);
	}
	sizes[cbn].om_max = sizes[cbn].om_off = -DPOFF1;
	gotos[cbn] = NIL;
	errcnt[cbn] = syneflg;
	parts[ cbn ] = NIL;
	dfiles[ cbn ] = FALSE;
	if (fp == NIL)
		return (NIL);
	/*
	 * Save the virtual name
	 * list stack pointer so
	 * the space can be freed
	 * later (funcend).
	 */
	fp->ptr[2] = nlp;
	if (fp->class != PROG) {
		for (q = fp->chain; q != NIL; q = q->chain) {
			enter(q);
		}
	}
	if (fp->class == FUNC) {
		/*
		 * For functions, enter the fvar
		 */
		enter(fp->ptr[NL_FVAR]);
#		ifdef PC
		    q = fp -> ptr[ NL_FVAR ];
		    sizes[cbn].om_off -= lwidth( q -> type );
		    sizes[cbn].om_max = sizes[cbn].om_off;
#		endif PC
	}
#	ifdef PTREE
		/*
		 *	pick up the pointer to porf declaration
		 */
	    PorFHeader[ ++nesting ] = fp -> inTree;
#	endif PTREE
	return (fp);
}

struct	nl *Fp;
int	pnumcnt;
/*
 * Funcend is called to
 * finish a block by generating
 * the code for the statements.
 * It then looks for unresolved declarations
 * of labels, procedures and functions,
 * and cleans up the name list.
 * For the program, it checks the
 * semantics of the program
 * statement (yuchh).
 */
funcend(fp, bundle, endline)
	struct nl *fp;
	int *bundle;
	int endline;
{
	register struct nl *p;
	register int i, b;
	int var, inp, out, *blk;
	bool chkref;
	struct nl *iop;
	char *cp;
	extern int cntstat;
#	ifdef PC
	    int	toplabel = getlab();
	    int	botlabel = getlab();
#	endif PC

	cntstat = 0;
/*
 *	yyoutline();
 */
	if (program != NIL)
		line = program->value[3];
	blk = bundle[2];
	if (fp == NIL) {
		cbn--;
#		ifdef PTREE
		    nesting--;
#		endif PTREE
		return;
	}
#ifdef OBJ
	/*
	 * Patch the branch to the
	 * entry point of the function
	 */
	patch4(fp->entloc);
	/*
	 * Put out the block entrance code and the block name.
	 * HDRSZE is the number of bytes of info in the static
	 * BEG data area exclusive of the proc name. It is
	 * currently defined as:
	/*	struct hdr {
	/*		long framesze;	/* number of bytes of local vars */
	/*		long nargs;	/* number of bytes of arguments */
	/*		bool tests;	/* TRUE => perform runtime tests */
	/*		short offset;	/* offset of procedure in source file */
	/*		char name[1];	/* name of active procedure */
	/*	};
	 */
#	define HDRSZE (2 * sizeof(long) + sizeof(short) + sizeof(bool))
	var = put(2, ((lenstr(fp->symbol,0) + HDRSZE) << 8)
		| (cbn == 1 && opt('p') == 0 ? O_NODUMP: O_BEG), (long)0);
	    /*
	     *  output the number of bytes of arguments
	     *  this is only checked on formal calls.
	     */
	put(2, O_CASE4, cbn == 1 ? (long)0 : (long)(fp->value[NL_OFFS]-DPOFF2));
	    /*
	     *	Output the runtime test mode for the routine
	     */
	put(2, sizeof(bool) == 2 ? O_CASE2 : O_CASE4, opt('t') ? TRUE : FALSE);
	    /*
	     *	Output line number and routine name
	     */
	put(2, O_CASE2, bundle[1]);
	putstr(fp->symbol, 0);
#endif OBJ
#ifdef PC
	/*
	 * put out the procedure entry code
	 */
	if ( fp -> class == PROG ) {
	    putprintf( "	.text" , 0 );
	    putprintf( "	.align	1" , 0 );
	    putprintf( "	.globl	_main" , 0 );
	    putprintf( "_main:" , 0 );
	    putprintf( "	.word	0" , 0 );
	    putprintf( "	calls	$0,_PCSTART" , 0 );
	    putprintf( "	movl	4(ap),__argc" , 0 );
	    putprintf( "	movl	8(ap),__argv" , 0 );
	    putprintf( "	calls	$0,_program" , 0 );
	    putprintf( "	calls	$0,_PCEXIT" , 0 );
	    ftnno = fp -> entloc;
	    putprintf( "	.text" , 0 );
	    putprintf( "	.align	1" , 0 );
	    putprintf( "	.globl	_program" , 0 );
	    putprintf( "_program:" , 0 );
	    stabfunc( "program" , fp -> class , bundle[1] , 0 );
	} else {
	    ftnno = fp -> entloc;
	    putprintf( "	.text" , 0 );
	    putprintf( "	.align	1" , 0 );
	    putprintf( "	.globl	" , 1 );
	    for ( i = 1 ; i < cbn ; i++ ) {
		putprintf( EXTFORMAT , 1 , enclosing[ i ] );
	    }
	    putprintf( "" , 0 );
	    for ( i = 1 ; i < cbn ; i++ ) {
		putprintf( EXTFORMAT , 1 , enclosing[ i ] );
	    }
	    putprintf( ":" , 0 );
	    stabfunc( fp -> symbol , fp -> class , bundle[1] , cbn - 1 );
	    for ( p = fp -> chain ; p != NIL ; p = p -> chain ) {
		stabparam( p -> symbol , p2type( p -> type )
			    , p -> value[ NL_OFFS ] , lwidth( p -> type ) );
	    }
	    if ( fp -> class == FUNC ) {
		    /*
		     *	stab the function variable
		     */
		p = fp -> ptr[ NL_FVAR ];
		stablvar( p -> symbol , p2type( p -> type ) , cbn 
			, p -> value[ NL_OFFS ] , lwidth( p -> type ) );
	    }
		/*
		 *	stab local variables
		 *	rummage down hash chain links.
		 */
	    for ( i = 0 ; i <= 077 ; i++ ) {
		for ( p = disptab[ i ] ; p != NIL ; p = p->nl_next) {
		    if ( ( p -> nl_block & 037 ) != cbn ) {
			break;
		    }
		    /*
		     *	stab local variables
		     *	that's named variables, but not params
		     */
		    if (   ( p -> symbol != NIL ) 
			&& ( p -> class == VAR )
			&& ( p -> value[ NL_OFFS ] < 0 ) ) {
			stablvar( p -> symbol , p2type( p -> type ) , cbn 
			    , p -> value[ NL_OFFS ] , lwidth( p -> type ) );
		    }
		}
	    }
	}
	stablbrac( cbn );
	    /*
	     *	register save mask
	     */
	if ( opt( 't' ) ) {
	    putprintf( "	.word	0x%x" , 0 , RUNCHECK | RSAVEMASK );
	} else {
	    putprintf( "	.word	0x%x" , 0 , RSAVEMASK );
	}
	putjbr( botlabel );
	putlab( toplabel );
	if ( profflag ) {
		/*
		 *	call mcount for profiling
		 */
	    putprintf( "	moval	1f,r0" , 0 );
	    putprintf( "	jsb	mcount" , 0 );
	    putprintf( "	.data" , 0 );
	    putprintf( "	.align	2" , 0 );
	    putprintf( "1:" , 0 );
	    putprintf( "	.long	0" , 0 );
	    putprintf( "	.text" , 0 );
	}
	    /*
	     *	set up unwind exception vector.
	     */
	putprintf( "	moval	%s,%d(%s)" , 0
		, UNWINDNAME , UNWINDOFFSET , P2FPNAME );
	    /*
	     *	save address of display entry, for unwind.
	     */
	putprintf( "	moval	%s+%d,%d(%s)" , 0
		, DISPLAYNAME , cbn * sizeof(struct dispsave)
		, DPTROFFSET , P2FPNAME );
	    /*
	     *	save old display 
	     */
	putprintf( "	movq	%s+%d,%d(%s)" , 0
		, DISPLAYNAME , cbn * sizeof(struct dispsave)
		, DSAVEOFFSET , P2FPNAME );
	    /*
	     *	set up new display by saving AP and FP in appropriate
	     *	slot in display structure.
	     */
	putprintf( "	movq	%s,%s+%d" , 0
		, P2APNAME , DISPLAYNAME , cbn * sizeof(struct dispsave) );
	    /*
	     *	ask second pass to allocate known locals
	     */
	putlbracket( ftnno , -sizes[ cbn ].om_max );
	    /*
	     *	and zero them if checking is on
	     *	by calling blkclr( bytes of locals , starting local address );
	     */
	if ( opt( 't' ) ) {
	    if ( ( -sizes[ cbn ].om_max ) > DPOFF1 ) {
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_blkclr" );
		putleaf( P2ICON ,  ( -sizes[ cbn ].om_max ) - DPOFF1
			, 0 , P2INT , 0 );
		putLV( 0 , cbn , sizes[ cbn ].om_max , P2CHAR );
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putdot( filename , line );
	    }
		/*
		 *  check number of longs of arguments
		 *  this can only be wrong for formal calls.
		 */
	    if ( fp -> class != PROG ) {
		    putleaf( P2ICON , 0 , 0 , ADDTYPE( P2PTR , P2FTN | P2INT ) ,
			    "_NARGCHK" );
		    putleaf( P2ICON ,
			(fp->value[NL_OFFS] - DPOFF2) / sizeof(long) ,
			0 , P2INT , 0 );
		    putop( P2CALL , P2INT );
		    putdot( filename , line );
	    }
	}
#endif PC
	if ( monflg ) {
		if ( fp -> value[ NL_CNTR ] != 0 ) {
			inccnt( fp -> value [ NL_CNTR ] );
		}
		inccnt( bodycnts[ fp -> nl_block & 037 ] );
	}
	if (fp->class == PROG) {
		/*
		 * The glorious buffers option.
		 *          0 = don't buffer output
		 *          1 = line buffer output
		 *          2 = 512 byte buffer output
		 */
#		ifdef OBJ
		    if (opt('b') != 1)
			    put(1, O_BUFF | opt('b') << 8);
#		endif OBJ
#		ifdef PC
		    if ( opt( 'b' ) != 1 ) {
			putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR ) , "_BUFF" );
			putleaf( P2ICON , opt( 'b' ) , 0 , P2INT , 0 );
			putop( P2CALL , P2INT );
			putdot( filename , line );
		    }
#		endif PC
		out = 0;
		for (p = fp->chain; p != NIL; p = p->chain) {
			if (strcmp(p->symbol, "input") == 0) {
				inp++;
				continue;
			}
			if (strcmp(p->symbol, "output") == 0) {
				out++;
				continue;
			}
			iop = lookup1(p->symbol);
			if (iop == NIL || bn != cbn) {
				error("File %s listed in program statement but not declared", p->symbol);
				continue;
			}
			if (iop->class != VAR) {
				error("File %s listed in program statement but declared as a %s", p->symbol, classes[iop->class]);
				continue;
			}
			if (iop->type == NIL)
				continue;
			if (iop->type->class != FILET) {
				error("File %s listed in program statement but defined as %s",
					p->symbol, nameof(iop->type));
				continue;
			}
#			ifdef OBJ
			    put(2, O_CON24, text(iop->type) ? 0 : width(iop->type->type));
			    i = lenstr(p->symbol,0);
			    put(2, O_CON24, i);
			    put(2, O_LVCON, i);
			    putstr(p->symbol, 0);
			    put(2, O_LV | bn<<8+INDX, (int)iop->value[NL_OFFS]);
			    put(1, O_DEFNAME);
#			endif OBJ
#			ifdef PC
			    putleaf( P2ICON , 0 , 0
				    , ADDTYPE( P2FTN | P2INT , P2PTR )
				    , "_DEFNAME" );
			    putLV( p -> symbol , bn , iop -> value[NL_OFFS]
				    , p2type( iop ) );
			    putCONG( p -> symbol , strlen( p -> symbol )
				    , LREQ );
			    putop( P2LISTOP , P2INT );
			    putleaf( P2ICON , strlen( p -> symbol )
				    , 0 , P2INT , 0 );
			    putop( P2LISTOP , P2INT );
			    putleaf( P2ICON
				, text(iop->type) ? 0 : width(iop->type->type)
				, 0 , P2INT , 0 );
			    putop( P2LISTOP , P2INT );
			    putop( P2CALL , P2INT );
			    putdot( filename , line );
#			endif PC
		}
		if (out == 0 && fp->chain != NIL) {
			recovered();
			error("The file output must appear in the program statement file list");
		}
	}
	/*
	 * Process the prog/proc/func body
	 */
	noreach = 0;
	line = bundle[1];
	statlist(blk);
#	ifdef PTREE
	    {
		pPointer Body = tCopy( blk );

		pDEF( PorFHeader[ nesting -- ] ).PorFBody = Body;
	    }
#	endif PTREE
#	ifdef OBJ
	    if (cbn== 1 && monflg != 0) {
		    patchfil(cntpatch - 2, (long)cnts, 2);
		    patchfil(nfppatch - 2, (long)pfcnt, 2);
	    }
#	endif OBJ
#	ifdef PC
	    if ( fp -> class == PROG && monflg ) {
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_PMFLUSH" );
		putleaf( P2ICON , cnts , 0 , P2INT , 0 );
		putleaf( P2ICON , pfcnt , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putLV( PCPCOUNT , 0 , 0 , P2INT );
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putdot( filename , line );
	    }
#	endif PC
	if (fp->class == PROG && inp == 0 && (input->nl_flags & (NUSED|NMOD)) != 0) {
		recovered();
		error("Input is used but not defined in the program statement");
	}
	/*
	 * Clean up the symbol table displays and check for unresolves
	 */
	line = endline;
	b = cbn;
	Fp = fp;
	chkref = syneflg == errcnt[cbn] && opt('w') == 0;
	for (i = 0; i <= 077; i++) {
		for (p = disptab[i]; p != NIL && (p->nl_block & 037) == b; p = p->nl_next) {
			/*
			 * Check for variables defined
			 * but not referenced 
			 */
			if (chkref && p->symbol != NIL)
			switch (p->class) {
				case FIELD:
					/*
					 * If the corresponding record is
					 * unused, we shouldn't complain about
					 * the fields.
					 */
				default:
					if ((p->nl_flags & (NUSED|NMOD)) == 0) {
						warning();
						nerror("%s %s is neither used nor set", classes[p->class], p->symbol);
						break;
					}
					/*
					 * If a var parameter is either
					 * modified or used that is enough.
					 */
					if (p->class == REF)
						continue;
#					ifdef OBJ
					    if ((p->nl_flags & NUSED) == 0) {
						warning();
						nerror("%s %s is never used", classes[p->class], p->symbol);
						break;
					    }
#					endif OBJ
#					ifdef PC
					    if (((p->nl_flags & NUSED) == 0) && ((p->ext_flags & NEXTERN) == 0)) {
						warning();
						nerror("%s %s is never used", classes[p->class], p->symbol);
						break;
					    }
#					endif PC
					if ((p->nl_flags & NMOD) == 0) {
						warning();
						nerror("%s %s is used but never set", classes[p->class], p->symbol);
						break;
					}
				case LABEL:
				case FVAR:
				case BADUSE:
					break;
			}
			switch (p->class) {
				case BADUSE:
					cp = "s";
					if (p->chain->ud_next == NIL)
						cp++;
					eholdnl();
					if (p->value[NL_KINDS] & ISUNDEF)
						nerror("%s undefined on line%s", p->symbol, cp);
					else
						nerror("%s improperly used on line%s", p->symbol, cp);
					pnumcnt = 10;
					pnums(p->chain);
					pchr('\n');
					break;

				case FUNC:
				case PROC:
#					ifdef OBJ
					    if ((p->nl_flags & NFORWD))
						nerror("Unresolved forward declaration of %s %s", classes[p->class], p->symbol);
#					endif OBJ
#					ifdef PC
					    if ((p->nl_flags & NFORWD) && ((p->ext_flags & NEXTERN) == 0))
						nerror("Unresolved forward declaration of %s %s", classes[p->class], p->symbol);
#					endif PC
					break;

				case LABEL:
					if (p->nl_flags & NFORWD)
						nerror("label %s was declared but not defined", p->symbol);
					break;
				case FVAR:
					if ((p->nl_flags & NMOD) == 0)
						nerror("No assignment to the function variable");
					break;
			}
		}
		/*
		 * Pop this symbol
		 * table slot
		 */
		disptab[i] = p;
	}

#	ifdef OBJ
	    put(1, O_END);
#	endif OBJ
#	ifdef PC
		/*
		 *	if there were file variables declared at this level
		 *	call pclose( &__disply[ cbn ] ) to clean them up.
		 */
	    if ( dfiles[ cbn ] ) {
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_PCLOSE" );
		putRV( DISPLAYNAME , 0 , cbn * sizeof( struct dispsave )
			, P2PTR | P2CHAR );
		putop( P2CALL , P2INT );
		putdot( filename , line );
	    }
		/*
		 *	if this is a function,
		 *	the function variable is the return value.
		 *	if it's a scalar valued function, return scalar,
		 *	else, return a pointer to the structure value.
		 */
	    if ( fp -> class == FUNC ) {
		struct nl	*fvar = fp -> ptr[ NL_FVAR ];
		long		fvartype = p2type( fvar -> type );
		long		label;
		char		labelname[ BUFSIZ ];

		switch ( classify( fvar -> type ) ) {
		    case TBOOL:
		    case TCHAR:
		    case TINT:
		    case TSCAL:
		    case TDOUBLE:
		    case TPTR:
			putRV( fvar -> symbol , ( fvar -> nl_block ) & 037
				, fvar -> value[ NL_OFFS ] , fvartype );
			break;
		    default:
			label = getlab();
			sprintf( labelname , PREFIXFORMAT ,
				LABELPREFIX , label );
			putprintf( "	.data" , 0 );
			putprintf( "	.lcomm	%s,%d" , 0 ,
				    labelname , lwidth( fvar -> type ) );
			putprintf( "	.text" , 0 );
			putleaf( P2NAME , 0 , 0 , fvartype , labelname );
			putLV( fvar -> symbol , ( fvar -> nl_block ) & 037
				, fvar -> value[ NL_OFFS ] , fvartype );
			putstrop( P2STASG , fvartype , lwidth( fvar -> type ) ,
				align( fvar -> type ) );
			putdot( filename , line );
			putleaf( P2ICON , 0 , 0 , fvartype , labelname );
			break;
		}
		putop( P2FORCE , fvartype );
		putdot( filename , line );
	    }
		/*
		 *	restore old display entry from save area
		 */

	    putprintf( "	movq	%d(%s),%s+%d" , 0
		, DSAVEOFFSET , P2FPNAME
		, DISPLAYNAME , cbn * sizeof(struct dispsave) );
	    stabrbrac( cbn );
	    putprintf( "	ret" , 0 );
		/*
		 *	let the second pass allocate locals
		 */
	    putlab( botlabel );
	    putprintf( "	subl2	$LF%d,sp" , 0 , ftnno );
	    putrbracket( ftnno );
	    putjbr( toplabel );
		/*
		 *	declare pcp counters, if any
		 */
	    if ( monflg && fp -> class == PROG ) {
		putprintf( "	.data" , 0 );
		putprintf( "	.comm	" , 1 );
		putprintf( PCPCOUNT , 1 );
		putprintf( ",%d" , 0 , ( cnts + 1 ) * sizeof (long) );
		putprintf( "	.text" , 0 );
	    }
#	endif PC
#ifdef DEBUG
	dumpnl(fp->ptr[2], fp->symbol);
#endif
	/*
	 * Restore the
	 * (virtual) name list
	 * position
	 */
	nlfree(fp->ptr[2]);
	/*
	 * Proc/func has been
	 * resolved
	 */
	fp->nl_flags &= ~NFORWD;
	/*
	 * Patch the beg
	 * of the proc/func to
	 * the proper variable size
	 */
	if (Fp == NIL)
		elineon();
#	ifdef OBJ
	    patchfil(var, (long)(-sizes[cbn].om_max), 2);
#	endif OBJ
	cbn--;
	if (inpflist(fp->symbol)) {
		opop('l');
	}
}


/*
 * Segend is called to check for
 * unresolved variables, funcs and
 * procs, and deliver unresolved and
 * baduse error diagnostics at the
 * end of a routine segment (a separately
 * compiled segment that is not the 
 * main program) for PC. This
 * routine should only be called
 * by PC (not standard).
 */
 segend()
 {
	register struct nl *p;
	register int i,b;
	char *cp;

#ifdef PC
	if (opt('s')) {
		standard();
		error("Separately compiled routine segments are not standard.");
	} else {
		b = cbn;
		for (i=0; i<077; i++) {
			for (p = disptab[i]; p != NIL && (p->nl_block & 037) == b; p = p->nl_next) {
			switch (p->class) {
				case BADUSE:
					cp = 's';
					if (p->chain->ud_next == NIL)
						cp++;
					eholdnl();
					if (p->value[NL_KINDS] & ISUNDEF)
						nerror("%s undefined on line%s", p->symbol, cp);
					else
						nerror("%s improperly used on line%s", p->symbol, cp);
					pnumcnt = 10;
					pnums(p->chain);
					pchr('\n');
					break;
				
				case FUNC:
				case PROC:
					if ((p->nl_flags & NFORWD) && ((p->ext_flags & NEXTERN) == 0))
						nerror("Unresolved forward declaration of %s %s", classes[p->class], p->symbol);
					break;

				case FVAR:
					if (((p->nl_flags & NMOD) == 0) && ((p->chain->ext_flags & NEXTERN) == 0))
						nerror("No assignment to the function variable");
					break;
			    }
			   }
			   disptab[i] = p;
		    }
	}
#endif PC
#ifdef OBJ
	error("Missing program statement and program body");
#endif OBJ

}


/*
 * Level1 does level one processing for
 * separately compiled routine segments
 */
level1()
{

#	ifdef OBJ
	    error("Missing program statement");
#	endif OBJ
#	ifdef PC
	    if (opt('s')) {
		    standard();
		    error("Missing program statement");
	    }
#	endif PC

	cbn++;
	sizes[cbn].om_max = sizes[cbn].om_off = -DPOFF1;
	gotos[cbn] = NIL;
	errcnt[cbn] = syneflg;
	parts[ cbn ] = NIL;
	dfiles[ cbn ] = FALSE;
	progseen = TRUE;
}



pnums(p)
	struct udinfo *p;
{

	if (p->ud_next != NIL)
		pnums(p->ud_next);
	if (pnumcnt == 0) {
		printf("\n\t");
		pnumcnt = 20;
	}
	pnumcnt--;
	printf(" %d", p->ud_line);
}

nerror(a1, a2, a3)
{

	if (Fp != NIL) {
		yySsync();
#ifndef PI1
		if (opt('l'))
			yyoutline();
#endif
		yysetfile(filename);
		printf("In %s %s:\n", classes[Fp->class], Fp->symbol);
		Fp = NIL;
		elineoff();
	}
	error(a1, a2, a3);
}
