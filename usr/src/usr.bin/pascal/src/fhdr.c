/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)fhdr.c 1.5 %G%";

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
	struct nl *cp, *dp;
	int s, o, *pp;

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
		 * check that forwards are of the right kind:
		 *     if this fails, we are trying to redefine it
		 *     and enter() will complain.
		 */
		if (  ( ( p->nl_flags & NFORWD ) != 0 )
		   && (  ( p->class == FUNC && r[0] == T_FDEC )
		      || ( p->class == PROC && r[0] == T_PDEC ) ) ) {
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
			p -> extra_flags |= NGLOBAL;
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
			p -> extra_flags |= NGLOBAL;
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
				(int)-leven(roundup(
			            (int)(DPOFF1+lwidth(p->type)),
				    (long)align(p->type))));
			    cp -> extra_flags |= NLOCAL;
#			endif PC
			cp->chain = p;
			p->ptr[NL_FVAR] = cp;
		}
		/*
		 * Enter the parameters
		 * and compute total size
		 */
	        p->value[NL_OFFS] = params(p, r[3]);
		/*
		 * because NL_LINENO field in the function 
		 * namelist entry has been used (as have all
		 * the other fields), the line number is
		 * stored in the NL_LINENO field of its fvar.
		 */
		if (p->class == FUNC)
		    p->ptr[NL_FVAR]->value[NL_LINENO] = r[1];
		else
		    p->value[NL_LINENO] = r[1];
		cbn--;
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
	p->value[NL_ENTLOC] = getlab();
	if (monflg) {
		bodycnts[ cbn ] = getcnt();
		p->value[ NL_CNTR ] = 0;
	}
#	ifdef OBJ
	    put(2, O_TRA4, (long)p->value[NL_ENTLOC]);
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

	/*
	 * deal with the parameter declaration for a routine.
	 * p is the namelist entry of the routine.
	 * formalist is the parse tree for the parameter declaration.
	 * formalist	[0]	T_LISTPP
	 *		[1]	pointer to a formal
	 *		[2]	pointer to next formal
	 * for by-value or by-reference formals, the formal is
	 * formal	[0]	T_PVAL or T_PVAR
	 *		[1]	pointer to id_list
	 *		[2]	pointer to type (error if not typeid)
	 * for function and procedure formals, the formal is
	 * formal	[0]	T_PFUNC or T_PPROC
	 *		[1]	pointer to id_list (error if more than one)
	 *		[2]	pointer to type (error if not typeid, or proc)
	 *		[3]	pointer to formalist for this routine.
	 */
fparams(p, formal)
	register struct nl *p;
	int *formal;
{
	params(p, formal[3]);
	p -> value[ NL_LINENO ] = formal[4];
	p -> ptr[ NL_FCHAIN ] = p -> chain;
	p -> chain = NIL;
}

params(p, formalist)
	register struct nl *p;
	int *formalist;
{
	struct nl *chainp, *savedp;
	struct nl *dp;
	register int **formalp;		/* an element of the formal list */
	register int *formal;		/* a formal */
	int *typ, *idlist;
	int w, o;

	/*
	 * Enter the parameters
	 * and compute total size
	 */
	chainp = savedp = p;

#	ifdef OBJ
	    o = 0;
#	endif OBJ
#	ifdef PC
		/*
		 * parameters used to be allocated backwards,
		 * then fixed.  for pc, they are allocated correctly.
		 * also, they are aligned.
		 */
	    o = DPOFF2;
#	endif PC
	for (formalp = formalist; formalp != NIL; formalp = formalp[2]) {
		p = NIL;
		formal = formalp[1];
		if (formal == NIL)
			continue;
		/*
		 * Parametric procedures
		 * don't have types !?!
		 */
		typ = formal[2];
		if ( typ == NIL ) {
		    if ( formal[0] != T_PPROC ) {
			error("Types must be specified for arguments");
			p = NIL;
		    }
		} else {
		    if ( formal[0] == T_PPROC ) {
			error("Procedures cannot have types");
			p = NIL;
		    } else {
			if (typ[0] != T_TYID) {
				error("Types for arguments can be specified only by using type identifiers");
				p = NIL;
			} else {
				p = gtype(typ);
			}
		    }
		}
		for (idlist = formal[1]; idlist != NIL; idlist = idlist[2]) {
			switch (formal[0]) {
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
#				    ifdef OBJ
					w = lwidth(p);
					o -= even(w);
#					ifdef DEC11
					    dp = defnl(idlist[1], VAR, p, o);
#					else
					    dp = defnl(idlist[1], VAR, p,
						(w < 2) ? o + 1 : o);
#					endif DEC11
#				    endif OBJ
#				    ifdef PC
					dp = defnl( idlist[1] , VAR , p 
						, o = roundup( o , (long)A_STACK ) );
					o += lwidth( p );
#				    endif PC
				    dp->nl_flags |= NMOD;
				    break;
			    case T_PVAR:
#				    ifdef OBJ
					dp = defnl(idlist[1], REF, p, o -= sizeof ( int * ) );
#				    endif OBJ
#				    ifdef PC
					dp = defnl( idlist[1] , REF , p
						, o = roundup( o , (long)A_STACK ) );
					o += sizeof(char *);
#				    endif PC
				    break;
			    case T_PFUNC:
				    if (idlist[2] != NIL) {
					error("Each function argument must be declared separately");
					idlist[2] = NIL;
				    }
#				    ifdef OBJ
					dp = defnl(idlist[1], FFUNC, p, o -= sizeof ( int * ) );
#				    endif OBJ
#				    ifdef PC
					dp = defnl( idlist[1] , FFUNC , p
						, o = roundup( o , (long)A_STACK ) );
					o += sizeof(char *);
#				    endif PC
				    dp -> nl_flags |= NMOD;
				    fparams(dp, formal);
				    break;
			    case T_PPROC:
				    if (idlist[2] != NIL) {
					error("Each procedure argument must be declared separately");
					idlist[2] = NIL;
				    }
#				    ifdef OBJ
					dp = defnl(idlist[1], FPROC, p, o -= sizeof ( int * ) );
#				    endif OBJ
#				    ifdef PC
					dp = defnl( idlist[1] , FPROC , p
						, o = roundup( o , (long)A_STACK ) );
					o += sizeof(char *);
#				    endif PC
				    dp -> nl_flags |= NMOD;
				    fparams(dp, formal);
				    break;
			    }
			if (dp != NIL) {
#				ifdef PC
				    dp -> extra_flags |= NPARAM;
#				endif PC
				chainp->chain = dp;
				chainp = dp;
			}
		}
	}
	p = savedp;
#	ifdef OBJ
		/*
		 * Correct the naivete (naivety)
		 * of our above code to
		 * calculate offsets
		 */
	    for (dp = p->chain; dp != NIL; dp = dp->chain)
		    dp->value[NL_OFFS] += -o + DPOFF2;
	    return (-o + DPOFF2);
#	endif OBJ
#	ifdef PC
	    return roundup( o , (long)A_STACK );
#	endif PC
}
