/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)fhdr.c 1.1 %G%";

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
