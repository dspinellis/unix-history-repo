/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)fdec.c 1.20 6/1/81";

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
			fp->extra_flags |= NEXTERN;
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
	struct nl	*functemp;

	cbn++;
	if (cbn >= DSPLYSZ) {
		error("Too many levels of function/procedure nesting");
		pexit(ERRS);
	}
	sizes[cbn].om_max = sizes[cbn].curtmps.om_off = -DPOFF1;
	sizes[cbn].reg_max = -1;
	sizes[cbn].curtmps.reg_off = 0;
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
#			ifdef PC
			    q -> extra_flags |= NPARAM;
#			endif PC
		}
	}
	if (fp->class == FUNC) {
		/*
		 * For functions, enter the fvar
		 */
		enter(fp->ptr[NL_FVAR]);
#		ifdef PC
		    q = fp -> ptr[ NL_FVAR ];
		    if (q -> type != NIL ) {
			functemp = tmpalloc(
					leven(
					    roundup(
						(int)lwidth(q -> type),
						(long)align(q -> type))),
					q -> type, NOREG);
			if ( q -> ptr[NL_OFFS] != functemp->value[NL_OFFS] )
			    panic("func var");
		    }
		    q -> extra_flags |= functemp -> extra_flags;
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
					if ((p->nl_flags & NFORWD) &&
					    ((p->extra_flags & NEXTERN) == 0))
						nerror("Unresolved forward declaration of %s %s", classes[p->class], p->symbol);
					break;

				case FVAR:
					if (((p->nl_flags & NMOD) == 0) &&
					    ((p->chain->extra_flags & NEXTERN) == 0))
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
	sizes[cbn].om_max = sizes[cbn].curtmps.om_off = -DPOFF1;
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
