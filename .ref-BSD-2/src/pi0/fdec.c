/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

/*
 * Funchdr inserts
 * declaration of a the
 * prog/proc/func into the
 * namelist. It also handles
 * the arguments and puts out
 * a transfer which defines
 * the entry point of a procedure.
 */

funchdr(r)
	int *r;
{
	register struct nl *p;
	register *il, **rl;
	int *rll, o;
	struct nl *cp, *dp, *sp;
	int *pp;

	send(REVFHDR, r);
	if (inpflist(r[2])) {
		opush('l');
		yyretrieve();	/* kludge */
	}
	line = r[1];
	if (r[3] == NIL && (p=lookup1(r[2])) != NIL && bn == cbn) {
		/*
		 * Symbol already defined
		 * in this block. it is either
		 * a redeclared symbol (error)
		 * or a forward declaration.
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
			return (p);
		}
	}
	/*
	 * Declare the prog/proc/func
	 */
	switch (r[0]) {
		case T_PROG:
			program = p = defnl(r[2], PROG, 0, 0);
			break;
		case T_PDEC:
			if (r[4] != NIL)
				error("Procedures do not have types, only functions do");
			p = enter(defnl(r[2], PROC, 0, 0));
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
					if (opt('s'))
						standard();
					error("Functions should not return %ss", clnames[o]);
			}
			break;
		default:
			panic("funchdr");
		}
	if (r[0] != T_PROG) {
		/*
		 * Mark this proc/func as
		 * begin forward declared
		 */
		p->nl_flags =| NFORWD;
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
			cp = defnl(r[2], FVAR, p->type, 0);
			cp->chain = p;
			p->value[NL_FVAR] = cp;
		}
		/*
		 * Enter the parameters
		 */
		cp = sp = p;
		for (rl = r[3]; rl != NIL; rl = rl[2]) {
			p = NIL;
			if (rl[1] == NIL)
				continue;
			/*
			 * Parametric procedures
			 * don't have types
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
							if (p->class == FILE)
								error("Files cannot be passed by value");
							else if (p->nl_flags & NFILES)
								error("Files cannot be a component of %ss passed by value",
									nameof(p));
						}
						dp = defnl(il[1], VAR, p, 0);
						break;
					case T_PVAR:
						dp = defnl(il[1], REF, p, 0);
						break;
					case T_PFUNC:
					case T_PPROC:
						error("Procedure/function parameters not implemented");
						continue;
					}
				if (dp != NIL) {
					cp->chain = dp;
					cp = dp;
				}
			}
		}
		cbn--;
		p = sp;
	} else { 
		cp = p;
		for (rl = r[3]; rl; rl = rl[2]) {
			if (rl[1] == NIL)
				continue;
			dp = defnl(rl[1], VAR, 0, 0);
			cp->chain = dp;
			cp = dp;
		}
	}
	return (p);
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
	send(REVFBDY);
	errcnt[cbn] = syneflg;
	parts = NIL;
	if (fp == NIL)
		return (NIL);
	/*
	 * Save the virtual name
	 * list stack pointer so
	 * the space can be freed
	 * later (funcend).
	 */
	fp->value[2] = nlp;
	if (fp->class != PROG)
		for (q = fp->chain; q != NIL; q = q->chain)
			enter(q);
	if (fp->class == FUNC) {
		/*
		 * For functions, enter the fvar
		 */
		enter(fp->value[NL_FVAR]);
	}
	return (fp);
}

int	pnumcnt;
struct	nl *Fp;
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
	int *blk;
	char *cp;

	blk = bundle[2];
	if (fp == NIL) {
		cbn--;
		return;
	}
	send(REVFEND, bundle, endline, syneflg == errcnt[cbn]);
	if (Fp != NIL)
		Fp = fp;
	/*
	 * Clean up the symbol table displays and check for unresolves
	 */
	line = endline;
	b = cbn;
	for (i = 0; i <= 077; i++) {
		for (p = disptab[i]; p != NIL && (p->nl_block & 037) == b; p = p->nl_next)
		if (p->class == BADUSE) {
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
			putchar('\n');
		}
		/*
		 * Pop this symbol
		 * table slot
		 */
		disptab[i] = p;
	}

#ifdef DEBUG
	dumpnl(fp->value[2], fp->symbol);
#endif
	/*
	 * Restore the
	 * (virtual) name list
	 * position
	 */
	nlfree(fp->value[2]);
	/*
	 * Proc/func has been
	 * resolved
	 */
	fp->nl_flags =& ~NFORWD;
	elineon();
	cbn--;
	if (inpflist(fp->symbol)) {
		opop('l');
	}
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
		printf("In %s %s:\n", classes[Fp->class], Fp->symbol);
		Fp = NIL;
	}
	elineoff();
	error(a1, a2, a3);
}
