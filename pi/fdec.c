#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"
#include "tree.h"
#include "opcode.h"

int	cntpatch;
int	nfppatch;

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
	int *rll;
	struct nl *cp, *dp, *sp;
	int o, *pp;

	if (inpflist(r[2])) {
		opush('l');
		yyretrieve();	/* kludge */
	}
	pfcnt++;
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
			putcnt();
			return (p);
		}
	}
	/*
	 * Declare the prog/proc/func
	 */
	switch (r[0]) {
		case T_PROG:
			if (opt('z'))
				monflg++;
			program = p = defnl(r[2], PROG, 0, 0);
			p->value[3] = r[1];
			break;
		case T_PDEC:
			if (r[4] != NIL)
				error("Procedures do not have types, only functions do");
			p = enter(defnl(r[2], PROC, 0, 0));
			p->nl_flags =| NMOD;
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
			p->nl_flags =| NMOD;
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
		 * and compute total size
		 */
		cp = sp = p;
		o = 0;
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
							if (p->class == FILE)
								error("Files cannot be passed by value");
							else if (p->nl_flags & NFILES)
								error("Files cannot be a component of %ss passed by value",
									nameof(p));
						}
						dp = defnl(il[1], VAR, p, o=- even(width(p)));
						dp->nl_flags =| NMOD;
						break;
					case T_PVAR:
						dp = defnl(il[1], REF, p, o=- 2);
						break;
					case T_PFUNC:
					case T_PPROC:
						error("Procedure/function parameters not implemented");
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
		p->value[NL_OFFS] = -o+DPOFF2;
		/*
		 * Correct the naievity
		 * of our above code to
		 * calculate offsets
		 */
		for (il = p->chain; il != NIL; il = il->chain)
			il->value[NL_OFFS] =+ p->value[NL_OFFS];
	} else { 
		/*
		 * The wonderful
		 * program statement!
		 */
		if (monflg) {
			cntpatch = put2(O_PXPBUF, 0);
			nfppatch = put3(NIL, 0, 0);
		}
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
	p->value[NL_LOC] = getlab();
	if (monflg) {
		put2(O_TRACNT, p->value[NL_LOC]);
		putcnt();
	} else
		put2(O_TRA, p->value[NL_LOC]);
	return (p);
}

funcfwd(fp)
	struct nl *fp;
{

	return (fp);
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
	sizes[cbn].om_off = 0;
	sizes[cbn].om_max = 0;
	gotos[cbn] = NIL;
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
	int var, inp, out, chkref, *blk;
	struct nl *iop;
	char *cp;
	extern int cntstat;

	cntstat = 0;
/*
	yyoutline();
*/
	if (program != NIL)
		line = program->value[3];
	blk = bundle[1];
	if (fp == NIL) {
		cbn--;
		return;
	}
	/*
	 * Patch the branch to the
	 * entry point of the function
	 */
	patch(fp->value[NL_LOC]);
	/*
	 * Put out the block entrance code and the block name.
	 * the CONG is overlaid by a patch later!
	 */
	var = put1(cbn == 1 && opt('p') == 0 ? O_NODUMP: O_BEG);
	put3(O_CONG, 8, fp->symbol);
	put2(NIL, bundle[0]);
	if (fp->class == PROG) {
		/*
		 * The glorious buffers option.
		 *          0 = don't buffer output
		 *          1 = line buffer output
		 *          2 = 512 byte buffer output
		 */
		if (opt('b') != 1)
			put1(O_BUFF | opt('b') << 8);
		inp = 0;
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
			if (iop->type->class != FILE) {
				error("File %s listed in program statement but defined as %s",
					p->symbol, nameof(iop->type));
				continue;
			}
			put2(O_LV | bn << 9, iop->value[NL_OFFS]);
			b = p->symbol;
			while (b->pchar != '\0')
				b++;
			i = b - p->symbol;
			put3(O_CONG, i, p->symbol);
			put2(O_DEFNAME | i << 8, text(iop->type) ? 0: width(iop->type->type));
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
	line = bundle[0];
	statlist(blk);
	if (cbn== 1 && monflg != 0) {
		patchfil(cntpatch, cnts);
		patchfil(nfppatch, pfcnt);
	}
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
					if ((p->nl_flags & NUSED) == 0) {
						warning();
						nerror("%s %s is never used", classes[p->class], p->symbol);
						break;
					}
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
					putchar('\n');
					break;

				case FUNC:
				case PROC:
					if (p->nl_flags & NFORWD)
						nerror("Unresolved forward declaration of %s %s", classes[p->class], p->symbol);
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

	put1(O_END);
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
	/*
	 * Patch the beg
	 * of the proc/func to
	 * the proper variable size
	 */
	i = sizes[cbn].om_max;
	if (sizes[cbn].om_max < -50000.)
		nerror("Storage requirement of %ld bytes exceeds hardware capacity", -sizes[cbn].om_max);
	if (Fp == NIL)
		elineon();
	patchfil(var, i);
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
		elineoff();
	}
	error(a1, a2, a3);
}
