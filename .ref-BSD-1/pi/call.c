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

/*
 * Call generates code for calls to
 * user defined procedures and functions
 * and is called by proc and funccod.
 * P is the result of the lookup
 * of the procedure/function symbol,
 * and porf is PROC or FUNC.
 * Psbn is the block number of p.
 */
call(p, argv, porf, psbn)
	struct nl *p;
	int *argv, porf, psbn;
{
	register struct nl *p1, *q;
	int *r;

	if (porf == FUNC)
		/*
		 * Push some space
		 * for the function return type
		 */
		put2(O_PUSH, even(-width(p->type)));
	/*
	 * Loop and process each of
	 * arguments to the proc/func.
	 */
	for (p1 = p->chain; p1 != NIL; p1 = p1->chain) {
		if (argv == NIL) {
			error("Not enough arguments to %s", p->symbol);
			return (NIL);
		}
		switch (p1->class) {
			case REF:
				/*
				 * Var parameter
				 */
				r = argv[1];
				if (r != NIL && r[0] != T_VAR) {
					error("Expression given (variable required) for var parameter %s of %s", p1->symbol, p->symbol);
					break;
				}
				q = lvalue(argv[1], MOD);
				if (q == NIL)
					break;
				if (q != p1->type) {
					error("Parameter type not identical to type of var parameter %s of %s", p1->symbol, p->symbol);
					break;
				}
				break;
			case VAR:
				/*
				 * Value parameter
				 */
				q = rvalue(argv[1], p1->type);
				if (q == NIL)
					break;
				if (incompat(q, p1->type, argv[1])) {
					cerror("Expression type clashed with type of value parameter %s of %s", p1->symbol, p->symbol);
					break;
				}
				if (isa(p1->type, "bcsi"))
					rangechk(p1->type, q);
				if (q->class != STR)
					convert(q, p1->type);
				break;
			default:
				panic("call");
		}
		argv = argv[2];
	}
	if (argv != NIL) {
		error("Too many arguments to %s", p->symbol);
		rvlist(argv);
		return (NIL);
	}
	put2(O_CALL | psbn << 9, p->value[NL_LOC]);
	put2(O_POP, p->value[NL_OFFS]-DPOFF2);
	return (p->type);
}

rvlist(al)
	register int *al;
{

	for (; al != NIL; al = al[2])
		rvalue(al[1], NIL);
}
