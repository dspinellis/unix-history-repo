/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3int.c,v 1.4 85/08/22 16:58:27 timo Exp $
*/

/* B interpreter using theaded trees */

#include "b.h"
#include "b0fea.h"
#include "b1mem.h"
#include "b1obj.h"
#include "b2nod.h"
#include "b3err.h"
#include "b3sem.h"
#include "b3env.h"
#include "b3int.h"
#include "b3in2.h"
#include "b3sta.h"


/* Relicts from old system: */

Visible value resval;
Visible bool terminated;


/* Shorthands: */

#define Pop2(fun) (w = pop(), v = pop(), fun(v, w), release(v), release(w))
#define Pop1(fun) (v = pop(), fun(v), release(v))
#define Dyop(funvw) \
	(w = pop(), v = pop(), push(funvw), release(v), release(w))
#define Monop(funv) (v = pop(), push(funv), release(v))
#define Flagged() (Thread2(pc) != NilTree)
#define LocFlagged() (Thread2(pc) != NilTree && !noloc)
#define ValOrLoc(feval, floc) (LocFlagged() ? (floc) : (feval))
#define Jump() (tracing && tr_jump(), next = Thread2(pc))
#define Comp(op) (w = pop(), v = pop(), report = (compare(v, w) op 0), Comp2())
#define Comp2() (release(v), !Flagged() ? release(w) : Comp3())
#define Comp3() (report ? push(w) : (Jump(), release(w)))
#define F(n) ((value)*Branch(pc, (n)))


/* Execute a threaded tree until the end or until a terminating-command.
   The boolean argument 'wantvalue' tells whether it must deliver
   a value or not.
*/

Hidden value
run(start, wantvalue) parsetree start; bool wantvalue; {
	value u, v, w; int k; bool X, Y; int call_stop= call_level;
#ifdef IBMPC
	int loopcnt= 0;
#endif
	parsetree old_next= next;
	/* While run can be used recursively, save some state info */

	next= start;
	for (;;) {
#ifdef IBMPC
		if (loopcnt++ == 100) {
			bdos(0x2c, 0, 0);
			/* forcing a DOS function call (get time) */
			/* so that a break interrupt can be executed */
			loopcnt= 0;
		}
#endif
		if (!still_ok) break;
		pc= next;
		if (pc == Halt) {
			error(MESS(3500, "unexpected program halt"));
			break;
		}
		if (!Is_parsetree(pc)) {
			if (pc == Stop) {
				if (call_level == call_stop) break;
				ret();
				continue;
			}
			if (!Is_number(pc)) syserr(MESS(3501, "run: bad thread"));
			switch (intval(pc)) {
			case 0:
				pc= Stop;
				break;
			case 1:
				error(
			MESS(3502, "none of the alternative tests of SELECT succeeds"));
				break;
			case 2:
				if (resexp == Rep)
					error(MESS(3503, "TEST-unit reports no outcome"));
				else
					error(MESS(3504, "YIELD-unit returns no value"));
				break;
			case 3:
				if (resexp == Rep)
				 error(MESS(3505, "test-refinement reports no outcome"));
				else
				 error(MESS(3506, "refinement returns no value"));
				 /* "expression-" seems superfluous here */
				break;
			default:
				v= convert(pc, No, No);
				error3(MESS(3507, "run-time error "), v, 0);
				release(v);
			}
			continue;
		}
		next = Thread(pc);
		if (tracing) tr_node(pc);
/* <<<<<<<<<<<<<<<< */
switch (Nodetype(pc)) {

case HOW_TO:
case REFINEMENT:
	error(MESS(3508, "run: cannot execute unit-definition"));
	break;

case YIELD:
case TEST:
	switch (Nodetype(F(FPR_FORMALS))) {
	case TAG:
		break;
	case MONF: case MONPRD:
		w= pop(); v= pop();
		put(v, w); release(v); release(w);
		break;
	case DYAF: case DYAPRD:
		w= pop(); v= pop(); u= pop();
		put(u, w); release(u); release(w);
		u= pop();
		put(u, v); release(u); release(v);
		break;
	default:
		syserr(MESS(3509, "bad FPR_FORMAL"));
	}
	break;

/* Commands */

case SUITE:
	curlino = F(SUI_LINO);
	curline = F(SUI_CMD);
	break;

case IF:
case AND:
case WHILE:
case TEST_SUITE:
	if (!report) Jump(); break;

case OR: if (report) Jump(); break;

case FOR:
	w= pop(); v= pop();
	if (!in_ranger(v, &w)) { release(v); release(w); Jump(); }
	else { push(v); push(w); }
	break;

case PUT: Pop2(put_with_check); break;
case INSERT: Pop2(l_insert); break;
case REMOVE: Pop2(l_remove); break;
case CHOOSE: Pop2(choose); break;
case DRAW: Pop1(draw); break;
case SET_RANDOM: Pop1(set_random); break;
case DELETE: Pop1(l_delete); break;
case CHECK: if (!report) checkerr(); break;

case WRITE:
	nl(F(WRT_L_LINES));
	if (F(WRT_EXPR)) { v = pop(); writ(v); release(v); }
	nl(F(WRT_R_LINES));
	break;

case READ: Pop2(read_eg); break;

case READ_RAW: Pop1(read_raw); break;

case QUIT:
	if (resexp != Voi)
	   error(MESS(3510, "QUIT may only occur in a HOW'TO or command-refinement"));
	if (call_level == 0 && still_ok) terminated= Yes;
	next= Stop; break;
case RETURN:
	if (resexp != Ret)
	   error(MESS(3511, "RETURN may only occur in a YIELD or expression-refinement"));
	resval = pop(); next= Stop; break;
case REPORT:
	if (resexp != Rep)
	   error(MESS(3512, "REPORT may only occur in a TEST-unit or test-refinement"));
	next= Stop; break;
case SUCCEED:
	if (resexp != Rep)
	   error(MESS(3513, "SUCCEED may only occur in a TEST-unit or test-refinement"));
	report = Yes; next= Stop; break;
case FAIL:
	if (resexp != Rep)
	   error(MESS(3514, "FAIL may only occur in a TEST-unit or test-refinement"));
	report = No; next= Stop; break;

case USER_COMMAND:
	x_user_command(F(UCMD_NAME), F(UCMD_ACTUALS), F(UCMD_DEF));
	break;

case EXTENDED_COMMAND:
#ifdef EXT_COMMAND
	x_extended_command(F(ECMD_NAME), F(ECMD_ACTUALS));
#endif
	break;

/* Expressions, targets */

case COLLATERAL:
	v = mk_compound(k= Nfields(F(COLL_SEQ)));
	while (--k >= 0)
		*Field(v, k) = pop();
	push(v);
	break;

/* Expressions, targets */

case SELECTION: Dyop(ValOrLoc(associate(v, w), tbsel_loc(v, w))); break;

case BEHEAD:
	w= pop(); v= pop();
	push(LocFlagged() ? trim_loc(v, w, '@') : behead(v, w));
	release(v); release(w);
	break;

case CURTAIL:
	w= pop(); v= pop();
	push(LocFlagged() ? trim_loc(v, w, '|') : curtail(v, w));
	release(v); release(w);
	break;

case MONF:
	v = pop();
	formula(Vnil, F(MON_NAME), v, F(MON_FCT));
	release(v);
	break;

case DYAF:
	w = pop(); v = pop();
	formula(v,  F(DYA_NAME), w, F(DYA_FCT));
	release(v); release(w);
	break;

case TEXT_LIT:
	v= F(XLIT_TEXT);
	if (F(XLIT_NEXT)) { w= pop(); v= concat(v, w); release(w); }
	else copy(v);
	push(v);
	break;

case TEXT_CONV:
	if (F(XCON_NEXT)) w= pop();
	u= pop();
	v= convert(u, Yes, Yes);
	release(u);
	if (F(XCON_NEXT)) {
		v= concat(u= v, w);
		release(u);
		release(w);
	}
	push(v);
	break;

case ELT_DIS: push(mk_elt()); break;

case LIST_DIS:
	u = mk_elt();
	k= Nfields(F(LDIS_SEQ));
	while (--k >= 0) {
		insert(v = pop(), &u);
		release(v);
	}
	push(u);
	break;

case RANGE_DIS: Dyop(mk_range(v, w)); break;

case TAB_DIS:
	u = mk_elt();
	k= Nfields(F(TDIS_SEQ));
	while ((k -= 2) >= 0) {
		w = pop(); v = pop();
		/* Should check for same key with different associate */
		replace(w, &u, v);
		release(v); release(w);
	}
	push(u);
	break;

/* Tests */

case NOT: report = !report; break;

/* Quantifiers can be described as follows:
   Report X at first test which reports Y.  If no test reports Y, report !X.
      type	X	Y
      SOME	Yes	Yes
      EACH	No	No
      NO	No	Yes. */

case EACH_IN:	X= Y= No; goto quant;
case NO_IN:	X= No; Y= Yes; goto quant;
case SOME_IN:	X= Y= Yes;
quant:
	w= pop(); v= pop();
	if (Is_compound(w) && report == Y) { report= X; Jump(); }
	else if (!in_ranger(v, &w)) { report= !X; Jump(); }
	else { push(v); push(w); break; }
	release(v); release(w);
	break;

case EACH_PARSING:	X= Y= No; goto parse;
case NO_PARSING:	X= No; Y= Yes; goto parse;
case SOME_PARSING:	X= Y= Yes;
parse:
	w= pop(); v= pop();
	if (Is_compound(w) && report == Y) { report= X; Jump(); }
	else if (!pa_ranger(v, &w)) { report= !X; Jump(); }
	else { push(v); push(w); break; }
	release(v); release(w);
	break;

case MONPRD:
	v = pop();
	proposition(Vnil, F(MON_NAME), v, F(MON_FCT));
	release(v);
	break;

case DYAPRD:
	w = pop(); v = pop();
	proposition(v, F(DYA_NAME), w, F(DYA_FCT));
	release(v); release(w);
	break;

case LESS_THAN: Comp(<); break;
case AT_MOST: Comp(<=); break;
case GREATER_THAN: Comp(>); break;
case AT_LEAST: Comp(>=); break;
case EQUAL: Comp(==); break;
case UNEQUAL: Comp(!=); break;

case TAGformal:
	call_formal(F(TAG_NAME), F(TAG_ID), LocFlagged());
	break;

case TAGlocal:
	push(ValOrLoc(v_local(F(TAG_NAME), F(TAG_ID)), local_loc(F(TAG_ID))));
	break;

case TAGglobal:
	push(ValOrLoc(v_global(F(TAG_NAME)), global_loc(F(TAG_NAME))));
	break;

case TAGmystery:
	if (LocFlagged()) push(l_mystery(F(TAG_NAME), F(TAG_ID)));
	else v_mystery(F(TAG_NAME), F(TAG_ID));
	break;

case TAGrefinement:
	call_refinement(F(TAG_NAME), F(TAG_ID), Flagged());
	break;

case TAGzerfun:
	formula(Vnil,  F(TAG_NAME), Vnil, F(TAG_ID));
	break;

case TAGzerprd:
	proposition(Vnil,  F(TAG_NAME), Vnil, F(TAG_ID));
	break;

case NUMBER:
	push(copy(F(NUM_VALUE)));
	break;

default:
	syserr(MESS(3515, "run: bad node type"));

}
/* >>>>>>>>>>>>>>>> */
	}
	v = Vnil;
	if (wantvalue && still_ok) v = pop();
	/* Unwind stack when stopped by error: */
	while (call_level != call_stop) ret();
	next= old_next;
	return v;
}


/* External interfaces: */

Visible Procedure execthread(start) parsetree start; {
	run(start, No);
}

Visible value evalthread(start) parsetree start; {
	return run(start, Yes);
}

Visible Procedure initint() {
	/* Dummy, relict */
}
