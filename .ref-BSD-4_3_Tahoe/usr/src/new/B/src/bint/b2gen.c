/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/* $Header: b2gen.c,v 1.4 85/08/27 10:57:31 timo Exp $ */

/* Code generation */

#include "b.h"
#include "b0fea.h"
#include "b1obj.h"
#include "b2exp.h"
#include "b2nod.h"
#include "b2gen.h" /* Must be after b2nod.h */
#include "b3err.h"
#include "b3env.h"
#include "b3int.h"
#include "b3sem.h"
#include "b3sou.h"

Visible Procedure fix_nodes(pt, code) parsetree *pt; parsetree *code; {
	context c; value *setup(), *su;
	sv_context(&c);
	curline= *pt; curlino= one;
	su= setup(*pt);
	if (su) analyze(*pt, su);
	curline= *pt; curlino= one;
	inithreads();
	fix(pt, su ? 'x' : 'v');
	endthreads(code);
	cleanup();
#ifdef TYPE_CHECK
	if (cntxt != In_prmnv) type_check(*pt);
#endif
	set_context(&c);
}

/* ******************************************************************** */

/* Utilities used by threading. */

/* A 'threaded tree' is, in our case, a fixed(*) parse tree with extra links
   that are used by the interpreter to determine the execution order.
   __________
   (*) 'Fixed' means: processed by 'fix_nodes', which removes UNPARSED
       nodes and distinguishes TAG nodes into local, global tags etc.
       fix_nodes also creates the threads, but this is accidental, not
       essential.  For UNPARSED nodes, the threads are actually laid
       in a second pass through the subtree that was UNPARSED.
   __________

   A small example: the parse tree for the expression  'a+b*c'  looks like

	(DYOP,
		(TAGlocal, "a"),
		"+",
		(DYOP,
			(TAGlocal, "b"),
			"*",
			(TAGlocal, "c"))).

   The required execution order is here:

	1) (TAGlocal, "a")
	2) (TAGlocal, "b")
	3) (TAGlocal, "c")
	4) (DYOP, ..., "*", ...)
	5) (DYOP, ..., "+", ...)

   Of course, the result of each operation (if it has a result) is pushed
   on a stack, and the operands are popped from this same stack.  Think of
   reversed polish notation (well-known by owners of HP pocket calculators).

   The 'threads' are explicit links from each node to its successor in this
   execution order.  Conditional operations like IF and AND have two threads,
   one for success and one for failure.  Loops can be made by having the
   thread from the last node of the loop body point to the head of the loop.

   Threading expressions, locations and simple-commands is easy: recursively
   thread each of the subtrees, then lay a thread from the last threaded
   to the current node.  Nodes occurring in a 'location' context are
   marked, so that the interpreter knows when to push a 'location' on
   the stack.

   Tests and looping commands cause most of the complexity of the threading
   utilities.  The basic technique is 'backpatching'.
   Nodes that need a conditional forward jump are chained together in a
   linked list, and when their destination is reached, all nodes in the
   chain get its 'address' patched into their secondary thread.  There is
   one such chain, called 'bpchain', which at all times contains those nodes
   whose secondary destination would be the next generated instruction.
   This is used by IF, WHILE, test-suites, AND and OR.

   To generate a loop, both this chain and the last normal instruction
   (if any) are diverted to the node where the loop continues.

   For test-suites, we also need to be capable of jumping unconditionally
   forward (over the remainder of the SELECT-command).  This is done by
   saving both the backpatch chain and the last node visited, and restoring
   them after the remainder has been processed.
*/

/* Implementation tricks: in order not to show circular lists to 'release',
   parse tree nodes are generated as compounds where there is room for two
   more fields than their length indicates.
*/

#define Flag (MkSmallInt(1))
	/* Flag used to indicate Location or TestRefinement node */

Hidden parsetree start; /* First instruction.  Picked up by endthreads() */

Hidden parsetree last; /* Last visited node */

Hidden parsetree bpchain; /* Backpatch chain for conditional goto's */
Hidden parsetree *wanthere; /* Chain of requests to return next tree */

extern string opcodes[];


/* Start threading */

Hidden Procedure inithreads() {
	bpchain= NilTree;
	wanthere= 0;
	last= 0;
	here(&start);
}

/* Finish threading */

Hidden Procedure endthreads(code) parsetree *code; {
	jumpto(Stop);
	if (!still_ok) start= NilTree;
	*code= start;
}


/* Fill 't' as secondary thread for all nodes in the backpatch chain,
   leaving the chain empty. */

Hidden Procedure backpatch(t) parsetree t; {
	parsetree u;
	while (bpchain != NilTree) {
		u= Thread2(bpchain);
		Thread2(bpchain)= t;
		bpchain= u;
	}
}

Visible Procedure jumpto(t) parsetree t; {
	parsetree u;
	if (!still_ok) return;
	while (wanthere != 0) {
		u= *wanthere;
		*wanthere= t;
		wanthere= (parsetree*)u;
	}
	while (last != NilTree) {
		u= Thread(last);
		Thread(last)= t;
		last= u;
	}
	backpatch(t);
}

Hidden parsetree seterr(n) int n; {
	return (parsetree)MkSmallInt(n);
}

/* Visit node 't', and set its secondary thread to 't2'. */

Hidden Procedure visit2(t, t2) parsetree t, t2; {
	if (!still_ok) return;
	jumpto(t);
	Thread2(t)= t2;
#ifdef DEBUG
	fprintf(stderr, "\tvisit %s %s\n", opcodes[Nodetype(t)],
		t2 == NilTree ? "" : "[*]");
#endif DEBUG
	Thread(t)= NilTree;
	last= t;
}

/* Visit node 't' */

Hidden Procedure visit(t) parsetree t; {
	visit2(t, NilTree);
}

/* Visit node 't' and flag it as a location (or test-refinement). */

Hidden Procedure lvisit(t) parsetree t; {
	visit2(t, Flag);
}

#ifdef NOT_USED
Hidden Procedure jumphere(t) parsetree t; {
	Thread(t)= last;
	last= t;
}
#endif

/* Add node 't' to the backpatch chain. */

Hidden Procedure jump2here(t) parsetree t; {
	if (!still_ok) return;
	Thread2(t)= bpchain;
	bpchain= t;
}

Hidden Procedure here(pl) parsetree *pl; {
	if (!still_ok) return;
	*pl= (parsetree) wanthere;
	wanthere= pl;
}

Visible Procedure hold(pl) struct state *pl; {
	if (!still_ok) return;
	pl->h_last= last; pl->h_bpchain= bpchain; pl->h_wanthere= wanthere;
	last= bpchain= NilTree; wanthere= 0;
}

Visible Procedure let_go(pl) struct state *pl; {
	parsetree p, *w;
	if (!still_ok) return;
	if (last) {
		for (p= last; Thread(p) != NilTree; p= Thread(p))
			;
		Thread(p)= pl->h_last;
	}
	else last= pl->h_last;
	if (bpchain) {
		for (p= bpchain; Thread2(p) != NilTree; p= Thread2(p))
			;
		Thread2(p)= pl->h_bpchain;
	}
	else bpchain= pl->h_bpchain;
	if (wanthere) {
		for (w= wanthere; *w != 0; w= (parsetree*) *w)
			;
		*w= (parsetree) pl->h_wanthere;
	}
	else wanthere= pl->h_wanthere;
}

Hidden bool reachable() {
	return last != NilTree || bpchain != 0 || wanthere != 0;
}


/* ******************************************************************** */
/* *********************** code generation **************************** */
/* ******************************************************************** */

Forward bool is_variable();
Forward bool is_cmd_ref();
Forward value copydef();

Visible Procedure fix(pt, flag) parsetree *pt; char flag; {
	struct state st; value v, function; parsetree t, l1= NilTree;
	typenode nt; string s; char c; int n, k, len;

	t= *pt;
	if (!Is_node(t) || !still_ok) return;
	nt= Nodetype(t);
	if (nt < 0 || nt >= NTYPES) syserr(MESS(2200, "fix bad tree"));
	s= gentab[nt];
	if (s == NULL) return;
	n= First_fieldnr;
	if (flag == 'x') curline= t;
	while ((c= *s++) != '\0' && still_ok) {
		switch (c) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			n= (c - '0') + First_fieldnr;
			break;
		case 'c':
			v= *Branch(t, n);
			if (v != Vnil) {
				len= Nfields(v);
				for (k= 0; k < len; ++k)
					fix(Field(v, k), flag);
			}
			++n;
			break;
		case '#':
			curlino= *Branch(t, n);
			++n;
			break;
		case 'g':
		case 'h':
			++n;
			break;
		case 'a':
		case 'l':
			if (flag == 'v' || flag == 't')
				c= flag;
			/* Fall through */
		case '!':
		case 't':
		case 'u':	
		case 'v':
		case 'x':
			fix(Branch(t, n), c);
			++n;
			break;
		case 'f':
			f_fpr_formals(*Branch(t, n));
			++n;
			break;

		case '?':
			if (flag == 'v')
				f_eunparsed(pt);
			else if (flag == 't')
				f_cunparsed(pt);
			else
			  syserr(MESS(2201, "fix unparsed with bad flag"));
			fix(pt, flag);
			break;
		case 'C':
			v= *Branch(t, REL_LEFT);
			if (Comparison(Nodetype(v)))
				jump2here(v);
			break;
		case 'D':
			v= (value)*Branch(t, DYA_NAME);
			if (!is_dyafun(v, &function))
			  fixerr2(v, MESS(2202, " isn't a dyadic function"));
			else
				*Branch(t, DYA_FCT)= copydef(function);
			break;
		case 'E':
			v= (value)*Branch(t, DYA_NAME);
			if (!is_dyaprd(v, &function))
			  fixerr2(v, MESS(2203, " isn't a dyadic predicate"));
			else
				*Branch(t, DYA_FCT)= copydef(function);
			break;
		case 'G':
			jumpto(l1);
			break;
		case 'H':
			here(&l1);
			break;
		case 'I':
			if (*Branch(t, n) == NilTree)
				break;
			/* Else fall through */
		case 'J':
			jump2here(t);
			break;
		case 'K':
			hold(&st);
			break;
		case 'L':
			let_go(&st);
			break;
		case 'M':
			v= (value)*Branch(t, MON_NAME);
			if (is_variable(v) || !is_monfun(v, &function))
			  fixerr2(v, MESS(2204, " isn't a monadic function"));
			else
				*Branch(t, MON_FCT)= copydef(function);
			break;
		case 'N':
			v= (value)*Branch(t, MON_NAME);
			if (is_variable(v) || !is_monprd(v, &function))
			  fixerr2(v, MESS(2205, " isn't a monadic predicate"));
			else
				*Branch(t, MON_FCT)= copydef(function);
			break;
#ifdef REACH
		case 'R':
			if (*Branch(t, n) != NilTree && !reachable())
			    fixerr(MESS(2206, "command cannot be reached"));
			break;
#endif
		case 'S':
			jumpto(Stop);
			break;
		case 'T':
			if (flag == 't')
				f_ctag(pt);
			else if (flag == 'v' || flag == 'x')
				f_etag(pt);
			else
				f_ttag(pt);
			break;
		case 'U':
			f_ucommand(pt);
			break;
		case 'V':
			visit(t);
			break;
		case 'X':
			if (flag == 'a' || flag == 'l' || flag == '!')
				lvisit(t);
			else
				visit(t);
			break;
		case 'W':
/*!*/			visit2(t, seterr(1));
			break;
		case 'Y':
			if (still_ok && reachable()) {
			  if (nt == YIELD)
			    fixerr(MESS(2207, "YIELD-unit returns no value"));
			  else
			    fixerr(MESS(2208, "TEST-unit reports no outcome"));
			}
			break;
		case 'Z':
			if (!is_cmd_ref(t) && still_ok && reachable())
  fixerr(MESS(2209, "refinement returns no value c.q. reports no outcome"));
  			*Branch(t, REF_START)= copy(l1);
			break;
		}
	}
}

/* ******************************************************************** */

Hidden bool is_cmd_ref(t) parsetree t; { /* HACK */
	value name= *Branch(t, REF_NAME);
	string s= strval(name);
	/* return isupper(*s); */
	return *s <= 'Z' && *s >= 'A';
}

Visible value copydef(f) value f; {
	funprd *fpr= Funprd(f);
	if (fpr->pre == Use) return Vnil;
	return copy(f);
}

Hidden bool is_basic_target(v) value v; {
	return envassoc(formals, v) ||
		locals != Vnil && envassoc(locals, v) ||
		envassoc(globals, v) ||
		envassoc(mysteries, v);
}

Hidden bool is_variable(v) value v; {
	value f;
	return is_basic_target(v) ||
		envassoc(refinements, v) ||
		is_zerfun(v, &f);
}

Hidden bool is_target(p) parsetree p; {
	value v= *Branch(p, First_fieldnr); int k, len;
	switch (Nodetype(p)) {

	case TAG:
		return is_basic_target(v);

	case SELECTION:
	case BEHEAD:
	case CURTAIL:
	case COMPOUND:
		return is_target(v);

	case COLLATERAL:
		len= Nfields(v);
		k_Overfields {
			if (!is_target(*Field(v, k))) return No;
		}
		return Yes;

	default:
		return No;

	}
}

/* ******************************************************************** */

Hidden Procedure f_actuals(formals, pactuals) parsetree formals, *pactuals; {
	/* name, actual, next */
	value actuals= *pactuals, act, form, next_a, next_f, kw, *pact;
	kw= *Branch(actuals, ACT_KEYW);
	pact= Branch(actuals, ACT_EXPR); act= *pact;
	form= *Branch(formals, FML_TAG);
	next_a= *Branch(actuals, ACT_NEXT); next_f= *Branch(formals, FML_NEXT);
	if (compare(*Branch(formals, FML_KEYW), kw) != 0)
		fixerr3(MESS(2210, "wrong keyword "), kw, 0);
	else if (act == Vnil && form != Vnil)
		fixerr3(MESS(2211, "missing actual after "), kw, 0);
	else if (next_a == Vnil && next_f != Vnil)
		fixerr3(MESS(2212, "can't find expected "),
			*Branch(next_f, FML_KEYW), 0);
	else if (act != Vnil && form == Vnil)
		fixerr3(MESS(2213, "unexpected actual after "), kw, 0);
	else if (next_a != Vnil && next_f == Vnil)
		fixerr3(MESS(2214, "unexpected keyword "),
			*Branch(next_a, ACT_KEYW), 0);
	else {
		if (act != Vnil) {
			parsetree st; struct state save;
			hold(&save); here(&st);
			if (is_target(act)) f_targ(pact);
			else f_expr(pact);
			jumpto(Stop); let_go(&save);
			*Branch(actuals, ACT_START)= copy(st);
		}
		if (still_ok && next_a != Vnil)
			f_actuals(next_f, Branch(actuals, ACT_NEXT));
	}
}

Hidden Procedure f_ucommand(pt) parsetree *pt; {
	value t= *pt, *aa;
	parsetree u, *f1= Branch(t, UCMD_NAME), *f2= Branch(t, UCMD_ACTUALS);
	release(*Branch(t, UCMD_DEF));
	*Branch(t, UCMD_DEF)= Vnil;
	if ((aa= envassoc(refinements, *f1)) != Pnil) {
		if (*Branch(*f2, ACT_EXPR) != Vnil
				|| *Branch(*f2, ACT_NEXT) != Vnil)
			fixerr(MESS(2215, "refinement with parameters"));
		else *Branch(t, UCMD_DEF)= copy(*aa);
	}
	else if (is_unit(*f1, How, &aa)) {
		u= How_to(*aa)->unit;
		f_actuals(*Branch(u, HOW_FORMALS), f2);
	}
	else if (still_ok)
		fixerr3(MESS(2216, "you haven't told me HOW'TO "), *f1, 0);
}

Hidden Procedure f_fpr_formals(t) parsetree t; {
	switch (Nodetype(t)) {
	case TAG:
		break;
	case MONF: case MONPRD:
		f_targ(Branch(t, MON_RIGHT));
		break;
	case DYAF: case DYAPRD:
		f_targ(Branch(t, DYA_LEFT));
		f_targ(Branch(t, DYA_RIGHT));
		break;
	default:
		syserr(MESS(2217, "f_fpr_formals"));
	}
}

Visible bool modify_tag(name, tag) parsetree *tag; value name; {
	value *aa, function;
	*tag= NilTree;
	if (aa= envassoc(formals, name))
		*tag= node3(TAGformal, name, copy(*aa));
	else if (locals != Vnil && (aa= envassoc(locals, name)))
		*tag= node3(TAGlocal, name, copy(*aa));
	else if (aa= envassoc(globals, name))
		*tag= node2(TAGglobal, name);
	else if (aa= envassoc(mysteries, name))
		*tag= node3(TAGmystery, name, copy(*aa));
	else if (aa= envassoc(refinements, name))
		*tag= node3(TAGrefinement, name, copy(*aa));
	else if (is_zerfun(name, &function))
		*tag= node3(TAGzerfun, name, copydef(function));
	else if (is_zerprd(name, &function))
		*tag= node3(TAGzerprd, name, copydef(function));
	else return No;
	return Yes;
}

Hidden Procedure f_etag(pt) parsetree *pt; {
	parsetree t= *pt; value name= copy(*Branch(t, TAG_NAME));
	if (modify_tag(name, &t)) {
		release(*pt);
		*pt= t;
		if (Nodetype(t) == TAGzerprd)
			fixerr2(name, MESS(2218, " cannot be used in an expression"));
		else
			visit(t);
	} else {
		fixerr2(name, MESS(2219, " has not yet received a value"));
		release(name);
	}
}

Hidden Procedure f_ttag(pt) parsetree *pt; {
	parsetree t= *pt; value name= copy(*Branch(t, TAG_NAME));
	if (modify_tag(name, &t)) {
		release(*pt);
		*pt= t;
		switch (Nodetype(t)) {
		case TAGrefinement:
			fixerr(MESS(2220, "a refinement may not be used as a target"));
			break;
		case TAGzerfun:
		case TAGzerprd:
			fixerr2(name, MESS(2221, " hasn't been initialised or defined"));
			break;
		default:
			lvisit(t);
			break;
		}
	} else {
		fixerr2(name, MESS(2222, " hasn't been initialised or defined"));
		release(name);
	}
}

Hidden Procedure f_ctag(pt) parsetree *pt; {
	parsetree t= *pt; value name= copy(*Branch(t, TAG_NAME));
	if (modify_tag(name, &t)) {
		release(*pt);
		*pt= t;
		switch (Nodetype(t)) {
		case TAGrefinement:
			lvisit(t); /* 'Loc' flag here means 'Test' */
			break;
		case TAGzerprd:
			visit(t);
			break;
		default:
			fixerr2(name, MESS(2223, " is neither a refined test nor a zeroadic predicate"));
			break;
		}
	} else {
		fixerr2(name, MESS(2224, " is neither a refined test nor a zeroadic predicate"));
		release(name);
	}
}
