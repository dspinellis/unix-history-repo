/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2tes.c,v 1.4 85/08/22 16:57:17 timo Exp $
*/

#include "b.h"
#include "b1obj.h"
#include "b2par.h"
#include "b2key.h"
#include "b2syn.h"
#include "b2nod.h"
#include "b3err.h"

Forward bool conjunction(), disjunction();
Forward parsetree right_test();

Visible parsetree test(q) txptr q; {
	parsetree v;
	skipsp(&tx);
	if (!(conjunction(q, &v) || disjunction(q, &v))) v= right_test(q);
	return v;
}

Forward bool negation(), quantification();
Forward parsetree tight_test();

Hidden parsetree right_test(q) txptr q; {
	parsetree v;
	skipsp(&tx);
	if (!(negation(q, &v) || quantification(q, &v))) v= tight_test(q);
	return v;
}

Hidden bool conjunction(q, v) txptr q; parsetree *v; {
	txptr ftx, ttx;
	if (find(K_AND, q, &ftx, &ttx)) {
		parsetree t;
		t= tight_test(ftx); tx= ttx;
		if (!conjunction(q, v)) *v= right_test(q);
		*v= node3(AND, t, *v);
		return Yes;
	}
	return No;
}

Hidden bool disjunction(q, v) txptr q; parsetree *v; {
	txptr ftx, ttx;
	if (find(K_OR, q, &ftx, &ttx)) {
		parsetree t;
		t= tight_test(ftx); tx= ttx;
		if (!disjunction(q, v)) *v= right_test(q);
		*v= node3(OR, t, *v);
		return Yes;
	}
	return No;
}

Hidden bool negation(q, v) txptr q; parsetree *v; {
	if (not_keyword()) {
		*v= node2(NOT, right_test(q));
		return Yes;
	}
	return No;
}

Hidden bool quantification(q, v) txptr q; parsetree *v; {
	bool some, each;
	if ((some= some_keyword()) || (each= each_keyword()) || no_keyword()) {
		parsetree t, e; typenode type;
		txptr utx, vtx, ftx, ttx;
		req(K_HAS, ceol, &utx, &vtx);
		if (utx > q) {
			parerr(MESS(2700, "HAS follows colon"));
			/* as in: SOME i IN x: SHOW i HAS a */
			utx= tx; vtx= q;
		}
		if (find(K_IN_quant, utx, &ftx, &ttx)) {
			idf_cntxt= In_ranger;
			t= idf(ftx); tx= ttx;
			type= some ? SOME_IN : each ? EACH_IN : NO_IN;
		} else if (find(K_PARSING, utx, &ftx, &ttx)) {
			idf_cntxt= In_ranger;
			t= idf(ftx);
			if (nodetype(t) != COLLATERAL)
				pprerr(MESS(2701, "no collateral_identifier where expected"));
			tx= ttx;
			type= some ? SOME_PARSING : each ? EACH_PARSING
			      : NO_PARSING;
		} else {
			parerr(MESS(2702, "neither IN nor PARSING found"));
			utx= tx; vtx= q; t= NilTree; type= Nonode;
		}
		e= expr(utx); tx= vtx;
		*v= node4(type, t, e, right_test(q));
		return Yes;
	}
	return No;
}

Forward bool cl_test(), order_test();
Forward parsetree ref_or_prop();

Hidden parsetree tight_test(q) txptr q; {
	parsetree v;
	skipsp(&tx);
	if (nothing(q, "test")) v= NilTree;
	else if (!(cl_test(q, &v) || order_test(q, &v))) {
		if (is_expr(Char(tx))) v= ref_or_prop(q);
		else {
			parerr(MESS(2703, "no test where expected"));
			v= NilTree;
		}
	}
	upto_test(q);
	return v;
}

Hidden bool cl_test(q, v) txptr q; parsetree *v; {
	txptr tx0= tx;
	if (open_sign()) { /* (expr) or (test) */
		txptr ftx, ttx, tx1;
		tx1= tx;
		req(")", q, &ftx, &ttx); tx= ttx;
		skipsp(&tx);
		if (!Text(q)) {
			tx= tx1;
			*v= compound(ttx, test);
			return Yes;
		}
	}
	tx= tx0;
	return No;
}

Forward typenode relop();

Hidden bool order_test(q, v) txptr q; parsetree *v; {
	txptr ftx;
	if (findrel(q, &ftx)) {
		typenode r;
		*v= singexpr(ftx);
		do {
			r= relop();
			if (!findrel(q, &ftx)) ftx= q;
			*v= node3(r, *v, singexpr(ftx));
		} while (ftx < q);
		return Yes;
	}
	return No;
}

Hidden typenode relop() {
	skipsp(&tx);
	return
		at_most_sign()		? AT_MOST :
		unequal_sign()		? UNEQUAL :
		at_least_sign()		? AT_LEAST :
		equals_sign()		? EQUAL :
		less_than_sign()	? LESS_THAN :
		greater_than_sign()	? GREATER_THAN :
		/* psyserr */		  Nonode;
}

/* refined_test or proposition */

Forward parsetree dyadic_proposition();

Hidden parsetree ref_or_prop(q) txptr q; {
	value t1;
	txptr tx0= tx;
	if (tag_operator(q, &t1)) {
		value t2;
		skipsp(&tx);
		if (!Text(q)) return node2(TAG, t1);
		if (tag_operator(q, &t2)) {
			skipsp(&tx);
			if (!Text(q))
				return node4(MONPRD, t1, node2(TAG, t2), Vnil);
			release(t1); release(t2);
			return (tx= tx0, unp_test(q));
		}
		release(t1);
		if (!dya_sign()) return (tx= tx0, unp_test(q));
	}
	return (tx= tx0, dyadic_proposition(q));
} 

Visible bool dya_proposition= No;

Hidden parsetree dyadic_proposition(q) txptr q; {
	parsetree v; value name;
	dya_proposition= Yes;
	v= singexpr(q);
	if (!Text(q)) /* unparsed */
		return v;
	if (!tag_operator(q, &name)) {
		parerr(MESS(2704, "no dyadic predicate where expected"));
		name= Vnil;
	}
	return node5(DYAPRD, v, name, singexpr(q), Vnil);
}

Hidden Procedure upto_test(q) txptr q; {
	skipsp(&tx);
	if (Text(q)) {
		txptr ftx, ttx;
		if (find(K_AND, q, &ftx, &ttx) || find(K_OR, q, &ftx, &ttx)) {
			tx= ftx;
			parerr(MESS(2705, "cannot determine priorities; use ( and ) to resolve"));
		} else parerr(MESS(2706, "something unexpected following test"));
		tx= q;
	}
}
