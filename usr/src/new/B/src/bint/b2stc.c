/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2stc.c,v 1.4 85/08/22 16:55:56 timo Exp $
*/

/* B (intra-unit) type check */

#include "b.h"
#include "b1obj.h"
#include "b2nod.h"
#include "b2syn.h" 		/* temporary? for Cap in tc_refinement */
#include "b2tcP.h"
#include "b2tcU.h"
#include "b2tcE.h"
#include "b3err.h"

/* ******************************************************************** */

Hidden value refname;

/*
 * if in commandsuite of refinement: 
 *	holds refinement name;
 * if in commandsuite of yield unit:
 * 	holds B-text "returned value" 
 *		(used in error messages, no confusion possible)
 * else
 *	Vnil
 * To be used in tc_return()
 */

/* ******************************************************************** */

Forward polytype pt_expr();

Visible Procedure type_check(v) parsetree v; {
	typenode n;
	extern bool extcmds; /* Set in main by -E option */

	if (extcmds || !still_ok || v EQ NilTree)
		return;
	n = nodetype(v);
	curline= v; curlino= one;
	start_vars();
	refname = Vnil;
	usetypetable(mk_elt());
	if (Unit(n)) tc_unit(v);
	else if (Command(n)) tc_command(v);
	else if (Expression(n)) p_release(pt_expr(v));
	else syserr(MESS(2300, "wrong argument of 'type_check'"));
	end_vars();
	deltypetable();
}

#define TABSIZE 72

Hidden	Procedure (*(uni_tab[TABSIZE]))(); /*Units*/
Hidden	Procedure (*(cmd_tab[TABSIZE]))(); /*Commands*/
Hidden	polytype  (*(exp_tab[TABSIZE]))(); /*Expressions*/
Hidden	Procedure (*(tes_tab[TABSIZE]))(); /*Tests*/

#define FF First_fieldnr

Hidden Procedure tc_node(v, tab) parsetree v; int (*(tab[]))(); {
	auto (*f)()= tab[nodetype(v)];
	switch (Nbranches(v)) { 
		case 0: (*f)(); break; 
		case 1: (*f)(*Branch(v,FF)); break; 
		case 2: (*f)(*Branch(v,FF), *Branch(v,FF+1)); break; 
		case 3: (*f)(*Branch(v,FF), *Branch(v,FF+1),
			*Branch(v,FF+2)); break; 
		case 4: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3)); break; 
		case 5: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4)); break; 
		case 6: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4), *Branch(v,FF+5)); break; 
		case 7: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4), *Branch(v,FF+5), 
			*Branch(v,FF+6)); break;
		case 8: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4), *Branch(v,FF+5), 
			*Branch(v,FF+6), *Branch(v,FF+7)); break;
		case 9: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4), *Branch(v,FF+5), 
			*Branch(v,FF+6), *Branch(v,FF+7),
			*Branch(v,FF+8)); break;
		default: syserr(MESS(2301, "Wrong size node in tc_node"));
	}
}

Hidden polytype pt_node(v, tab) parsetree v; polytype (*(tab[]))(); {
	polytype (*f)()= tab[nodetype(v)];
	switch (Nbranches(v)) { 
		case 0: (*f)(); break; 
		case 1: (*f)(*Branch(v,FF)); break; 
		case 2: (*f)(*Branch(v,FF), *Branch(v,FF+1)); break; 
		case 3: (*f)(*Branch(v,FF), *Branch(v,FF+1),
			*Branch(v,FF+2)); break; 
		case 4: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3)); break; 
		case 5: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4)); break; 
		case 6: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4), *Branch(v,FF+5)); break; 
		case 7: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4), *Branch(v,FF+5), 
			*Branch(v,FF+6)); break;
		case 8: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4), *Branch(v,FF+5), 
			*Branch(v,FF+6), *Branch(v,FF+7)); break;
		case 9: (*f)(*Branch(v,FF), *Branch(v,FF+1), 
			*Branch(v,FF+2), *Branch(v,FF+3), 
			*Branch(v,FF+4), *Branch(v,FF+5), 
			*Branch(v,FF+6), *Branch(v,FF+7),
			*Branch(v,FF+8)); break;
		default: syserr(MESS(2302, "Wrong size node in pt_node"));
			/* NOTREACHED */
	}
}

/* ******************************************************************** */
/* Type Check units */
/* ******************************************************************** */

Hidden Procedure tc_unit(v) parsetree v; {
	if (v != NilTree) tc_node(v, uni_tab);
}

Hidden Procedure tc_howto_unit(name, formals, cmt,
			      suite, refinement, reftab, nlocals)
	parsetree suite, refinement;
	value name, formals, cmt, reftab, nlocals; {

	tc_command(suite);
	tc_unit(refinement);
}

Hidden Procedure tc_yield_unit(name, adic, formals, cmt,
			      suite, refinement, reftab, nlocals)
	parsetree suite, refinement;
	value name, adic, formals, cmt, reftab, nlocals; {

	refname = mk_text("returned value");
	tc_command(suite);
	release(refname); refname = Vnil;
	tc_unit(refinement);
}

Hidden Procedure tc_test_unit(name, adic, formals, cmt,
			     suite, refinement, reftab, nlocals)
	parsetree suite, refinement;
	value name, adic, formals, cmt, reftab, nlocals; {

	tc_command(suite);
	tc_unit(refinement);
}

Hidden Procedure tc_refinement(name, cmt, suite, next)
	parsetree suite, next; value name, cmt; {
	value n1 = curtail(name, one);

	if (!Cap(charval(n1))) 	/* should test for expression refinement */
		refname = copy(name);
	release(n1);
	tc_command(suite);
	if (refname NE Vnil) {
		release(refname); refname = Vnil;
	}
	
	tc_unit(next);
}

/* ******************************************************************** */
/* TypeCheck commands */
/* ******************************************************************** */

Hidden Procedure tc_command(v) parsetree v; {
	curline= v;
	end_vars();
	start_vars();
	if (v != NilTree) tc_node(v, cmd_tab);
}

Hidden Procedure tc_suite(lino, cmd, cmt, next)
	parsetree cmd, next; value lino, cmt; {

	curlino= lino;
	tc_command(cmd);
	tc_command(next);
}

Hidden Procedure tc_put(e, t) parsetree e, t; {
	polytype te, tt, u;
	te = pt_expr(e);
	tt = pt_expr(t);
	unify(te, tt, &u);
	p_release(te); p_release(tt); p_release(u);
}

Hidden Procedure tc_ins_rem(e, t) parsetree e, t; {
	polytype t_list_e, tt, u;
	t_list_e = mkt_list(pt_expr(e));
	tt = pt_expr(t);
	unify(tt, t_list_e, &u);
	p_release(t_list_e); p_release(tt); p_release(u);
}

Hidden Procedure tc_choose(t, e) parsetree t, e; {
	polytype t_tlt_t, te, u;
	t_tlt_t = mkt_tlt(pt_expr(t));
	te = pt_expr(e);
	unify(te, t_tlt_t, &u);
	p_release(te); p_release(t_tlt_t); p_release(u);
}

Hidden Procedure tc_draw(t) parsetree t; {
	polytype t_number, tt, u;
	tt = pt_expr(t);
	t_number = mkt_number();
	unify(tt, t_number, &u);
	p_release(t_number); p_release(tt); p_release(u);
}

Hidden Procedure tc_set_random(e) parsetree e; {
	p_release(pt_expr(e));
}

Hidden Procedure tc_delete(t) parsetree t; {
	p_release(pt_expr(t));
}

Hidden Procedure tc_check(c) parsetree c; {
	tc_test(c);
}

Hidden Procedure tc_nothing(t) parsetree t; {}

Hidden Procedure tc_write(nl1, e, nl2) parsetree e; value nl1, nl2; {
	if (e != NilTree)
		p_release(pt_expr(e));
}

Hidden Procedure tc_read(t, e) parsetree t, e; {
	polytype te, tt, u;
	te = pt_expr(e);
	tt = pt_expr(t);
	unify(tt, te, &u);
	p_release(te); p_release(tt); p_release(u);
}

Hidden Procedure tc_raw_read(t) parsetree t; {
	polytype t_text, tt, u;
	t_text = mkt_text();
	tt = pt_expr(t);
	unify(tt, t_text, &u);
	p_release(t_text); p_release(tt); p_release(u);
}

Hidden Procedure tc_ifwhile(c, cmt, s) parsetree c, s; value cmt; {
	tc_test(c);
	tc_command(s);
}

Hidden Procedure tc_for(t, e, cmt, s) parsetree t, e, s; value cmt; {
	polytype t_tlt_t, te, u;

	t_tlt_t = mkt_tlt(pt_expr(t));
	te = pt_expr(e);
	unify(te, t_tlt_t, &u);
	p_release(te); p_release(t_tlt_t); p_release(u);

	tc_command(s);
}

Hidden Procedure tc_select(cmt, s) parsetree s; value cmt; {
	tc_command(s);
}

Hidden Procedure tc_tes_suite(lino, c, cmt, s, next) 
	parsetree c, s, next; value lino, cmt; {
	curlino= lino;
	if (c != NilTree) {
		tc_test(c);
		tc_command(s);
	}
	tc_command(next);
}

Hidden Procedure tc_else(lino, cmt, s) parsetree s; value lino, cmt; {
	curlino= lino;
	tc_command(s);
}

Hidden Procedure tc_return(e) parsetree e; {
	polytype te, tt, u;
	te = pt_expr(e);
	if (refname EQ Vnil)
		error(MESS(2303, "RETURN not in YIELD unit or expression refinement"));
	else {
		tt = mkt_var(copy(refname));
		unify(tt, te, &u);
		p_release(tt); p_release(u);
	}
	p_release(te);
}

Hidden Procedure tc_report(c) parsetree c; {
	tc_test(c);
}

Hidden Procedure tc_user_command(name, v) value name, v; {
	parsetree e; value w= v;
	while (w != Vnil) {
		e= *Branch(w, ACT_EXPR);
		if (e != NilTree)
			p_release(pt_expr(e));
		w= *Branch(w, ACT_NEXT);
	}
}

/* ******************************************************************** */
/* calculate PolyType of EXPRessions
/* ******************************************************************** */

Hidden polytype pt_expr(v) parsetree v; {
	return pt_node(v, exp_tab);
}
		
Hidden polytype pt_compound(e) parsetree e; {
	return pt_expr(e);
}

Hidden polytype pt_collateral(e) value e; {
	intlet k, len= Nfields(e);
	polytype tc;
	tc = mkt_compound(len);
	for (k = 0; k < len; k++)
		putsubtype(pt_expr(*Field(e, k)), tc, k);
	return tc;
}

Hidden bool is_string(v, s) value v; string s; {
	value t;
	relation rel;
	
	rel = compare(v, t= mk_text(s));
	release(t);
	return (rel EQ 0 ? Yes : No);
}

Hidden bool monf_on_number(n) value n; {
	return (is_string(n, "~") ||
		is_string(n, "+") ||
		is_string(n, "-") ||
		is_string(n, "*/") ||
		is_string(n, "/*") ||
		is_string(n, "root") ||
		is_string(n, "abs") ||
		is_string(n, "sign") ||
		is_string(n, "floor") ||
		is_string(n, "ceiling") ||
		is_string(n, "round") ||
		is_string(n, "sin") ||
		is_string(n, "cos") ||
		is_string(n, "tan") ||
		is_string(n, "atan") ||
		is_string(n, "exp") ||
		is_string(n, "log")
	);
}

Hidden bool dyaf_on_number(n) value n; {
	return (is_string(n, "+") ||
		is_string(n, "-") ||
		is_string(n, "*") ||
		is_string(n, "/") ||
		is_string(n, "**") ||
		is_string(n, "root") ||
		is_string(n, "round") ||
		is_string(n, "mod") ||
		is_string(n, "atan") ||
		is_string(n, "log")
	);
}

Hidden polytype pt_monf(name, r, fct) parsetree r; value name, fct; {
	polytype tr, tf, u;

	tr = pt_expr(r);

	if (monf_on_number(name)) {
		polytype t_number = mkt_number();
		unify(tr, t_number, &u);
		p_release(u);
		tf = t_number;
	}
	else if (is_string(name, "keys")) {
		polytype t_table, t_keys;
		t_keys = mkt_newvar();
		t_table = mkt_table(p_copy(t_keys), mkt_newvar());
		unify(tr, t_table, &u);
		p_release(t_table); p_release(u);
		tf = mkt_list(t_keys);
	}
	else if (is_string(name, "#")) {
		polytype t_tlt = mkt_tlt(mkt_newvar());
		unify(tr, t_tlt, &u);
		p_release(t_tlt); p_release(u);
		tf = mkt_number();
	}
	else if (is_string(name, "min") || is_string(name, "max")) {
		polytype t_tlt_x, t_x;
		t_x = mkt_newvar();
		t_tlt_x = mkt_tlt(p_copy(t_x));
		unify(tr, t_tlt_x, &u);
		p_release(t_tlt_x); p_release(u);
		tf = t_x;
	}
	else {
		tf = mkt_newvar();
	}
	
	p_release(tr);
	return tf;
}

Hidden polytype pt_dyaf(l, name, r, fct) parsetree l, r; value name, fct; {
	polytype tl, tr, tf, u;
	
	tl = pt_expr(l);
	tr = pt_expr(r);
	if (dyaf_on_number(name)){
		polytype t_number = mkt_number();
		unify(tl, t_number, &u);
		p_release(u);
		unify(tr, t_number, &u);
		p_release(u);
		tf = t_number;
	}
	else if (is_string(name, "^")) {
		polytype t_text = mkt_text();
		unify(tl, t_text, &u);
		p_release(u);
		unify(tr, t_text, &u);
		p_release(u);
		tf = t_text;
	}
	else if (is_string(name, "^^")) {
		polytype t_text = mkt_text(), t_number = mkt_number();
		unify(tl, t_text, &u);
		p_release(u);
		unify(tr, t_number, &u);
		p_release(u); p_release(t_number);
		tf = t_text;
	}
	else if (is_string(name, "<<")
		 ||
		 is_string(name, "><")
		 ||
		 is_string(name, ">>"))
	{
		polytype t_number = mkt_number();
		unify(tr, t_number, &u);
		p_release(u); p_release(t_number);
		tf = mkt_text();
	}
	else if (is_string(name, "#")) {
		polytype t_tlt_l = mkt_tlt(p_copy(tl));
		unify(tr, t_tlt_l, &u);
		p_release(t_tlt_l); p_release(u);
		tf = mkt_number();
	}
	else if (is_string(name, "min") || is_string(name, "max")) {
		polytype t_tlt_l = mkt_tlt(p_copy(tl));
		unify(tr, t_tlt_l, &u);
		tf = p_copy(asctype(u));
		p_release(t_tlt_l); p_release(u);
	}
	else if (is_string(name, "th'of")) {
		polytype t_number, t_tlt_x, t_x;
		t_number = mkt_number();
		unify(tl, t_number, &u);
		p_release(t_number); p_release(u);
		t_x = mkt_newvar();
		t_tlt_x = mkt_tlt(p_copy(t_x));
		unify(tr, t_tlt_x, &u);
		p_release(t_tlt_x); p_release(u);
		tf = t_x;
	}
	else {
		tf = mkt_newvar();
	}
	
	p_release(tl);
	p_release(tr);
	
	return tf;
}

Hidden polytype pt_tag(name) value name; {
	polytype var;
/*
 *	if (is_globalstring(name, "pi") || is_globalstring(name, "e"))
 *		return mkt_number();
 *	else
 */
	var = mkt_var(copy(name));
add_var(var);
 	return var;
}

Hidden polytype pt_tformal(name, number) value name, number; {
	return pt_tag(name);
}

Hidden polytype pt_tlocal(name, number) value name, number; {
	return pt_tag(name);
}

Hidden polytype pt_tglobal(name) value name; {
	return pt_tag(name);
}

Hidden polytype pt_tmystery(name, number) value name, number; {
	return pt_tag(name);
}

Hidden polytype pt_trefinement(name) value name; {
	return pt_tag(name);
}

Hidden polytype pt_tfun(name, fct) value name, fct; {
	return pt_tag(name);
}

Hidden polytype pt_tprd(name, fct) value name, fct; {
	return pt_tag(name);
}

Hidden polytype pt_number(v, t) value v, t; {
	return mkt_number();
}

Hidden polytype pt_text_dis(q, v) parsetree v; value q; {
	while(v NE NilTree) {
		switch (nodetype(v)) {
		case TEXT_LIT:
			v = *Branch(v, XLIT_NEXT);
			break;
		case TEXT_CONV:
			p_release(pt_expr(*Branch(v, XCON_EXPR)));
			v = *Branch(v, XCON_NEXT);
			break;
		default:
			v = NilTree;
		}
	}
	return mkt_text();
}

Hidden polytype pt_elt_dis() {
	return mkt_lt(mkt_newvar());
}

Hidden polytype pt_list_dis(e) value e; {
	intlet k, len= Nfields(e);
	polytype tres = pt_expr(*Field(e, 0));
	for (k = 1; k < len; k++) {
		polytype te, u;
		te = pt_expr(*Field(e, k));
		unify(te, tres, &u);
		p_release(te); p_release(tres);
		tres = u;
	}
	return mkt_list(tres);
}

Hidden polytype pt_range_dis(l, h) parsetree l, h; {
	polytype tl, th, t_tn, tres, u;
	t_tn = mkt_tn();
	tl = pt_expr(l);
	unify(tl, t_tn, &tres);
	p_release(tl); p_release(t_tn);
	th = pt_expr(h);
	unify(th, tres, &u);
	release(th); release(tres);
	return mkt_list(u);
}

Hidden polytype pt_tab_dis(e) value e; {
	intlet k, len= Nfields(e);
	polytype tresk, tresa;
	tresk = pt_expr(*Field(e, 0));
	tresa = pt_expr(*Field(e, 1));
	for (k = 2; k < len; k += 2) {
		polytype tk, ta, u;
		tk = pt_expr(*Field(e, k));
		unify(tk, tresk, &u);
		p_release(tk); p_release(tresk);
		tresk = u;
		ta = pt_expr(*Field(e, k+1));
		unify(ta, tresa, &u);
		p_release(ta); p_release(tresa);
		tresa = u;
	}
	return mkt_table(tresk, tresa);
}

Hidden polytype pt_selection(t, k) parsetree t, k; {
	polytype tt, ta, ttab, u;
	tt = pt_expr(t);
	ta = mkt_newvar();
	ttab = mkt_table(pt_expr(k), p_copy(ta));
	unify(tt, ttab, &u);
	p_release(tt); p_release(ttab); p_release(u);
	return ta;
}

Hidden polytype pt_trim(l, r) parsetree l, r; {
	polytype tl, tr, t_text, t_number, u;
	
	tl = pt_expr(l);
	t_text = mkt_text();
	unify(tl, t_text, &u);
	p_release(tl); p_release(u);
	tr = pt_expr(r);
	t_number = mkt_number();
	unify(tr, t_number, &u);
	p_release(tr); p_release(t_number); p_release(u);
	return t_text;
}

Hidden polytype pt_unparsed(v, t) parsetree v, t; {
	return mkt_newvar();
}

/* ******************************************************************** */
/* Type Check tests */
/* ******************************************************************** */

Hidden Procedure tc_test(v) parsetree v; {
	tc_node(v, tes_tab);
}

Hidden Procedure tc_compound(c) parsetree c; {
	tc_test(c);
}

Hidden Procedure tc_junction(l, r) parsetree l, r; {
	tc_test(l);
	tc_test(r);
}

Hidden Procedure tc_not(r) parsetree r; {
	tc_test(r);
}

Hidden Procedure tc_in_quantification(t, e, c) parsetree t, e, c; {
	polytype t_tlt_t, te, u;

	t_tlt_t = mkt_tlt(pt_expr(t));
	te = pt_expr(e);
	unify(te, t_tlt_t, &u);
	p_release(te); p_release(t_tlt_t); p_release(u);
	
	tc_test(c);
}

Hidden Procedure tc_p_quantification(t, e, c) parsetree t, e, c; {
	intlet k, len;
	value ct; 		/* the Collateral Tag in t */
	polytype t_text, te, u;

	t_text = mkt_text();
	
	ct = *Branch(t, COLL_SEQ);
	len = Nfields(ct);
	k_Over_len {
		polytype ttag;
		ttag = mkt_var(copy(*Branch(*Field(ct, k), TAG_NAME)));
add_var(ttag);
		unify(ttag, t_text, &u);
		p_release(ttag); p_release(u);
	}
	
	te = pt_expr(e);
	unify(te, t_text, &u);
	p_release(te); p_release(t_text); p_release(u);
	
	tc_test(c);
}

Hidden Procedure tc_tag(name) value name; {}

Hidden Procedure tc_tformal(name, number) value name, number; {
	tc_tag(name);
}

Hidden Procedure tc_tlocal(name, number) value name, number; {
	tc_tag(name);
}

Hidden Procedure tc_tglobal(name) value name; {
	tc_tag(name);
}

Hidden Procedure tc_tmystery(name, number) value name, number; {
	tc_tag(name);
}

Hidden Procedure tc_trefinement(name) value name; {
	tc_tag(name);
}

Hidden Procedure tc_tfun(name, fct) value name, fct; {
	tc_tag(name);
}

Hidden Procedure tc_tprd(name, fct) value name, fct; {
	tc_tag(name);
}

Hidden Procedure tc_monprd(name, r, pred) parsetree r; value name, pred; {
	p_release(pt_expr(r));
}

Hidden Procedure tc_dyaprd(l, name, r, pred) parsetree l, r; value name, pred; {
	polytype tl, tr;
	tl = pt_expr(l);
	tr = pt_expr(r);
	if (is_string(name, "in") || is_string(name, "not'in")) {
		polytype t_tlt_l, u;
		t_tlt_l = mkt_tlt(p_copy(tl));
		unify(tr, t_tlt_l, &u);
		p_release(t_tlt_l); p_release(u);
	}
	p_release(tl); p_release(tr);
}

Forward polytype pt_relop();

Hidden Procedure tc_relop(l, r) parsetree l, r; {
	p_release(pt_relop(l, r));
}

Hidden polytype pt_relop(l, r) parsetree l, r; {
	polytype tl, tr, u;

	if (Comparison(nodetype(l)))
		tl = pt_relop(*Branch(l, REL_LEFT), *Branch(l, REL_RIGHT));
	else
		tl = pt_expr(l);
	tr = pt_expr(r);
	unify(tl, tr, &u);
	p_release(tl); p_release(tr);
	return u;
}

Hidden Procedure tc_unparsed(c, t) parsetree c, t; {}

Hidden Procedure uni_bad() { syserr(MESS(2304, "bad uni node in type check")); }
Hidden Procedure cmd_bad() { syserr(MESS(2305, "bad cmd node in type check")); }
Hidden polytype exp_bad() { syserr(MESS(2306, "bad exp node in type check"));
			    return (polytype) 0; }
Hidden Procedure tes_bad() { syserr(MESS(2307, "bad tes node in type check")); }

Visible Procedure inittyp() {
	int i;
	for (i= 0; i<TABSIZE; i++) {
		 uni_tab[i]= uni_bad;
		 cmd_tab[i]= cmd_bad;
		 exp_tab[i]= exp_bad;
		 tes_tab[i]= tes_bad;
	}

	uni_tab[HOW_TO]=	tc_howto_unit;
	uni_tab[YIELD]=		tc_yield_unit;
	uni_tab[TEST]=		tc_test_unit;
	uni_tab[REFINEMENT]=	tc_refinement;

	cmd_tab[SUITE]=	  	tc_suite;
	cmd_tab[PUT]=	   	tc_put;
	cmd_tab[INSERT]=	tc_ins_rem;
	cmd_tab[REMOVE]=	tc_ins_rem;
	cmd_tab[CHOOSE]=	tc_choose;
	cmd_tab[DRAW]=	   	tc_draw;
	cmd_tab[SET_RANDOM]=  	tc_set_random;
	cmd_tab[DELETE]=	tc_delete;
	cmd_tab[CHECK]=	   	tc_check;
	cmd_tab[SHARE]=	   	tc_nothing;
	cmd_tab[WRITE]=	   	tc_write;
	cmd_tab[READ]=	   	tc_read;
	cmd_tab[READ_RAW]=	tc_raw_read;
	cmd_tab[IF]=	   	tc_ifwhile;
	cmd_tab[WHILE]=	   	tc_ifwhile;
	cmd_tab[FOR]=	   	tc_for;
	cmd_tab[SELECT]=	tc_select;
	cmd_tab[TEST_SUITE]=  	tc_tes_suite;
	cmd_tab[ELSE]=	   	tc_else;
	cmd_tab[QUIT]=	   	tc_nothing;
	cmd_tab[RETURN]=	tc_return;
	cmd_tab[REPORT]=	tc_report;
	cmd_tab[SUCCEED]=	tc_nothing;
	cmd_tab[FAIL]=	   	tc_nothing;
	cmd_tab[USER_COMMAND]=	tc_user_command;
	cmd_tab[EXTENDED_COMMAND]= tc_nothing;
	exp_tab[TAG]=		pt_tag;
	tes_tab[TAG]=		tc_tag;
	exp_tab[TAGformal]=	pt_tformal;
	tes_tab[TAGformal]=	tc_tformal;
	exp_tab[TAGlocal]=	pt_tlocal;
	tes_tab[TAGlocal]=	tc_tlocal;
	exp_tab[TAGglobal]=	pt_tglobal;
	tes_tab[TAGglobal]=	tc_tglobal;
	exp_tab[TAGmystery]=	pt_tmystery;
	tes_tab[TAGmystery]=	tc_tmystery;
	exp_tab[TAGrefinement]=	pt_trefinement;
	tes_tab[TAGrefinement]=	tc_trefinement;
	exp_tab[TAGzerfun]=	pt_tfun;
	tes_tab[TAGzerfun]=	tc_tfun;
	exp_tab[TAGzerprd]=	pt_tprd;
	tes_tab[TAGzerprd]=	tc_tprd;
	
	exp_tab[COMPOUND]=	pt_compound;
	tes_tab[COMPOUND]=	tc_compound;
	exp_tab[COLLATERAL]=	pt_collateral;
	exp_tab[SELECTION]=	pt_selection;
	exp_tab[BEHEAD]=	pt_trim;
	exp_tab[CURTAIL]=	pt_trim;

	exp_tab[UNPARSED]=	pt_unparsed;
	tes_tab[UNPARSED]=	tc_unparsed;
	
	exp_tab[MONF]=	   	pt_monf;
	exp_tab[DYAF]=	   	pt_dyaf;
	exp_tab[NUMBER]=	pt_number;
	exp_tab[TEXT_DIS]=	pt_text_dis;
	exp_tab[ELT_DIS]=	pt_elt_dis;
	exp_tab[LIST_DIS]=	pt_list_dis;
	exp_tab[RANGE_DIS]=   	pt_range_dis;
	exp_tab[TAB_DIS]=	pt_tab_dis;
	
	tes_tab[AND]=	   	tc_junction;
	tes_tab[OR]=	   	tc_junction;
	tes_tab[NOT]=	   	tc_not;
	tes_tab[SOME_IN]=	tc_in_quantification;
	tes_tab[EACH_IN]=	tc_in_quantification;
	tes_tab[NO_IN]=	   	tc_in_quantification;
	tes_tab[SOME_PARSING]=	tc_p_quantification;
	tes_tab[EACH_PARSING]=	tc_p_quantification;
	tes_tab[NO_PARSING]=  	tc_p_quantification;
	tes_tab[MONPRD]=	tc_monprd;
	tes_tab[DYAPRD]=	tc_dyaprd;
	tes_tab[LESS_THAN]=   	tc_relop;
	tes_tab[AT_MOST]=	tc_relop;
	tes_tab[GREATER_THAN]=	tc_relop;
	tes_tab[AT_LEAST]=	tc_relop;
	tes_tab[EQUAL]=	   	tc_relop;
	tes_tab[UNEQUAL]=	tc_relop;
}
