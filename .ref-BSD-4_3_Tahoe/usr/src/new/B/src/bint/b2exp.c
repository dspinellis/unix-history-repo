/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2exp.c,v 1.4 85/08/22 16:54:36 timo Exp $
*/

#include "b.h"
#include "b1obj.h"
#include "b2par.h"
#include "b2syn.h"
#include "b2nod.h"
#include "b2exp.h"
#include "b3err.h"

/* ******************************************************************** */
/*		expression						*/
/* ******************************************************************** */

Visible parsetree expr(q) txptr q; {
	return collateral(q, singexpr);
}

Forward parsetree rsingexpr();

Visible parsetree singexpr(q) txptr q; {
	if (nothing(q, "expression")) return NilTree;
	else {
		expadm adm;
		initexp(&adm);
		return rsingexpr(q, &adm);
	}
}

Hidden Procedure initexp(adm) expadm *adm; {
	Parsed(adm)= Yes;
	N_fld(adm)= 0;
	Prop(adm)= dya_proposition;
	dya_proposition= No;
}

Hidden bool expr_opr() {
	return reptext_sign() || center_sign() || leftadj_sign() ||
		rightadj_sign();
}

Forward parsetree term(), factor(), primary(), base(), unp_expr();
Forward bool element();

Hidden parsetree rsingexpr(q, adm) txptr q; expadm *adm; {
	parsetree v; value w; txptr tx0= tx;
	v= term(q, adm);
	skipsp(&tx);
	if (Parsed(adm) && Text(q) && expr_opr()) {
		if (nodetype(v) == DYAF) pprerr(Prio);
		dya_formula(q, adm, &v, mk_text(textsign), L_expr, base);
	}
	skipsp(&tx);
	if (Parsed(adm) && Prop(adm)) {
		if (Text(q) && (nodetype(v) == DYAF || Level(adm) < L_expr))
			/* predicate must follow */
			return v; 
		else if (Text(q) && tag_operator(q, &w))
			dya_formula(q, adm, &v, w, L_expr, unp_expr);
		else
			parerr(MESS(2100, "no test where expected"));
	}
	if (Parsed(adm) && Text(q) && tag_operator(q, &w)) {
		if (nodetype(v) == DYAF) pprerr(Prio);
		dya_formula(q, adm, &v, w, L_expr, base);
	}
	if (!Parsed(adm)) /* v is an UNPARSED node */
		*Branch(v, UNP_TEXT)= cr_text(tx0, tx);
	upto_expr(q);
	return v;
}

Hidden Procedure dya_formula(q, adm, v, name, lev, fct)
	txptr q; expadm *adm; parsetree *v, (*fct)(); value name; intlet lev; {

	parsetree w;
	if (Level(adm) < lev) pprerr(Prio);
	N_fld(adm)+= 2;
	w= (*fct)(q, adm);
	if (Parsed(adm)) {
		N_fld(adm)-= 2;
		if (Trim(adm))
			*v= node3(b_behead(name) ? BEHEAD : CURTAIL, *v, w);
		else
			*v= node5(DYAF, *v, name, w, Vnil);
	} else {
		*Field(Unp_comp(adm), --N_fld(adm))= name;
		*Field(Unp_comp(adm), --N_fld(adm))= *v;
		*v= w;
	}
}

/* ******************************************************************** */
/*		term							*/
/* ******************************************************************** */

Hidden bool term_opr() {
	return plus_sign() || minus_sign() || join_sign();
}

Hidden parsetree term(q, adm) txptr q; expadm *adm; {
	parsetree v= factor(q, adm);
	skipsp(&tx);
	while (Parsed(adm) && Text(q) && term_opr()) {
		dya_formula(q, adm, &v, mk_text(textsign), L_term, factor);
		skipsp(&tx);
	}
	return v;
}

/* ******************************************************************** */
/*		factor							*/
/* ******************************************************************** */

Hidden parsetree factor(q, adm) txptr q; expadm *adm; {
	parsetree v= primary(q, adm);
	skipsp(&tx);
	while (Parsed(adm) && Text(q) && times_sign()) {
		dya_formula(q, adm, &v, mk_text(textsign), L_factor, primary);
		skipsp(&tx);
	}
	if (Parsed(adm) && Text(q) && over_sign())
		dya_formula(q, adm, &v, mk_text(textsign), L_factor, primary);
	return v;
}

/* ******************************************************************** */
/*		primary							*/
/* ******************************************************************** */

Hidden parsetree primary(q, adm) txptr q; expadm *adm; {
	parsetree v;
	v= base(q, adm);
	skipsp(&tx);
	if (Parsed(adm) && Text(q) && number_sign())
		dya_formula(q, adm, &v, mk_text(textsign), L_number, base);
	skipsp(&tx);
	if (Parsed(adm) && Text(q) && power_sign())
		dya_formula(q, adm, &v, mk_text(textsign), L_power, base);
	return v;
}

/* ******************************************************************** */
/*		base							*/
/* ******************************************************************** */

Forward parsetree rbase();

Hidden parsetree base(q, adm) txptr q; expadm *adm; {
	State(adm)= S_else;
	Level(adm)= L_expr;
	Trim(adm)= No;
	return rbase(q, adm);
}

Hidden bool critical(adm, v) expadm *adm; value v; {
	if (State(adm) == S_t) {
		if (b_plus(v) || b_minus(v))
			return Level(adm) >= L_term;
		if (b_number(v))
			return Level(adm) >= L_number;
	}
	return No;
}

Hidden parsetree mon_formula(q, adm, w, fct)
	txptr q; expadm *adm; value w; parsetree (*fct)(); {

	parsetree v;
	N_fld(adm)++;
	v= (*fct)(q, adm);
	if (Parsed(adm)) {
		N_fld(adm)--;
		return v == NilTree ? node2(TAG, w) : node4(MONF, w, v, Vnil);
	} else {
		*Field(Unp_comp(adm), --N_fld(adm))= w;
		return v;
	}
}

Hidden Procedure adjust_level(adm, lev) expadm *adm; intlet lev; {
	if (lev < Level(adm)) Level(adm)= lev;
}

Hidden parsetree rbase(q, adm) txptr q; expadm *adm; {
	parsetree v; value name;
	skipsp(&tx);
	if (Text(q) && tag_operator(q, &name)) {
		if (State(adm) == S_tt)
			return mon_formula(q, adm, name, unp_expr);
		if (State(adm) == S_t) {
			if (Level(adm) == L_expr || Prop(adm)) State(adm)= S_tt;
			else if (!Trim(adm)) adjust_level(adm, L_bottom); 
		} else State(adm)= S_t;
		v= mon_formula(q, adm, name, rbase);
		if (!Trim(adm) && Parsed(adm) && nodetype(v) == MONF) 
			adjust_level(adm, L_bottom);
		return v;
	} else if (Text(q) && (dyamon_sign() || mon_sign())) {
		name= mk_text(textsign);
		if (State(adm) == S_tt || critical(adm, name))
			return mon_formula(q, adm, name, unp_expr);
		if (!Trim(adm)) {
			if (State(adm) == S_t) adjust_level(adm, L_bottom);
			else if (b_minus(name)) adjust_level(adm, L_factor);
			else if (b_number(name)) adjust_level(adm, L_number);
			else if (b_numtor(name) || b_denomtor(name)) 
				adjust_level(adm, L_bottom);
		}
		State(adm)= S_else;
		if (!Trim(adm) && b_minus(name)) {
			intlet lev= Level(adm);
			v= mon_formula(q, adm, name, primary);
			adjust_level(adm, lev);
			return v;
		} else
			return mon_formula(q, adm, name, rbase);
	} else if (Text(q) && element(q, &v)) {
		if (State(adm) == S_tt)
			return mon_formula(q, adm, v, unp_expr);
		exp_trimmed_text(q, adm, &v);
		return v;
	} else {
		if (State(adm) == S_else) 
			parerr(MESS(2101, "no expression where expected"));
		return NilTree;
	}
}

/* ******************************************************************** */
/*		element							*/
/* ******************************************************************** */

Forward bool closed_expr(), constant(), text_dis(), tlr_dis(), seltrim_tag();

Hidden bool element(q, v) txptr q; parsetree *v; {
	if (seltrim_tag(q, v) || closed_expr(q, v) || constant(q, v) ||
	    text_dis(q, v) || tlr_dis(q, v)
	   ) {
		selection(q, v);
		return Yes;
	}
	return No;
}

/* ******************************************************************** */
/*		(seltrim_tag)						*/
/* ******************************************************************** */

Hidden bool seltrim_tag(q, v) txptr q; parsetree *v; {
	value name; txptr tx0= tx;
	if (Text(q) && is_tag(&name)) {
		txptr tx1= tx;
		skipsp(&tx);
		if (Text(q) && (sub_sign() || trim_sign())) {
			tx= tx1;
			*v= node2(TAG, name);
			return Yes;
		} else {
			release(name);
			tx= tx0;
		}
	}
	return No;
}

/* ******************************************************************** */
/*		(expression)						*/
/* ******************************************************************** */

Hidden bool closed_expr(q, v) txptr q; parsetree *v; {
	return open_sign() ? (*v= compound(q, expr), Yes) : No;
}

/* ******************************************************************** */
/*		constant						*/
/*									*/
/* note: stand_alone E<number> not allowed				*/
/* ******************************************************************** */

Forward bool digits();

Hidden bool constant(q, v) txptr q; parsetree *v; {
	if (Dig(Char(tx)) || Char(tx) == '.') {
		txptr tx0= tx;
		bool d= digits(q);
		if (Text(q) && point_sign() && !digits(q) && !d)
			pprerr(MESS(2102, "point without digits"));
		if (Text(q) && Char(tx) == 'E' &&
		    (Dig(Char(tx+1)) || !keymark(Char(tx+1)))
		   ) {
			tx++;
			if (Text(q) && (plus_sign() || minus_sign()));
			if (!digits(q)) pprerr(MESS(2103, "E not followed by exponent"));
		}
		*v= node3(NUMBER, numconst(tx0, tx), cr_text(tx0, tx));
		return Yes;
	}
	return No;
}

Hidden bool digits(q) txptr q; {
	txptr tx0= tx;
	while (Text(q) && Dig(Char(tx))) tx++;
	return tx > tx0;
}

/* ******************************************************************** */
/*		textual_display						*/
/* ******************************************************************** */

Forward parsetree text_body();

Hidden bool text_dis(q, v) txptr q; parsetree *v; {
	if (apostrophe_sign() || quote_sign()) {
		parsetree w; value aq= mk_text(textsign);
		w= text_body(q, textsign);
		if (w == NilTree) w= node3(TEXT_LIT, mk_text(""), NilTree);
		*v= node3(TEXT_DIS, aq, w);
		return Yes;
	}
	return No;
}

Forward bool is_conversion();

Hidden parsetree text_body(q, aq) txptr q; string aq; {
	value head; parsetree tail;
	txptr tx0= tx;
	while (Text(q)) {
		if (Char(tx) == *aq || Char(tx) == '`') {
			head= tx0 < tx ? cr_text(tx0, tx) : Vnil;
			if (Char(tx) == Char(tx+1)) {
				value spec= cr_text(tx, tx+1);
				tx+= 2;
				tail= text_body(q, aq);
				tail= node3(TEXT_LIT, spec, tail);
			} else {
				parsetree e;
				if (is_conversion(q, &e)) {
					tail= text_body(q, aq);
					tail= node3(TEXT_CONV, e, tail);
				} else {
					tx++;
					tail= NilTree;
				}
			}
			if (head == Vnil) return tail;
			else return node3(TEXT_LIT, head, tail);
		} else
			tx++;
	}
	parerr2(MESS(2104, "cannot find matching "), MESSMAKE(aq));
	return NilTree;
}

Hidden bool is_conversion(q, v) txptr q; parsetree *v; {
	if (conv_sign()) {
		txptr ftx, ttx;
		req("`", q, &ftx, &ttx);
		*v= expr(ftx); tx= ttx; 
		return Yes;
	}
	return No;
}

/* ******************************************************************** */
/*		table_display; list_display; range_display;		*/
/* ******************************************************************** */

Hidden bool elt_dis(v) parsetree *v; {
	if (curlyclose_sign()) {
		*v= node1(ELT_DIS);
		return Yes;
	}
	return No;
}

Hidden bool range_dis(q, v) txptr q; parsetree *v; {
	txptr ftx, ttx;
	if (find("..", q, &ftx, &ttx)) {
		parsetree w;
		if (Char(ttx) == '.') { ftx++; ttx++; }
		w= singexpr(ftx); tx= ttx;
		*v= node3(RANGE_DIS, w, singexpr(q));
		return Yes;
	}
	return No;
}

Forward value tab_comp();

Hidden bool tab_dis(q, v) txptr q; parsetree *v; {
	if (Char(tx) == '[') {
		*v= node2(TAB_DIS, tab_comp(q, 1));
		return Yes;
	}
	return No;
}

Hidden value tab_comp(q, n) txptr q; intlet n; {
	value v; parsetree key, assoc; txptr ftx, ttx;
	if (find(";", q, &ftx, &ttx)) {
		tab_elem(ftx, &key, &assoc); tx= ttx;
		v= tab_comp(q, n+2);
	} else {
		tab_elem(q, &key, &assoc);
		v= mk_compound(n+1);
	}
	*Field(v, n-1)= key;
	*Field(v, n)= assoc;
	return v;
}

Hidden Procedure tab_elem(q, key, assoc) txptr q; parsetree *key, *assoc; {
	txptr ftx, ttx;
	need("[");
	req("]", q, &ftx, &ttx);
	*key= expr(ftx); tx= ttx;
	need(":");
	*assoc= singexpr(q);
}

Forward value list_comp();

Hidden Procedure list_dis(q, v) txptr q; parsetree *v; {
	*v= node2(LIST_DIS, list_comp(q, 1));
}

Hidden value list_comp(q, n) txptr q; intlet n; {
	value v; parsetree w; txptr ftx, ttx;
	if (find(";", q, &ftx, &ttx)) {
		w= singexpr(ftx); tx= ttx;
		v= list_comp(q, n+1);
	} else {
		w= singexpr(q);
		v= mk_compound(n);
	}
	*Field(v, n-1)= w;
	return v;
}

Hidden bool tlr_dis(q, v) txptr q; parsetree *v; {
	if (curlyopen_sign()) {
		skipsp(&tx);
		if (!elt_dis(v)) {
			txptr ftx, ttx;
			req("}", q, &ftx, &ttx);
			if (!range_dis(ftx, v)) {
				skipsp(&tx);
				if (!tab_dis(ftx, v)) list_dis(ftx, v);
			}
			tx= ttx;
		}
		return Yes;
	}
	return No;
}

/* ******************************************************************** */
/*		selection						*/
/* ******************************************************************** */

Visible Procedure selection(q, v) txptr q; parsetree *v; {
	txptr ftx, ttx;
	skipsp(&tx);
	while (Text(q) && sub_sign()) {
		req("]", q, &ftx, &ttx);
		*v= node3(SELECTION, *v, expr(ftx)); tx= ttx;
		skipsp(&tx);
	}
}

/* ******************************************************************** */
/*		trimmed_text						*/
/* ******************************************************************** */

Hidden bool is_trimmed_text(q) txptr q; {
	txptr tx0= tx; bool b;
	skipsp(&tx);
	b= Text(q) && trim_sign();
	tx= tx0;
	return b;
}

Hidden Procedure trimmed_text(q, adm, v) txptr q; expadm *adm; parsetree *v; {
	Trim(adm)= Yes;
	while (Parsed(adm) && Text(q) && trim_sign()) {
		State(adm)= S_else;
		dya_formula(q, adm, v, mk_text(textsign), L_bottom, rbase);
		skipsp(&tx);
	}
	Trim(adm)= No;
}

Visible Procedure tar_trimmed_text(q, v) txptr q; parsetree *v; {
	if (is_trimmed_text(q)) {
		expadm adm;
		initexp(&adm);
		Level(&adm)= L_bottom;
		trimmed_text(q, &adm, v);
	}
}

Hidden Procedure exp_trimmed_text(q, adm, v)
	txptr q; expadm *adm; parsetree *v; {

	if (!Trim(adm) && is_trimmed_text(q)) {
		intlet s= State(adm); /* save */
		if (State(adm) == S_t) adjust_level(adm, L_bottom); 
		trimmed_text(q, adm, v);
		State(adm)= s; /* restore */
	}
}

/* ******************************************************************** */
/*		unp_expr, unp_test 					*/
/* ******************************************************************** */

Forward bool item();

Hidden parsetree unp_expr(q, adm) txptr q; expadm *adm; {
	value v;
	skipsp(&tx);
	if (Text(q) && item(q, &v)) {
		return mon_formula(q, adm, v, unp_expr);
	} else {
		Parsed(adm)= No;
		Unp_comp(adm)= mk_compound(N_fld(adm));
		return node3(UNPARSED, Unp_comp(adm), Vnil);
	}
}

Visible parsetree unp_test(q) txptr q; {
	parsetree v; expadm adm; txptr tx0= tx;
	initexp(&adm);
	v= unp_expr(q, &adm);
	*Branch(v, UNP_TEXT)= cr_text(tx0, tx);
	return v;
}

Visible bool tag_operator(q, v) txptr q; value *v; {
	txptr tx0= tx;
	if (Text(q) && is_tag(v)) {
		skipsp(&tx);
		if (!(Text(q) && (sub_sign() || trim_sign()))) return Yes;
		else {
			release(*v);
			tx= tx0;
		}
	}
	return No;
}

Hidden bool dm_operator(q, v) txptr q; value *v; {
	return dyamon_sign() ? (*v= mk_text(textsign), Yes) : tag_operator(q, v);
}

Hidden bool d_operator(q, v) txptr q; value *v; {
	return dya_sign() ? (*v= mk_text(textsign), Yes) : dm_operator(q, v);
}

Hidden bool m_operator(q, v) txptr q; value *v; {
	return mon_sign() ? (*v= mk_text(textsign), Yes) : dm_operator(q, v);
}

Hidden bool trim_operator(q, v) txptr q; value *v; {
	return trim_sign() ? (*v= mk_text(textsign), Yes) : No;
}

Hidden bool item(q, v) txptr q; value *v; {
	return  tag_operator(q, v) || trim_operator(q, v) ||
		d_operator(q, v) || m_operator(q, v) ||
		element(q, v);
}

/* ********************************************************************	*/
/*		upto_expr						*/
/* ********************************************************************	*/

Hidden Procedure upto_expr(q) txptr q; {
	skipsp(&tx);
	if (Text(q)) {
		value dum;
		if (d_operator(q, &dum)) {
			release(dum);
			pprerr(Prio);
		} else parerr(MESS(2105, "something unexpected following expression"));
		tx= q;
	}
}

/* ********************************************************************	*/

Hidden bool is_opr(v, s) value v; string s; {
	value t= Vnil;
	bool is= Is_text(v) && compare(v, t= mk_text(s)) == 0;
	release(t);
	return is;
}

Visible bool b_about(v) value v;	{ return is_opr(v, "~"); }
Visible bool b_numtor(v) value v; 	{ return is_opr(v, "*/"); }
Visible bool b_denomtor(v) value v; 	{ return is_opr(v, "/*"); }
Visible bool b_plus(v) value v; 	{ return is_opr(v, "+"); }
Visible bool b_minus(v) value v; 	{ return is_opr(v, "-"); }
Visible bool b_number(v) value v; 	{ return is_opr(v, "#"); }
Visible bool b_behead(v) value v; 	{ return is_opr(v, "@"); }
Visible bool b_curtail(v) value v; 	{ return is_opr(v, "|"); }
#ifdef NOT_USED
Visible bool b_times(v) value v; 	{ return is_opr(v, "*"); }
Visible bool b_over(v) value v; 	{ return is_opr(v, "/"); }
Visible bool b_power(v) value v; 	{ return is_opr(v, "**"); }
Visible bool b_join(v) value v;		{ return is_opr(v, "^"); }
Visible bool b_reptext(v) value v; 	{ return is_opr(v, "^^"); }
Visible bool b_center(v) value v; 	{ return is_opr(v, "><"); }
Visible bool b_leftadj(v) value v; 	{ return is_opr(v, "<<"); }
Visible bool b_rightadj(v) value v; 	{ return is_opr(v, ">>"); }
#endif
