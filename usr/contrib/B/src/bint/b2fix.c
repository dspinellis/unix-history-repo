/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/* $Header: b2fix.c,v 1.4 85/08/22 16:55:08 timo Exp $ */

/* Fix unparsed expr/test */

#include "b.h"
#include "b1obj.h"
#include "b2exp.h"
#include "b2nod.h"
#include "b2gen.h" /* Must be after b2nod.h */
#include "b2par.h" /* For is_b_tag */
#include "b3err.h"
#include "b3env.h"
#include "b3sem.h"

Forward parsetree fix_expr(), fix_test();

Visible Procedure f_eunparsed(pt) parsetree *pt; {
	f_unparsed(pt, fix_expr);
}

Visible Procedure f_cunparsed(pt) parsetree *pt; {
	f_unparsed(pt, fix_test);
}

Hidden Procedure f_unparsed(pt, fct) parsetree *pt, (*fct)(); {
	parsetree t= *pt; unpadm adm;
	struct state v;
	/* Ignore visits done during resolving UNPARSED: */
	hold(&v);
	initunp(&adm, *Branch(t, UNP_SEQ));
	t= (*fct)(&adm);
	release(*pt);
	*pt= t;
	jumpto(NilTree);
	let_go(&v);
}

/* ********************************************************************	*/

#define Fld		*Field(Node(adm), N_fld(adm))
#define Is_fld		(N_fld(adm) < Nfields(Node(adm)))
#define Get_fld(v)	v= copy(Fld); N_fld(adm)++

Hidden Procedure initunp(adm, root) unpadm *adm; value root; {
	Prop(adm)= No;
	Node(adm)= root;
	N_fld(adm)= 0;
}

/* ********************************************************************	*/

Hidden bool f_dyafun(v, s, fct) value v, *fct; string s; {
	value t= Vnil; 
	bool is= Is_text(v) && compare(v, t= mk_text(s)) == 0 && is_dyafun(v, fct);
	release(t);
	return is;
}

Hidden bool f_dyatag(v, fct) value v, *fct; {
	return Is_text(v) && is_b_tag(v) && is_dyafun(v, fct);
}

Visible bool is_b_tag(v) value v; {
	value a, b, c; bool x;
	/* REPORT v|1 in {'a' .. 'z'} */
	a= mk_charrange(b= mk_text("a"), c= mk_text("z"));
	release(b); release(c);
	x= in(b= curtail(v, one), a);
	release(a); release(b);
	return x;
}

/* ********************************************************************	*/

Hidden Procedure fix_formula(adm, v, fct, lev, right)
	unpadm *adm; parsetree *v, (*right)(); value fct; intlet lev; {

	parsetree w; value name;
	if (Level(adm) < lev) fixerr(Prio);
	Get_fld(name);
	w= (*right)(adm);
	if (Trim(adm)) *v= node3(b_behead(name) ? BEHEAD : CURTAIL, *v, w);
	else *v= node5(DYAF, *v, name, w, copy(fct));
}

/* ********************************************************************	*/

Hidden bool b_expr_opr(v, fct) value v, *fct; {
	return	f_dyafun(v, "^^", fct) || f_dyafun(v, "><", fct) ||
		f_dyafun(v, "<<", fct) || f_dyafun(v, ">>", fct) ||
		f_dyatag(v, fct);
}

Forward parsetree fix_term(), fix_factor(), fix_primary(), fix_base();

Hidden parsetree fix_expr(adm) unpadm *adm; {
	parsetree v; value fct;
	if (!Is_fld) {
		fixerr(MESS(4700, "no expression where expected"));
		return NilTree;
	}
	v= fix_term(adm);
	if (Is_fld && b_expr_opr(Fld, &fct)) {
		if (nodetype(v) == DYAF) fixerr(Prio);
		fix_formula(adm, &v, fct, L_expr, fix_base);
	}
	if (Is_fld && !Prop(adm)) {
		value f;
		if (Is_text(Fld) && is_dyafun(Fld, &f)) fixerr(Prio);
		else fixerr(MESS(4701, "something unexpected following expression"));
	}
	return v;
}

Hidden parsetree fix_test(adm) unpadm *adm; {
	parsetree v; value w= Vnil, f= Vnil; value *aa;
	if (!Is_fld) {
		fixerr(MESS(4702, "no test where expected"));
		return NilTree;
	}
	if (Is_text(Fld)) {
		Get_fld(v);
		if (is_zerprd(v, &f)) {
			if (Is_fld)
				fixerr(MESS(4703, "something unexpected following test"));
			return node3(TAGzerprd, v, copydef(f));
		} else if (aa= envassoc(refinements, v)) {
			if (!Is_fld) return node3(TAGrefinement, v, copy(*aa));
		} else if (is_monprd(v, &f)) 
			return node4(MONPRD, v, fix_expr(adm), copydef(f));
		release(v);
		N_fld(adm)--;
	}
	Prop(adm)= Yes;
	v= fix_expr(adm);
	Prop(adm)= No;
	if (!(Is_fld && Is_text(Fld) && is_dyaprd(Fld, &f)))
		fixerr(MESS(4704, "no test where expected"));
	if (Is_fld) Get_fld(w);
	return node5(DYAPRD, v, w, fix_expr(adm), copydef(f));
}

/* ********************************************************************	*/

Hidden bool b_term_opr(v, fct) value v, *fct; {
	return	f_dyafun(v, "+", fct) || f_dyafun(v, "-", fct) ||
		f_dyafun(v, "^", fct);
}

Hidden parsetree fix_term(adm) unpadm *adm; {
	parsetree v; value fct;
	v= fix_factor(adm);
	while (Is_fld && b_term_opr(Fld, &fct))
		fix_formula(adm, &v, fct, L_term, fix_factor);
	return v;
}

/* ********************************************************************	*/

Hidden parsetree fix_factor(adm) unpadm *adm; {
	parsetree v; value fct;
	v= fix_primary(adm);
	while (Is_fld && f_dyafun(Fld, "*", &fct))
		fix_formula(adm, &v, fct, L_factor, fix_primary);
	if (Is_fld && f_dyafun(Fld, "/", &fct))
		fix_formula(adm, &v, fct, L_factor, fix_primary);
	return v;
}

/* ********************************************************************	*/

Hidden parsetree fix_primary(adm) unpadm *adm; {
	parsetree v; value fct;
	v= fix_base(adm);
	if (Is_fld && f_dyafun(Fld, "#", &fct))
		fix_formula(adm, &v, fct, L_number, fix_base);
	if (Is_fld && f_dyafun(Fld, "**", &fct))
		fix_formula(adm, &v, fct, L_power, fix_base);
	return v;
}

/* ********************************************************************	*/

Forward parsetree fix_rbase();

Hidden parsetree fix_base(adm) unpadm *adm; {
	Level(adm)= L_expr;
	Trim(adm)= No;
	return fix_rbase(adm);
}

Forward parsetree fix_monadic();

Hidden parsetree fix_rbase(adm) unpadm *adm; {
	parsetree v, w= NilTree; value f;
	if (!Is_fld && !Prop(adm)) {
		fixerr(MESS(4705, "no expression where expected"));
		return NilTree;
	}
	if (Is_parsetree(Fld)) {
		f_expr(Branch(Node(adm), N_fld(adm)));
		Get_fld(v);
		fix_trim(adm, &v);
		return v;
	}
	Get_fld(v);
	if (modify_tag(v, &w)) fix_trim(adm, &w);
	else if (is_monfun(v, &f)) w= fix_monadic(adm, v, f);
	else {
		fixerr2(v, MESS(4706, " has not yet received a value"));
		release(v);
	}
	return w;
}

Hidden Procedure adjust_level(adm, lev)	unpadm *adm; intlet lev; {
	if (lev < Level(adm)) Level(adm)= lev;
}

Hidden parsetree fix_monadic(adm, v, fct) unpadm *adm; value v, fct; {
	if (!Trim(adm)) {
		if (b_minus(v)) adjust_level(adm, L_factor); 
		else if (b_number(v)) adjust_level(adm, L_power); 
		else if (!(b_plus(v) || b_about(v))) 
			adjust_level(adm, L_bottom);
	}
	if (!Trim(adm) && b_minus(v)) {
		intlet lev= Level(adm);
		parsetree t= node4(MONF, v, fix_primary(adm), copydef(fct));
		adjust_level(adm, lev);
		return t;
	} else 
		return node4(MONF, v, fix_rbase(adm), copydef(fct));
}

Hidden Procedure fix_trim(adm, v) unpadm *adm; parsetree *v; {
	if (!Trim(adm)) {
		Trim(adm)= Yes;
		while (Is_fld && (b_behead(Fld) || b_curtail(Fld)))
			fix_formula(adm, v, Vnil, L_bottom, fix_rbase);
		Trim(adm)= No;
	}
}
