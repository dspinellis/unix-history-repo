/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2uni.c,v 1.4 85/08/22 16:57:24 timo Exp $
*/

#include "b.h"
#include "b0fea.h"
#include "b1obj.h"
#include "b2par.h"
#include "b2key.h"
#include "b2syn.h"
#include "b2nod.h"
#include "b3env.h"
#include "b3err.h"
#include "b3sou.h" /* for permkey() */

/* ******************************************************************** */
/*		unit							*/
/* ******************************************************************** */

Visible bool unit_keyword() {
	bool b; txptr tx0= tx;
	b= how_to_keyword() || yield_keyword() || test_keyword();
	tx= tx0;
	return b;
}

Hidden value formlist, sharelist;
Hidden envtab reftab; 
Visible literal idf_cntxt;

Forward bool is_howto_unit(), is_yield_unit(), is_test_unit();
Forward parsetree unicmd_suite(), ref_suite();

Visible parsetree unit(heading) bool heading; {
	parsetree v= NilTree;
	if (!heading) {
		lino= 1;
		cntxt= In_unit;
		release(uname); uname= Vnil;
	}
	if (!is_howto_unit(&v, heading) &&
	    !is_yield_unit(&v, heading) &&
	    !is_test_unit(&v, heading)
	   )
		parerr(MESS(2800, "no unit keyword where expected"));
#ifdef TYPE_CHECK
	if (!heading) type_check(v);
#endif
	return v;
}

/* ******************************************************************** */
/*		howto_unit						*/
/* ******************************************************************** */

Forward value hu_formals();

Hidden bool is_howto_unit(v, heading) parsetree *v; bool heading; {
	if (how_to_keyword()) {
		value kw, w, f;
		txptr ftx, ttx;
		if (cur_ilev != 0) parerr(MESS(2801, "unit starts with indentation"));
		formlist= mk_elt(); 
		skipsp(&tx);
		kw= keyword(); 
		release(uname); uname= permkey(kw, How);
		if (in(kw, kwlist)) pprerr2(kw, MESS(2802, " is a reserved keyword"));
		req(":", ceol, &ftx, &ttx);
		idf_cntxt= In_formal;
		f= hu_formals(ftx, kw); tx= ttx;
		if (!is_comment(&w)) w= Vnil;
		*v= node8(HOW_TO, copy(kw), f, w, NilTree, NilTree, Vnil, Vnil);
		if (!heading) {
			sharelist= mk_elt();
			*Branch(*v, HOW_SUITE)= unicmd_suite();
			reftab= mk_elt();
			*Branch(*v, HOW_REFINEMENT)= ref_suite();
			*Branch(*v, HOW_R_NAMES)= reftab;
			release(sharelist);
		}
		release(formlist); 
		return Yes;
	}
	return No;
}

Hidden value hu_formals(q, kw) txptr q; value kw; {
	value t, v, w;
	skipsp(&tx);
	if (Text(q) && is_tag(&t)) treat_idf(t);
	else t= Vnil;
	skipsp(&tx);
	v= Text(q) ? hu_formals(q, keyword()) : Vnil;
	w= node4(FORMAL, kw, t, v);
	return w;
}

/* ******************************************************************** */
/*		yield_unit						*/
/* ******************************************************************** */

Forward parsetree ytu_formals();

Hidden bool is_yield_unit(v, heading) parsetree *v; bool heading; {
	if (yield_keyword()) {
		parsetree f; value name, w, adicity;
		txptr ftx, ttx;
		if (cur_ilev != 0) parerr(MESS(2803, "unit starts with indentation"));
		formlist= mk_elt(); 
		skipsp(&tx);
		req(":", ceol, &ftx, &ttx);
		f= ytu_formals(ftx, 'y', &name, &adicity); tx= ttx;
		if (!is_comment(&w)) w= Vnil;
		*v= node9(YIELD, copy(name), adicity, f, w, NilTree,
			  NilTree, Vnil, Vnil);
		if (!heading) {
			sharelist= mk_elt();
			*Branch(*v, FPR_SUITE)= unicmd_suite();
			reftab= mk_elt();
			*Branch(*v, FPR_REFINEMENT)= ref_suite();
			*Branch(*v, FPR_R_NAMES)= reftab;
			release(sharelist);
		}
		release(formlist); 
		return Yes;
	}
	return No;
}

/* ******************************************************************** */
/*		test_unit						*/
/* ******************************************************************** */

Hidden bool is_test_unit(v, heading) parsetree *v; bool heading; {
	if (test_keyword()) {
		parsetree f; value name, w, adicity;
		txptr ftx, ttx;
		if (cur_ilev != 0) parerr(MESS(2804, "unit starts with indentation"));
		formlist= mk_elt();
		skipsp(&tx);
		req(":", ceol, &ftx, &ttx);
		f= ytu_formals(ftx, 't', &name, &adicity); tx= ttx;
		if (!is_comment(&w)) w= Vnil;
		*v= node9(TEST, copy(name), adicity, f, w, NilTree,
			  NilTree, Vnil, Vnil);
		if (!heading) {
			sharelist= mk_elt();
			*Branch(*v, FPR_SUITE)= unicmd_suite();
			reftab= mk_elt();
			*Branch(*v, FPR_REFINEMENT)= ref_suite();
			*Branch(*v, FPR_R_NAMES)= reftab;
			release(sharelist);
		}
		release(formlist); 
		return Yes;
	}
	return No;
}

/* ******************************************************************** */

#define FML_IN_FML MESS(2805, " is already a formal parameter or operand")
#define SH_IN_FML  MESS(2806, " is already a formal parameter")
#define SH_IN_SH   MESS(2807, " is already a shared identifier")
#define REF_IN_FML MESS(2808, " is already a formal parameter")
#define REF_IN_SH  MESS(2809, " is already a shared identifier")
#define REF_IN_REF MESS(2810, " is already a refinement name")

Hidden Procedure treat_idf(t) value t; {
	switch (idf_cntxt) {
		case In_formal:	if (in(t, formlist)) pprerr2(t, FML_IN_FML);
				insert(t, &formlist);
				break;
		case In_share:	if (in(t, formlist)) pprerr2(t, SH_IN_FML);
				if (in(t, sharelist)) pprerr2(t, SH_IN_SH);
				insert(t, &sharelist);
				break;
		case In_ref:	if (in(t, formlist)) pprerr2(t, REF_IN_FML);
				if (in(t, sharelist)) pprerr2(t, REF_IN_SH);
				break;
		case In_ranger: break;
		default:	break;
	}
}

Forward parsetree fml_operand();

Hidden parsetree ytu_formals(q, yt, name, adic)
	txptr q; char yt; value *name, *adic; {

	parsetree v1, v2, v3;
	*name= Vnil;
	idf_cntxt= In_formal;
	v1= fml_operand(q);
	skipsp(&tx);
	if (!Text(q)) { /* zeroadic */
		*adic= zero; 
		if (nodetype(v1) == TAG) {
			*name= *Branch(v1, TAG_NAME);
			release(uname); uname= permkey(*name, Zer);
	 	} else
			pprerr(MESS(2811, "user defined functions must be tags"));
		return v1;
	}

	v2= fml_operand(q);
	skipsp(&tx);
	if (!Text(q)) { /* monadic */
		*adic= one; 
		if (nodetype(v1) == TAG) {
			*name= *Branch(v1, TAG_NAME);
			release(uname); uname= permkey(*name, Mon);
		} else
			pprerr(MESS(2812, "no monadic function name"));
		if (nodetype(v2) == TAG) treat_idf(*Branch(v2, TAG_NAME));
		return node4(yt == 'y' ? MONF : MONPRD, *name, v2, Vnil);
	}

	v3= fml_operand(q);
	/* dyadic */
	*adic= mk_integer(2);
	if (nodetype(v2) == TAG) {
		*name= *Branch(v2, TAG_NAME);
		release(uname); uname= permkey(*name, Dya);
	} else
		pprerr(MESS(2813, "no dyadic function name"));
	upto(q, "dyadic formal formula");
	if (nodetype(v1) == TAG) treat_idf(*Branch(v1, TAG_NAME));
	if (nodetype(v3) == TAG) treat_idf(*Branch(v3, TAG_NAME));
	return node5(yt == 'y' ? DYAF : DYAPRD, v1, *name, v3, Vnil);
}

Hidden parsetree fml_operand(q) txptr q; {
	value t;
	skipsp(&tx);
	if (nothing(q, "formal operand")) return NilTree;
	else if (is_tag(&t)) return node2(TAG, t);
	else if (open_sign()) return compound(q, idf);
	else {
		parerr(MESS(2814, "no formal operand where expected"));
		tx= q;
		return NilTree;
	}
}

/* ******************************************************************** */
/*		unit_command_suite					*/
/* ******************************************************************** */

Forward parsetree ucmd_seq();

Forward bool share();

Hidden parsetree unicmd_suite() {
	if (ateol()) 
		return ucmd_seq(0, Yes);
	else {
		parsetree v; value c; intlet l= lino;
		suite_command(&v, &c);
		return node5(SUITE, mk_integer(l), v, c, NilTree);
	}
}

Hidden parsetree ucmd_seq(cil, first) intlet cil; bool first; {
	value c; intlet level, l;
	level= ilev(); l= lino;
	if (is_comment(&c)) 
		return node5(SUITE, mk_integer(l), NilTree, c,
				ucmd_seq(cil, first));
	if ((level == cil && !first) || (level > cil && first)) {
		parsetree v;
		findceol();
		if (share(ceol, &v, &c)) 
			return node5(SUITE, mk_integer(l), v, c,
					ucmd_seq(level, No));
		veli();
		return cmd_suite(cil, first);
	}
	veli();
	return NilTree;
} 

Hidden bool share(q, v, c) txptr q; parsetree *v; value *c; {
	if (share_keyword()) {
		idf_cntxt= In_share;
		*v= node2(SHARE, idf(q));
		*c= tail_line();
		return Yes;
	}
	return No;
}


/* ******************************************************************** */
/*		refinement_suite					*/
/* ******************************************************************** */

Hidden parsetree ref_suite() {
	value name; bool t;
	if (ilev() > 0) {
		parerr(MESS(2815, "indentation where not allowed"));
		return NilTree;
	}
	if ((t= is_tag(&name)) || is_keyword(&name)) {
		parsetree v, s; value w, *aa, r;
		skipsp(&tx);
		if (Char(tx) != ':') {
			release(name);
			tx= fcol();
			veli(); return NilTree;
		}
		/* lino= 1; cntxt= In_ref; */
		tx++;
		if (t) {
			idf_cntxt= In_ref;
			treat_idf(name);
		}
		if (in_env(reftab, name, &aa)) pprerr2(name, REF_IN_REF);
		findceol();
		if (!is_comment(&w)) w= Vnil;
		s= cmd_suite(0, Yes);
		v= node6(REFINEMENT, name, w, s, Vnil, Vnil);
		e_replace(r= mk_ref(v), &reftab, name);
		release(r);
		*Branch(v, REF_NEXT)= ref_suite();
		return v;
	} 
	veli();
	return NilTree;
}

/* ******************************************************************** */
/*		collateral, compound					*/
/* ******************************************************************** */

Hidden parsetree n_collateral(q, n, base)
	txptr q; intlet n; parsetree (*base)(); {

	parsetree v, w; txptr ftx, ttx;
	if (find(",", q, &ftx, &ttx)) {
		w= (*base)(ftx); tx= ttx;
		v= n_collateral(q, n+1, base);
	} else {
		w= (*base)(q);
		if (n == 1) return w;
		v= mk_compound(n);
	}
	*Field(v, n-1)= w;
	return n > 1 ? v : node2(COLLATERAL, v);
}

Visible parsetree collateral(q, base) txptr q; parsetree (*base)(); {
	return n_collateral(q, 1, base);
}

Visible parsetree compound(q, base) txptr q; parsetree (*base)(); {
	parsetree v; txptr ftx, ttx;
	req(")", q, &ftx, &ttx);
	v= (*base)(ftx); tx= ttx;
	return node2(COMPOUND, v);
}

/* ******************************************************************** */
/*		idf, singidf						*/
/* ******************************************************************** */

Hidden parsetree singidf(q) txptr q; {
	parsetree v;
	skipsp(&tx);
	if (nothing(q, "identifier"))
		v= NilTree;
	else if (open_sign())
		v= compound(q, idf);
	else if (is_tag(&v)) {
		treat_idf(v);
		v= node2(TAG, v);
	} else {
		parerr(MESS(2816, "no identifier where expected"));
		v= NilTree;
	}
	upto(q, "identifier");
	return v;
}

Visible parsetree idf(q) txptr q; {
	return collateral(q, singidf);
}
