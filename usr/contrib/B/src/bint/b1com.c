/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1com.c,v 1.4 85/08/22 16:48:13 timo Exp $
*/

/************************************************************************/
/* B compounds                                                          */
/* plus Hows Funs and other odd types that don't fit anywhere else      */
/*                                                                      */
/* A compound is modelled as a sequence of len values, its fields.      */
/*                                                                      */
/************************************************************************/

#include "b.h"
#include "b1obj.h"
#include "b2nod.h"

/* NODES */

Visible typenode nodetype(v) parsetree v; {
	return Is_parsetree(v) ? Nodetype(v) : Nonode;
}

Hidden value
mk_ptn(type, len) /* make parsetree node */
	typenode type;
	intlet len;
{
	parsetree v= (parsetree) grab_ptn((len<<8) | type);
	*Branch(v, len)= *Branch(v, len+1)= NilTree;
	return v;
}

Visible parsetree
node1(type)
	typenode type;
{
	return mk_ptn(type, 0);
}

Visible parsetree
node2(type, a1)
	typenode type; value a1;
{
	parsetree v= mk_ptn(type, 1); value *p= Ats(v);
	*p++= a1;
	return v;
}

Visible parsetree
node3(type, a1, a2)
	typenode type; value a1, a2;
{
	parsetree v= mk_ptn(type, 2); value *p= Ats(v);
	*p++= a1; *p++= a2;
	return v;
}

Visible parsetree
node4(type, a1, a2, a3)
	typenode type; value a1, a2, a3;
{
	parsetree v= mk_ptn(type, 3); value *p= Ats(v);
	*p++= a1; *p++= a2; *p++= a3;
	return v;
}

Visible parsetree
node5(type, a1, a2, a3, a4)
	typenode type; value a1, a2, a3, a4;
{
	parsetree v= mk_ptn(type, 4); value *p= Ats(v);
	*p++= a1; *p++= a2; *p++= a3; *p++= a4;
	return v;
}

Visible parsetree
node6(type, a1, a2, a3, a4,a5)
	typenode type; value a1, a2, a3, a4, a5;
{
	parsetree v= mk_ptn(type, 5); value *p= Ats(v);
	*p++= a1; *p++= a2; *p++= a3; *p++= a4; *p++= a5;
	return v;
}

Visible parsetree
node8(type, a1, a2, a3, a4, a5, a6, a7)
	typenode type; value a1, a2, a3, a4, a5, a6, a7;
{
	parsetree v= mk_ptn(type, 7); value *p= Ats(v);
	*p++= a1; *p++= a2; *p++= a3; *p++= a4; *p++= a5; *p++= a6; *p++= a7;
	return v;
}

Visible parsetree
node9(type, a1, a2, a3, a4, a5, a6, a7, a8)
	typenode type; value a1, a2, a3, a4, a5, a6, a7, a8;
{
	parsetree v= mk_ptn(type, 8); value *p= Ats(v);
	*p++= a1; *p++= a2; *p++= a3; *p++= a4; *p++= a5; *p++= a6;
	*p++= a7; *p++= a8;
	return v;
}

/* OTHER TYPES */

Visible loc
mk_simploc(id, en)
	basidf id; env en;
{
	loc l= grab_sim();
	(*Ats(l))= copy(id); (*(Ats(l)+1))= (value) en;
	return l;
}

Visible loc
mk_trimloc(R, B, C)
	loc R; value B, C;
{
	loc l= grab_tri(); trimloc *ll= (trimloc *)Ats(l);
	ll->R= copy(R); ll->B= copy(B); ll->C= copy(C);
	return l;
}

Visible loc
mk_tbseloc(R, K)
	loc R; value K;
{
	loc l= grab_tse(); tbseloc *ll= (tbseloc *)Ats(l);
	ll->R= copy(R); ll->K= copy(K);
	return l;
}

Visible fun
mk_fun(adic, pre, unit, filed)
	literal adic; intlet pre; parsetree unit; bool filed;
{
	fun f= grab_fun(); funprd *ff= (funprd *)Ats(f);
	ff->adic= adic; ff->pre= pre; ff->unit= unit;
	ff->unparsed= Yes; ff->filed= filed;
	ff->code= NilTree;
	return f;
}

Visible prd
mk_prd(adic, pre, unit, filed)
	literal adic; intlet pre; parsetree unit; bool filed;
{
	prd p= grab_prd(); funprd *pp= (funprd *)Ats(p);
	pp->adic= adic; pp->pre= pre; pp->unit= unit;
	pp->unparsed= Yes; pp->filed= filed;
	pp->code= NilTree;
	return p;
}

Visible value
mk_how(unit, filed)
	parsetree unit; bool filed;
{
	value h= grab_how(); how *hh= (how *)Ats(h);
	hh->unit= unit; hh->unparsed= Yes; hh->filed= filed;
	hh->code= NilTree;
	return h;
}

Visible value
mk_ref(rp)
	parsetree rp;
{
	value r= grab_ref();
	*Ats(r)= copy(rp);
	return r;
}

Visible value
mk_per(v)
	value v;
{
	value p= grab_per();
	*Ats(p)= copy(v);
	return p;
}
