/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3in2.c,v 1.4 85/08/22 16:58:21 timo Exp $
*/

/* B interpreter -- independent subroutines */

#include "b.h"
#include "b1obj.h"
#include "b3env.h"
#include "b3in2.h"
#include "b3sem.h"
#include "b3sou.h"

/* Making ranges */

Hidden value c_range(lo, hi) value lo, hi; {
	char a, z;
	if (!character(lo))
		error(MESS(3400, "in {p..q}, p is a text but not a character"));
	else if (!Is_text(hi))
		error(MESS(3401, "in {p..q}, p is a text, but q is not"));
	else if (!character(hi))
		error(MESS(3402, "in {p..q}, q is a text, but not a character"));
	else {
		a= charval(lo); z= charval(hi);
		if (z < a-1) error(MESS(3403, "in {p..q}, character q < x < p"));
		else return mk_charrange(lo, hi);
	}
	return Vnil;
}

Hidden value i_range(lo, hi) value lo, hi; {
	value entries, res= Vnil;
	if (!integral(lo))
		error(MESS(3404, "in {p..q}, p is a number but not an integer"));
	else if (!Is_number(hi))
		error(MESS(3405, "in {p..q}, p is a number but q is not"));
	else if (!integral(hi))
		error(MESS(3406, "in {p..q}, q is a number but not an integer"));
	else {
		entries= diff(lo, hi);
		if (compare(entries, one)>0)
			error(MESS(3407, "in {p..q}, integer q < x < p"));
		else res= mk_numrange(lo, hi);
		release(entries);
	}
	return res;
}

Visible value mk_range(v1, v2) value v1, v2; {
	value r= Vnil;
	if (Is_text(v1)) r= c_range(v1, v2);
	else if (Is_number(v1)) r= i_range(v1, v2);
	else error(MESS(3408, "in {p..q}, p is neither a text nor a number"));
	return r;
}


/* Newlines for WRITE /// */

Visible Procedure nl(n) value n; {
	value l= size(n); int c= intval(l); release(l);
	while (c--) newline();
}


/* Evaluating basic targets */

Visible value v_local(name, number) value name, number; {
	value *aa= envassoc(curnv->tab, number);
	if (aa != Pnil && *aa != Vnil) return copy(*aa);
	error3(0, name, MESS(3409, " has not yet received a value"));
	return Vnil;
}

Visible value v_global(name) value name; {
	value *aa= envassoc(prmnv->tab, name);
	if (aa != Pnil && *aa != Vnil) return copy(tarvalue(name, *aa));
	error3(0, name, MESS(3410, " has not yet received a value"));
	return Vnil;
}


/* Locating mysteries */

Visible loc l_mystery(name, number) value name, number; {
	if (Is_compound(curnv->tab)) return local_loc((basidf) number);
	return global_loc(name);
}


/* Rangers */

/* An IN-ranger is represented on the stack as a compound of three fields:
   the last index used, the value of the expression after IN, and its length.
   (The latter is redundant, but saves save many calls of 'size()'.)
   When first called, there is, of course, no compound on the stack, but only
   the value of the expression.  As the expression should always be a text,
   list or table, this is recognizable as a special case, and then the
   compound is created.
   Return value is Yes if a new element was available and assigned, No if not.
*/

Visible bool in_ranger(l, pv) loc l; value *pv; {
	value v= *pv, ind, tlt, len, i1, val; bool res;
	if (!Is_compound(v) || Nfields(v) != 3) { /* First time */
		tlt= v;
		if (!Is_tlt(tlt)) {
			error(MESS(3411, "in ... i IN e, e is not a text, list or table"));
			return No;
		}
		if (empty(tlt)) return No;
		*pv= v= mk_compound(3);
		*Field(v, 0)= ind= one;
		*Field(v, 1)= tlt;
		*Field(v, 2)= len= size(tlt);
		bind(l);
	}
	else {
		ind= *Field(v, 0); tlt= *Field(v, 1); len= *Field(v, 2);
		res= numcomp(ind, len) < 0;
		if (!res) { unbind(l); return No; }
		*Field(v, 0)= ind= sum(i1= ind, one); release(i1);
	}
	put(val= th_of(ind, tlt), l); release(val);
	return Yes;
}


/* PARSING-rangers are treated similarly to IN-rangers, but here the
   compound contains the last parse (i.e., N texts). */

Visible bool pa_ranger(l, pv) loc l; value *pv; {
	value v= *pv, e, f; int len, k;
	if (!Is_compound(v)) {
		if (!Is_text(v)) {
			error(MESS(3412, "in  ... i PARSING e, e is not a text"));
			return No;
		}
		if (!Is_compound(l)) {
			error(
		MESS(3413, "in ... i PARSING e, i is not a collateral identifier"));
			return No;
		}
		v= mk_compound(len= Nfields(l));
		*Field(v, len-1)= *pv;
		*Field(v, 0)= e= mk_text("");
		for (k= 1; k < len-1; ++k)
			*Field(v, k)= copy(e);
		*pv= v;
		bind(l);
		put(v, l);
		return Yes;
	}
	uniql(pv); v= *pv;
	len= Nfields(v);
	for (k= len-1; k > 0; --k) {
		if (!empty(f= *Field(v, k))) {
			value head, tail, prev, newprev, two= sum(one, one);
			head= curtail(f, one); tail= behead(f, two);
			release(f);
			newprev= concat(prev= *Field(v, k-1), head);
			release(prev); release(head);
			*Field(v, k-1)= newprev;
			if (k < len-1)
				*Field(v, k)= *Field(v, len-1);
			*Field(v, len-1)= tail;
			put(v, l);
			return Yes;
		}
	}
	unbind(l);
	return No;
}
