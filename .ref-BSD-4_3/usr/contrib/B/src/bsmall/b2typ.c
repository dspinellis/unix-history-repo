/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2typ.c,v 1.1 84/06/28 00:49:27 timo Exp $ */

/* Type matching */
#include "b.h"
#include "b1obj.h"
#include "b2env.h"
#include "b2sem.h"
#include "b2typ.h"

#define Tnil ((btype) Vnil)

Forward btype valtype();

/* All the routines in this file are temporary */
/* Thus length() and empty() have been put here too */

Visible int length(v) value v; {
	value s= size(v);
	int len= intval(s);
	release(s);
	return len;
}

Visible bool empty(v) value v; {
	value s= size(v);
	bool b= large(s) || intval(s)!=0;
	release(s);
	return !b;
}

Visible btype loctype(l) loc l; {
	value *ll;
	if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		if (!in_env(sl->e->tab, sl->i, &ll)) return Tnil;
		return valtype(*ll);
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l);
		btype tt= loctype(tl->R), associate;
		if (tt == Tnil) return Tnil;
		if (!empty(tt)) associate= th_of(one, tt);
		else associate= Tnil;
		release(tt);
		return associate;
	} else if (Is_trimloc(l)) {
		return mk_text("");
	} else if (Is_compound(l)) {
		btype ct= mk_compound(Nfields(l)); intlet k, len= Nfields(l);
		k_Overfields { put_in_field(loctype(*field(l, k)), &ct, k); }
		return ct;
	} else {
		syserr("loctype asked of non-target");
		return Tnil;
	}
}

Visible btype valtype(v) value v; {
	if (Is_number(v)) return mk_integer(0);
	else if (Is_text(v)) return mk_text("");
	else if (Is_compound(v)) {
		btype ct= mk_compound(Nfields(v)); intlet k, len= Nfields(v);
		k_Overfields { put_in_field(valtype(*field(v, k)), &ct, k); }
		return ct;
	} else if (Is_ELT(v)) {
		return mk_elt();
	} else if (Is_list(v)) {
		btype tt= mk_elt(), vt, ve;
		if (!empty(v)) {
			insert(vt= valtype(ve= min1(v)), &tt);
			release(vt); release(ve);
		}
		return tt;
	} else if (Is_table(v)) {
		btype tt= mk_elt(), vk, va;
		if (!empty(v)) {
			vk= valtype(*key(v, 0));
			va= valtype(*assoc(v, 0));
			replace(va, &tt, vk);
			release(vk); release(va);
		}
		return tt;
	} else {
		syserr("valtype called with unknown type");
		return Tnil;
	}
}

Visible must_agree(t, u, m) btype t, u; string m; {
	intlet k, len;
	value vt, vu;
	if (t == Tnil || u == Tnil || t == u) return;
	if ((Is_number(t) && Is_number(u))
	 || (Is_text(t) && Is_text(u))
	 || (Is_ELT(u) && (Is_ELT(t) || Is_list(t) || Is_table(t)))
	 || (Is_ELT(t) && (Is_ELT(u) || Is_list(u) || Is_table(u)))) return;
	else if (Is_compound(t) && Is_compound(u)) {
		if ((len= Nfields(t)) != Nfields(u)) error(m);
		else k_Overfields { must_agree(*field(t,k), *field(u,k), m); }
	} else {
		if (Is_list(t) && Is_list(u)) {
			if (!empty(t) && !empty(u)) {
				must_agree(vt= min1(t), vu= min1(u), m);
				release(vt); release(vu);
			}
		} else if (Is_table(t) && Is_table(u)) {
			if (!empty(t) && !empty(u)) {
				must_agree(*key(t, 0), *key(u, 0), m);
				must_agree(*assoc(t, 0), *assoc(u, 0), m);
			}
		} else error(m);
	}
}
