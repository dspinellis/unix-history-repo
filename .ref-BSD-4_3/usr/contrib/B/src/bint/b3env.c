/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3env.c,v 1.4 85/08/22 16:57:42 timo Exp $
*/

/* Environments */

#include "b.h"
#include "b1obj.h"
#include "b3err.h" /*for curline, curlino*/

Visible envtab prmnvtab;
Visible envchain prmnvchain;
Visible env prmnv;

/* context: */
/* The bound tags for the current environment are stored in *bndtgs */
/* A new bound tag list is created on evaluating a refined test or expression */

Visible env curnv;
Visible value *bndtgs;
Hidden value bndtglist;
Visible literal cntxt, resexp;
Visible value uname= Vnil;
Visible intlet lino;
Visible intlet f_lino;

Visible context read_context;

Visible Procedure sv_context(sc) context *sc; {
	sc->curnv= curnv;
	sc->bndtgs= bndtgs;
	sc->cntxt= cntxt;
	sc->resexp= resexp;
	sc->uname= copy(uname);
	sc->cur_line= curline;
	sc->cur_lino= curlino;
}

Visible Procedure set_context(sc) context *sc; {
	curnv= sc->curnv;
	bndtgs= sc->bndtgs;
	cntxt= sc->cntxt;
	resexp= sc->resexp;
	release(uname); uname= sc->uname;
	curline= sc->cur_line;
	curlino= sc->cur_lino;
}

Visible Procedure initenv() {
	/* The following invariant must be maintained:
	   EITHER:
	      the original permanent-environment table resides in prmnv->tab
	      and prmnvtab == Vnil
	   OR:
	      the original permanent-environment table resides in prmnvtab
	      and prmnv->tab contains a scratch-pad copy.
	*/
	prmnv= &prmnvchain;
	prmnv->tab= mk_elt(); prmnvtab= Vnil;
	prmnv->inv_env= Enil;
	bndtglist= mk_elt();
}

Visible Procedure endenv() {
	release(prmnv->tab); prmnv->tab= Vnil;
	release(bndtglist); bndtglist= Vnil;
	release(uname); uname= Vnil;
	release(erruname); erruname= Vnil;
}

Visible Procedure re_env() {
	setprmnv(); bndtgs= &bndtglist;
}

Visible Procedure setprmnv() {
	/* the current and permanent environment are reset
	   to the original permanent environment */
	if (prmnvtab != Vnil) {
		prmnv->tab= prmnvtab;
		prmnvtab= Vnil;
	}
	curnv= prmnv;
}

Visible Procedure e_replace(v, t, k) value v, *t, k; {
	if (Is_compound(*t)) {
		int n= SmallIntVal(k);
		uniql(t);
		if (*Field(*t, n) != Vnil) release(*Field(*t, n));
		*Field(*t, n)= copy(v);
	}
	else if (!Is_table(*t)) syserr(MESS(2900, "replacing in non-environment"));
	else replace(v, t, k);
}

Visible Procedure e_delete(t, k) value *t, k; {
	if (Is_compound(*t) && IsSmallInt(k)) {
		int n= SmallIntVal(k);
		if (*Field(*t, n) != Vnil) {
			uniql(t); release(*Field(*t, n));
			*Field(*t, n)= Vnil;
		}
	}
	else if (!Is_table(*t)) syserr(MESS(2901, "deleting from non-environment"));
	else if (in_keys(k, *t)) delete(t, k);
}

Visible value* envassoc(t, ke) value t, ke; {
	if (Is_compound(t) && IsSmallInt(ke)) {
		int n= SmallIntVal(ke);
		if (*Field(t, n) == Vnil) return Pnil;
		return Field(t, n);
	}
	if (!Is_table(t)) syserr(MESS(2902, "selection on non-environment"));
	return adrassoc(t, ke);
}

Visible bool in_env(tab, ke, aa) value tab, ke, **aa; {
	/* IF ke in keys tab:
		PUT tab[ke] IN aa
		SUCCEED
	   FAIL
	*/
	*aa= envassoc(tab, ke);
	return (*aa != Pnil);
}

Visible Procedure extbnd_tags(btl, et) value btl; envtab et; {
	/* Copy bound targets to the invoking environment */
	/* FOR tag IN btl: \ btl is the bound tag list
	       IF tag in keys et: \ et is the environment we're just leaving
	           PUT et[tag] IN curnv[tag] \ curnv is the invoking environment
	*/
	value *aa, tag;
	int len= length(btl), k;
	for (k= 1; k <= len; k++) {
		tag= thof(k, btl);
		if (in_env(et, tag, &aa)) {
			e_replace(*aa, &(curnv->tab), tag);
			if (*bndtgs != Vnil) insert(tag, bndtgs);
		}
		release(tag);
	}
}

Visible Procedure lst_ttgs() {
	int k, len;
	len= length(prmnv->tab);
	k_Over_len {
		writ(*key(prmnv->tab, k));
		wri_space();
	}
	newline();
}
