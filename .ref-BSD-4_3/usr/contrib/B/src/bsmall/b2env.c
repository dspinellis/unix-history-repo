/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2env.c,v 1.1 84/06/28 00:49:06 timo Exp $ */

/* Environments */
#include "b.h"
#include "b1obj.h"

envtab prmnvtab;
envchain prmnvchain;
env prmnv;

/* context: */
env curnv; value *bndtgs; value bndtglist;
literal cntxt, resexp; value uname; literal utype;
intlet cur_ilev, lino; txptr tx, ceol;

context read_context;
context how_context;

bool xeq= Yes;

Visible Procedure sv_context(sc) context *sc; {
	sc->curnv= curnv;
	sc->bndtgs= bndtgs;
	sc->cntxt= cntxt;
	sc->resexp= resexp;
	sc->uname= uname;
	sc->utype= utype;
	sc->cur_ilev= cur_ilev;
	sc->lino= lino;
	sc->tx= tx;
	sc->ceol= ceol;
}

Visible Procedure set_context(sc) context *sc; {
	curnv= sc->curnv;
	bndtgs= sc->bndtgs;
	cntxt= sc->cntxt;
	resexp= sc->resexp;
	uname= sc->uname;
	utype= sc->utype;
	cur_ilev= sc->cur_ilev;
	lino= sc->lino;
	tx= sc->tx;
	ceol= sc->ceol;
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
	if (!Is_table(*t)) syserr("replacing in non-environment");
	else replace(v, t, k);
}

Visible Procedure e_delete(t, k) value *t, k; {
	if (!Is_table(*t)) syserr("deleting from non-environment");
	if (in_keys(k, *t)) delete(t, k);
}

Visible value* envassoc(t, ke) value t, ke; {
	if (!Is_table(t)) syserr("selection on non-environment");
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

Visible Procedure extbnd_tags(btl, en, et) value btl; envtab *en, et; {
	/* FOR v IN btl:
	       IF v in keys et:
	           PUT et[v] IN en[v]
	*/
	value *aa, v;
	int len= length(btl), k;
	for (k= 1; k <= len; k++) {
		v= thof(k, btl);
		if (in_env(et, v, &aa)) e_replace(*aa, en, v);
		release(v);
	}
}

Visible Procedure restore_env(e0) env e0; {
	/*not yet implemented*/
}

Visible value* lookup(t) value t; {
	return envassoc(curnv->tab, t);
}

