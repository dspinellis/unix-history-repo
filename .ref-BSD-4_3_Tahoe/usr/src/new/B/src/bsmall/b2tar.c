/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2tar.c,v 1.1 84/06/28 00:49:23 timo Exp $ */

/* B target locating */
#include "b.h"
#include "b1obj.h"
#include "b2env.h"
#include "b2syn.h"
#include "b2sem.h"

Visible loc statrimloc(l, v) loc l; value v; {
	/* temporary, while no static type check */
	return (loc) mk_elt();
}

Visible loc statbseloc(l, k) loc l; value k; {
	/* temporary, while no static type check */
	return (loc) mk_elt();
}

Visible loc targ(q) txptr q; {
	value c; loc l; txptr i, j; intlet len, k;
	if ((len= 1+count(",", q)) == 1) return bastarg(q);
	c= mk_compound(len);
	k_Overfields {
		if (!Lastfield(k)) req(",", q, &i, &j);
		else i= q;
		l= bastarg(i);
		put_in_field(l, &c, k);
		if (!Lastfield(k)) tx= j;
	}
	return (loc) c;
}

Visible loc bastarg(q) txptr q; {
	loc l; txptr i, j; value k;
	Skipsp(tx);
	nothing(q, "target");
	if (Char(tx) == '(') {
		tx++; req(")", q, &i, &j);
		l= targ(i); tx= j;
	} else if (Letter(Char(tx))) {
		value t= tag(), *aa;
		aa= lookup(t);
		if (aa == Pnil) l= local_loc(t);
		else if (Is_refinement(*aa))
			pprerr("refined targets are not allowed", "");
		else if (Is_formal(*aa)) {
			l= loc_formal(*aa);
		} else if (Is_shared(*aa))
			l= global_loc(t);
		else l= local_loc(t);
		release(t);
	} else parerr("no target where expected", "");
	Skipsp(tx);
	while (tx < q && Char(tx) == '[') {
		if (xeq) check_location(l);
		tx++; req("]", q, &i, &j);
		k= expr(i); tx= j;
		if (xeq) {
			loc ll= l;
			l= tbsel_loc(l, k);
			release(k); release(ll);
		} else l= statbseloc(l, k);
		Skipsp(tx);
	}
	if (tx < q && (Char(tx) == '@' || Char(tx) == '|')) {
		value v= xeq ? content(l) : Vnil; intlet B, C;
		if (xeq && !Is_text(v))
		  error("in the target t@p or t|p, t does not contain a text");
		trimbc(q, xeq ? length(v) : 0, &B, &C);
		release(v);
		if (xeq) l= trim_loc(l, B, C);
		else l= statrimloc(l, k);
		Skipsp(tx);
	}
	if (tx < q) parerr(Char(tx) == ',' ? "comma not allowed here" :
					"garbage following target", "");
	return l;
}
