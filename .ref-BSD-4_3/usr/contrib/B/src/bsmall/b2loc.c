/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2loc.c,v 1.1 84/06/28 00:49:16 timo Exp $ */

/* B locations and environments */
#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b2env.h" /* for bndtgs */
#include "b2sem.h"

Hidden value* location(l) loc l; {
	value *ll;
	if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		if (!in_env(sl->e->tab, sl->i, &ll)) error("target still empty");
		return ll;
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l);
		ll= adrassoc(*location(tl->R), tl->K);
		if (ll == Pnil) error("key not in table");
		return ll;
	} else {
		syserr("call of location with improper type");
		return (value *) Dummy;
	}
}

Hidden Procedure uniquify(l) loc l; {
	if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		value *ta= &(sl->e->tab), ke= sl->i;
		uniql(ta);
		check_location(l);
		uniq_assoc(*ta, ke);
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l);
		value t, ke;
		uniquify(tl->R);
		t= *location(tl->R); ke= tl->K;
		if (!Is_table(t)) error("selection on non-table");
		if (empty(t)) error("selection on empty table");
		check_location(l);
		uniq_assoc(t, ke);
	} else if (Is_trimloc(l)) { syserr("uniquifying trimloc");
	} else if (Is_compound(l)) { syserr("uniquifying comploc");
	} else syserr("uniquifying non-location");
}

Visible Procedure check_location(l) loc l; {
	VOID location(l);
	/* location may produce an error message */
}

Visible value content(l) loc l; {
	return copy(*location(l));
}

Visible loc trim_loc(R, B, C) loc R; intlet B, C; {
	if (Is_trimloc(R)) {
		trimloc *rr= Trimloc(R);
		return mk_trimloc(rr->R, B, C);
	} else if (Is_simploc(R) || Is_tbseloc(R)) {
		return mk_trimloc(R, B, C);
	} else {
		error("trim (@ or |) on target of improper type");
		/* NOTREACHED */
	}
}

Visible loc tbsel_loc(R, K) loc R; value K; {
	if (Is_simploc(R) || Is_tbseloc(R)) return mk_tbseloc(R, K);
	else error("selection on target of improper type");
	/* NOTREACHED */
}

Visible loc local_loc(i) basidf i; { return mk_simploc(i, curnv); }

Visible loc global_loc(i) basidf i; { return mk_simploc(i, prmnv); }

Visible Procedure put(v, l) value v; loc l; {
	if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		e_replace(v, &(sl->e->tab), sl->i);
	} else if (Is_trimloc(l)) {
		trimloc *tl= Trimloc(l);
		value rr, nn, head, tail, part;
		intlet B= tl->B, C= tl->C, len;
		rr= *location(tl->R);
		if (!Is_text(rr)) error("trim target contains no text");
		if (!Is_text(v))
		    error("putting non-text in trim(@ or|) on text location");
		len= length(rr);
		if (B < 0 || C < 0 || B+C > len)
		    error("trim (@ or |) on text location out of bounds");
		head= trim(rr, 0, len-B); /* rr|B */
		tail= trim(rr, len-C, 0); /* rr@(#rr-C+1) */
		part= concat(head, v);
		nn= concat(part, tail);
		put(nn, tl->R);
		release(nn); release(head); release(tail); release(part);
	} else if (Is_compound(l)) {
		intlet k, len= Nfields(l);
		if (!Is_compound(v))
		    error("putting non-compound in compound location");
		if (Nfields(v) != Nfields(l))
		    error("putting compound in compound location of different length");
		k_Overfields { put(*field(v, k), *field(l, k)); }
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l);
		uniquify(tl->R);
		replace(v, location(tl->R), tl->K);
	} else error("putting in non-target");
}

Hidden bool l_exists(l) loc l; {
	if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		return in_keys(sl->i, sl->e->tab);
	} else if (Is_trimloc(l)) {
		error("deleting trimmed (@ or |) target");
		return No;
	} else if (Is_compound(l)) {
		intlet k, len= Nfields(l);
		k_Overfields { if (!l_exists(*field(l, k))) return No; }
		return Yes;
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l);
		uniquify(tl->R);
		return in_keys(tl->K, *location(tl->R));
	} else {
		error("deleting non-target");
		return No;
	}
}

Hidden Procedure l_del(l) loc l; {
	if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		if (in_keys(sl->i, sl->e->tab)) {
			uniql(&(sl->e->tab)); /*no need?: see delete*/
			e_delete(&(sl->e->tab), sl->i);
		}
	} else if (Is_trimloc(l)) {
		error("deleting trimmed (@ or |) target");
	} else if (Is_compound(l)) {
		intlet k, len= Nfields(l);
		k_Overfields { l_del(*field(l, k)); }
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l);
		value *lc;
		uniquify(tl->R);
		lc= location(tl->R);
		if (in_keys(tl->K, *lc)) delete(lc, tl->K);
	} else error("deleting non-target");
}

Visible Procedure l_delete(l) loc l; {
	if (l_exists(l)) l_del(l);
	else error("deleting non-existent target");
}

Visible Procedure l_insert(v, l) value v; loc l; {
	value *ll;
	uniquify(l);
	ll= location(l);
	insert(v, ll);
}

Visible Procedure l_remove(v, l) value v; loc l; {
	uniquify(l);
	remove(v, location(l));
}

Visible Procedure choose(l, v) loc l; value v; {
	value w, s, r;
	if (!Is_tlt(v)) error("choosing from non-text, -list or -table");
	s= size(v);
	if (compare(s, zero) == 0)
		error("choosing from empty text, list or table");
	/* PUT (floor(random*#v) + 1) th'of v IN l */
	r= prod(w= random(), s); release(w); release(s);
	w= floorf(r); release(r);
	r= sum(w, one); release(w);
	put(w= th_of(r, v), l); release(w);
}

Visible Procedure draw(l) loc l; {
	value r= random();
	put(r, l);
	release(r);
}

Visible Procedure bind(l) loc l; {
	if (Is_simploc(l)) {
		simploc *ll= Simploc(l);
		if (!in(ll->i, *bndtgs)) /* kludge */
			insert(ll->i, bndtgs);
	} else if (Is_compound(l)) {
		intlet k, len= Nfields(l);
		k_Overfields { bind(*field(l, k)); }
	} else if (Is_trimloc(l)) {
		pprerr("t@p or t|p not allowed in ranger", "");
	} else if (Is_tbseloc(l)) {
		pprerr("t[e] not allowed in ranger", "");
	} else error("binding non-identifier");
}
