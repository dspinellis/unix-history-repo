/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3loc.c,v 1.4 85/08/27 10:56:45 timo Exp $
*/

/* B locations and environments */
#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b3env.h" /* for bndtgs */
#include "b3sem.h"
#include "b3sou.h" /* for tarvalue() */
#include "b3err.h" /* for still_ok */

Hidden value* location(l) loc l; {
	value *ll;
	if (Is_locloc(l)) {
		if (!in_env(curnv->tab, l, &ll))
			error(MESS(3600, "target not initialised"));
		return ll;
	} else if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		if (!in_env(sl->e->tab, sl->i, &ll))
		    if (Is_locloc(sl->i))
		    	error(MESS(3601, "target not initialised"));
		    else error3(0, sl->i,
		    	MESS(3602, " hasn't been initialised"));
		return ll;
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l);
		ll= location(tl->R);
		if (still_ok) {	
			ll= adrassoc(*ll, tl->K);
			if (ll == Pnil && still_ok) error(MESS(3603, "key not in table"));
		}
		return ll;
	} else {
		syserr(MESS(3604, "call of location with improper type"));
		return (value *) Dummy;
	}
}

Hidden Procedure uniquify(l) loc l; {
	if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		value *ta= &(sl->e->tab), ke= sl->i;
		uniql(ta);
		check_location(l);
		if (still_ok) {
			if (Is_compound(*ta)) uniql(Field(*ta, intval(ke)));
			else {	value *aa, v;
				VOID uniq_assoc(*ta, ke);
				aa= adrassoc(*ta, ke);
				v= copy(tarvalue(ke, *aa));
				release(*aa);
				*aa= v;
				uniql(aa);
			}
		}
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l);
		value t, ke;
		uniquify(tl->R);
		if (still_ok) { t= *location(tl->R); ke= tl->K; }
		if (still_ok) {
			if (!Is_table(t)) error(MESS(3605, "selection on non-table"));
			else if (empty(t)) error(MESS(3606, "selection on empty table"));
			else {
				check_location(l);
				if (still_ok) VOID uniq_assoc(t, ke);
			}
		}
	} else if (Is_trimloc(l)) { syserr(MESS(3607, "uniquifying trimloc"));
	} else if (Is_compound(l)) { syserr(MESS(3608, "uniquifying comploc"));
	} else syserr(MESS(3609, "uniquifying non-location"));
}

Visible Procedure check_location(l) loc l; {
	VOID location(l);
	/* location may produce an error message */
}

Visible value content(l) loc l; {
	value *ll= location(l);
	return still_ok ? copy(*ll) : Vnil;
}

Visible loc trim_loc(l, v, sign) loc l; value v; char sign; {
	loc root, res; value text, B, C;
	if (Is_simploc(l) || Is_tbseloc(l)) {
		uniquify(l); /* Call tarvalue at proper time */
		root= l;
		B= zero; C= zero;
	} else if (Is_trimloc(l)) {
		trimloc *rr= Trimloc(l);
		root= rr->R;
		B= rr->B; C= rr->C;
	} else {
		error(MESS(3610, "trim (@ or |) on target of improper type"));
		return Lnil;
	}
	text= content(root);
	if (!still_ok);
	else if (!Is_text(text)) {
		error(MESS(3611, "in the target t@p or t|p, t does not contain a text"));
	} else {
		value s= size(text), w, x, b_plus_c;
		if (sign == '@') B= sum(B, w=diff(v, one));
		else {	C= sum(C, w=diff(x= diff(s, B), v)); release(x); }
		release(w);
		b_plus_c= sum(B, C);
		if (still_ok && (compare(B,zero)<0 || compare(C,zero)<0
			      || compare(b_plus_c,s)>0))
			error(MESS(3612, "in the target t@p or t|p, p is out of bounds"));
		else res= mk_trimloc(root, B, C);
		if (sign == '@') release(B); 
		else release(C);
		release(s); release(b_plus_c);
	}
	release(text);
	if (still_ok) return res; else return Lnil;
}

Visible loc tbsel_loc(R, K) loc R; value K; {
	if (Is_simploc(R) || Is_tbseloc(R)) return mk_tbseloc(R, K);
	else error(MESS(3613, "selection on target of improper type"));
	return Lnil;
}

Visible loc local_loc(i) basidf i; { return mk_simploc(i, curnv); }

Visible loc global_loc(i) basidf i; { return mk_simploc(i, prmnv); }

Hidden Procedure put_trim(v, tl) value v; trimloc *tl; {
	value rr, nn, head, tail, part;
	value B= tl->B, C= tl->C, len, len_minus_c, tail_start;
	rr= *location(tl->R);
	len= size(rr);
	len_minus_c= diff(len, C); release(len);
	tail_start= sum(len_minus_c, one); release(len_minus_c);
	if (compare(B, zero)<0 || compare(C, zero)<0
	 || compare(B, tail_start)>=0)
		error(MESS(3614, "trim (@ or |) on text location out of bounds"));
	else {
		head= curtail(rr, B); /* rr|B */
		tail= behead(rr, tail_start); /* rr@(#rr-C+1) */
		part= concat(head, v); release(head);
		nn= concat(part, tail); release(part); release(tail);
		put(nn, tl->R); release(nn);
	}
	release(tail_start);
}

Visible Procedure put(v, l) value v; loc l; {
	if (Is_locloc(l)) {
		e_replace(v, &curnv->tab, l);
	} else if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		e_replace(v, &(sl->e->tab), sl->i);
	} else if (Is_trimloc(l)) {
		if (!Is_text(v)) error(MESS(3615, "putting non-text in trim (@ or |)"));
		else put_trim(v, Trimloc(l));
	} else if (Is_compound(l)) {
		intlet k, len= Nfields(l);
		if (!Is_compound(v))
		    error(MESS(3616, "putting non-compound in compound location"));
		else if (Nfields(v) != Nfields(l))
		    error(MESS(3617, "putting compound in compound location of different length"));
		else k_Overfields { put(*Field(v, k), *Field(l, k)); }
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l); value *rootloc;
		uniquify(tl->R);
		if (still_ok) {
			rootloc= location(tl->R);
			if (still_ok && !Is_table(*rootloc))
				error(MESS(3621, "selection on non-table"));
			if (still_ok) replace(v, rootloc, tl->K);
		}
	} else error(MESS(3618, "putting in non-target"));
}

/* Check for correct effect of multiple put-command: catches PUT 1, 2 IN x, x.  
   The assignment cannot be undone, but this is not considered a problem.
   For trimmed-texts, no checks are made because the language definition
   itself causes problem (try PUT "abc", "" IN x@2|1, x@3|1). */

Hidden bool putck(v, l) value v; loc l; {
	intlet k, len; value w;
	if (!still_ok) return No;
	if (Is_compound(l)) {
		if (!Is_compound(v) || Nfields(v) != (len= Nfields(l)))
			return No; /* Severe type error */
		k_Overfields
			{ if (!putck(*Field(v, k), *Field(l, k))) return No; }
		return Yes;
	}
	if (Is_trimloc(l)) return Yes; /* Don't check trim locations */
	w= *location(l);
	/* Unfortunately, this may already cause an error, e.g. after
	   PUT 1, {} IN t[1], t.  This can't be helped unless we introduce
	   a flag so that location will shut up. */
	return still_ok && compare(v, w) == 0;
}

/* The check can't be called from within put because put is recursive,
   and so is the check: then, for the inner levels the check would be done
   twice.  Moreover, we don't want to clutter up put, which is called
   internally in, many places. */

Visible Procedure put_with_check(v, l) value v; loc l; {
	intlet i, k, len; bool ok;
	put(v, l);
	if (!still_ok || !Is_compound(l))
		return; /* Single target can't be wrong */
	len= Nfields(l); ok= Yes;
	/* Quick check for putting in all different local targets: */
	k_Overfields {
		if (!IsSmallInt(*Field(l, k))) { ok= No; break; }
		for (i= k-1; i >= 0; --i) {
			if (*Field(l, i) == *Field(l, k)) { ok= No; break; }
		}
		if (!ok) break;
	}
	if (ok) return; /* All different local basic-targets */
	if (!putck(v, l))
		error(MESS(3619, "putting different values in same location"));
}


Hidden bool l_exists(l) loc l; {
	if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		return envassoc(sl->e->tab, sl->i) != Pnil;
	} else if (Is_trimloc(l)) {
		error(MESS(3620, "deleting trimmed (@ or |) target"));
		return No;
	} else if (Is_compound(l)) {
		intlet k, len= Nfields(l);
		k_Overfields { if (!l_exists(*Field(l, k))) return No; }
		return Yes;
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l); value *ll;
		uniquify(tl->R); /* call tarvalue() at proper place */
		if (still_ok) ll= location(tl->R);
		if (still_ok && !Is_table(*ll))
			error(MESS(3621, "selection on non-table"));
		return still_ok && in_keys(tl->K, *ll);
	} else {
		error(MESS(3622, "deleting non-target"));
		return No;
	}
}

/* Delete a location if it exists */

Hidden Procedure l_del(l) loc l; {
	if (Is_simploc(l)) {
		simploc *sl= Simploc(l);
		e_delete(&(sl->e->tab), sl->i);
	} else if (Is_trimloc(l)) {
		error(MESS(3623, "deleting trimmed (@ or |) target"));
	} else if (Is_compound(l)) {
		intlet k, len= Nfields(l);
		k_Overfields { l_del(*Field(l, k)); }
	} else if (Is_tbseloc(l)) {
		tbseloc *tl= Tbseloc(l);
		value *lc;
		uniquify(tl->R);
		if (still_ok) {
			lc= location(tl->R);
			if (in_keys(tl->K, *lc)) delete(lc, tl->K);
		}
	} else error(MESS(3624, "deleting non-target"));
}

Visible Procedure l_delete(l) loc l; {
	if (l_exists(l)) l_del(l);
	else if (still_ok) error(MESS(3625, "deleting non-existent target"));
}

Visible Procedure l_insert(v, l) value v; loc l; {
	value *ll;
	uniquify(l);
	if (still_ok) {
		ll= location(l);
		if (!Is_list(*ll)) error(MESS(3626, "inserting in non-list"));
		else insert(v, ll);
	}
}

Visible Procedure l_remove(v, l) value v; loc l; {
	value *ll;
	uniquify(l);
	if (still_ok) {
		ll= location(l);
		if (!Is_list(*ll)) error(MESS(3627, "removing from non-list"));
		else if (empty(*ll)) error(MESS(3628, "removing from empty list"));
		else remove(v, ll);
	}
}

/* Warning: choose is only as good as the accuracy of the random-number */
/* generator. In particular, for very large values of v, elements will  */
/* be chosen unfairly. Choose should be rewritten to cope with this     */

Visible Procedure choose(l, v) loc l; value v; {
	value w, s, r;
	if (!Is_tlt(v)) error(MESS(3629, "choosing from non-text, -list or -table"));
	else if (empty(v)) error(MESS(3630, "choosing from empty text, list or table"));
	else {
		/* PUT (floor(random*#v) + 1) th'of v IN l */
		s= size(v);
		r= prod(w= random(), s); release(w); release(s);
		w= floorf(r); release(r);
		r= sum(w, one); release(w);
		put(w= th_of(r, v), l); release(w); release(r);
	}
}

Visible Procedure draw(l) loc l; {
	value r= random();
	put(r, l);
	release(r);
}

Visible Procedure bind(l) loc l; {
	if (*bndtgs != Vnil) {
		if (Is_simploc(l)) {
			simploc *ll= Simploc(l);
			if (!in(ll->i, *bndtgs)) /* kludge */ /* what for? */
				insert(ll->i, bndtgs);
		} else if (Is_compound(l)) {
			intlet k, len= Nfields(l);
			k_Overfields { bind(*Field(l, k)); }
		} else error(MESS(3631, "binding non-identifier"));
	}
	l_del(l);
}

Visible Procedure unbind(l) loc l; {
	if (*bndtgs != Vnil) {
		if (Is_simploc(l)) {
			simploc *ll= Simploc(l);
			if (in(ll->i, *bndtgs))
				remove(ll->i, bndtgs);
		} else if (Is_compound(l)) {
			intlet k, len= Nfields(l);
			k_Overfields { unbind(*Field(l, k)); }
		} else error(MESS(3632, "unbinding non-identifier"));
	}
	l_del(l);
}
