/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1tlt.c,v 1.1 84/06/28 00:49:00 timo Exp $ */

#include "b.h"
#include "b1obj.h"
#include "B1tlt.h"

Visible value mk_elt() { return grab_elt(); }

Visible value size(x) value x; { /* monadic # operator */
	if (!Is_tlt(x)) error("in #t, t is not a text, list or table");
	return mk_integer((int) Length(x));
}

#define Lisent(tp,k) (*(tp+(k)))

Visible value size2(v, t) value v, t; { /* Dyadic # operator */
	intlet len= Length(t), n= 0, k; value *tp= Ats(t);
	if (!Is_tlt(t)) error("in e#t, t is not a text, list or table");
	switch (t->type) {
	case Tex:
		{string cp= (string)tp; char c;
			if (v->type != Tex)
				error("in e#t, t is a text but e is not");
			if (Length(v) != 1) error(
				"in e#t, e is a text but not a character");
			c= *Str(v);
			Overall if (*cp++ == c) n++;
		} break;
	case ELT:
		break;
	case Lis:
		{intlet lo= -1, mi, xx, mm, hi= len; relation c;
		bins:	if (hi-lo < 2) break;
			mi= (lo+hi)/2;
			if ((c= compare(v, Lisent(tp,mi))) == 0) goto some;
			if (c < 0) hi= mi; else lo= mi;
			goto bins;
		some:	xx= mi;
			while (xx-lo > 1) {
				mm= (lo+xx)/2;
				if (compare(v, Lisent(tp,mm)) == 0) xx= mm;
				else lo= mm;
			}
			xx= mi;
			while (hi-xx > 1) {
				mm= (xx+hi)/2;
				if (compare(v, Lisent(tp,mm)) == 0) xx= mm;
				else hi= mm;
			}
			n= hi-lo-1;
		} break;
	case Tab:
		Overall if (compare(v, Dts(*tp++)) == 0) n++;
		break;
	default:
		syserr("e#t with non text, list or table");
		break;
	}
	return mk_integer((int) n);
}

Hidden bool less(r) relation r;    { return r<0; }
Hidden bool greater(r) relation r; { return r>0; }

Hidden value mm1(t, rel) value t; bool (*rel)(); {
	intlet len= Length(t), k; value m, *tp= Ats(t);
	switch (t->type) {
	case Tex:
		{string cp= (string) tp; char mc= '\0', mm[2];
			Overall {
				if (mc == '\0' || ((*rel)(*cp < mc ? -1 : (*cp > mc ? 1 : 0))))
					mc= *cp;
				cp++;
			}
			mm[0]= mc; mm[1]= '\0';
			m= mk_text(mm);
		} break;
	case Lis:
		if ((*rel)(-1)) /*min*/ m= copy(*Ats(t));
		else m= copy(*(Ats(t)+len-1));
		break;
	case Tab:
		{value dm= Vnil;
			Overall {
				if (dm == Vnil || (*rel)(compare(Dts(*tp), dm)))
					dm= Dts(*tp);
				tp++;
			}
			m= copy(dm);
		} break;
	default:
		syserr("min or max t, with non text, list or table");
	}
	return m;
}

Hidden value mm2(v, t, rel) value v, t; bool (*rel)(); {
	intlet len= Length(t), k; value m= Vnil, *tp= Ats(t);
	switch (t->type) {
	case Tex:
		{string cp= (string) tp; char c, mc= '\0', mm[2];
			c= *Str(v);
			Overall {
				if ((*rel)(c < *cp ? -1 : c > *cp ? 1 : 0)) {
					if (mc == '\0' || (*rel)(*cp < mc ? -1 : *cp>mc ? 1 : 0))
						mc= *cp;
				}
				cp++;
			}
			if (mc != '\0') {
				mm[0]= mc; mm[1]= '\0';
				m= mk_text(mm);
			}
		} break;
	case Lis:
		{intlet lim1, mid, lim2;
			if ((*rel)(-1)) { /*min*/
				lim1= 1; lim2= len-1;
			} else {
				lim2= 1; lim1= len-1;
			}
			if (!(*rel)(compare(v, Lisent(tp,lim2)))) break;
			if (len == 1 || (*rel)(compare(v, Lisent(tp,lim1)))) {
				m= copy(Lisent(tp,lim1));
				break;
			}
			/* v rel tp[lim2] && !(v rel tp[lim1]) */
			while (abs(lim2-lim1) > 1) {
				mid= (lim1+lim2)/2;
				if ((*rel)(compare(v, Lisent(tp,mid)))) lim2= mid;
				else lim1= mid;
			}
			m= copy(Lisent(tp,lim2));
		} break;
	case Tab:
		{value dm= Vnil;
			Overall {
				if ((*rel)(compare(v, Dts(*tp)))) {
					if (dm == Vnil ||
						(*rel)(compare(Dts(*tp), dm)))
						dm= Dts(*tp);
				}
				tp++;
			}
			if (dm != Vnil) m= copy(dm);
		} break;
	default:
		syserr("min2 or max2 with non text, list or table");
		break;
	}
	return m;
}

Visible value min1(t) value t; { /* Monadic min */
	if (!Is_tlt(t)) error("in min t, t is not a text, list or table");
	if (Length(t) == 0) error("in min t, t is empty");
	return mm1(t, less);
}

Visible value min2(v, t) value v, t; {
	value m;
	if (!Is_tlt(t)) error("in e min t, t is not a text, list or table");
	if (Length(t) == 0) error("in e min t, t is empty");
	if (Is_text(t)) {
		if (!Is_text(v)) error("in e min t, t is a text but e is not");
		if (Length(v) != 1) error("in e min t, e is a text but not a character");
	}
	m= mm2(v, t, less);
	if (m == Vnil) error("in e min t, no element of t exceeds e");
	return m;
}

Visible value max1(t) value t; {
	if (!Is_tlt(t)) error("in max t, t is not a text, list or table");
	if (Length(t) == 0) error("in max t, t is empty");
	return mm1(t, greater);
}

Visible value max2(v, t) value v, t; {
	value m;
	if (!Is_tlt(t)) error("in e max t, t is not a text, list or table");
	if (Length(t) == 0) error("in e max t, t is empty");
	if (Is_text(t)) {
		if (!Is_text(v)) error("in e max t, t is a text but e is not");
		if (Length(v) != 1) error("in e max t, e is a text but not a character");
	}
	m= mm2(v, t, greater);
	if (m == Vnil) error("in e max t, no element of t is less than e");
	return m;
}

Visible value th_of(n, t) value n, t; {
	return thof(intval(n), t);
}

Visible value thof(n, t) int n; value t; {
	intlet len= Length(t); value w;
	if (!Is_tlt(t)) error("in n th'of t, t is not a text, list or table");
	if (n <= 0 || n > len) error("in n th'of t, n is out of bounds");
	switch (t->type) {
	case Tex:
		{char ww[2];
			ww[0]= *(Str(t)+n-1); ww[1]= '\0';
			w= mk_text(ww);
		} break;
	case Lis:
		w= copy(*(Ats(t)+n-1));
		break;
	case Tab:
		w= copy(Dts(*(Ats(t)+n-1)));
		break;
	default:
		syserr("th'of with non text, list or table");
	}
	return w;
}

Visible bool found(elem, v, probe, where)
	value (*elem)(), v, probe; intlet *where;
	/* think of elem(v,lo-1) as -Infinity and elem(v,hi+1) as +Infinity.
	   found and where at the end satisfy:
	   SELECT:
	       SOME k IN {lo..hi} HAS probe = elem(v,k):
	           found = Yes AND where = k
	       ELSE: found = No AND elem(v,where-1) < probe < elem(v,where).
	*/
{relation c; intlet lo=0, hi= Length(v)-1;
	if (lo > hi) { *where= lo; return No; }
	if ((c= compare(probe, (*elem)(v, lo))) == 0) {*where= lo; return Yes; }
	if (c < 0) { *where=lo; return No; }
	if (lo == hi) { *where=hi+1; return No; }
	if ((c= compare(probe, (*elem)(v, hi))) == 0) { *where=hi; return Yes; }
	if (c > 0) { *where=hi+1; return No; }
	/* elem(lo) < probe < elem(hi) */
	while (hi-lo > 1) {
		if ((c= compare(probe, (*elem)(v, (lo+hi)/2))) == 0) {
			*where= (lo+hi)/2; return Yes;
		}
		if (c < 0) hi= (lo+hi)/2; else lo= (lo+hi)/2;
	}
	*where= hi; return No;
}

Visible bool in(v, t) value v, t; {
	intlet where, k, len= Length(t); value *tp= Ats(t);
	if (!Is_tlt(t)) error("in the test e in t, t is not a text, list or table");
	switch (t->type) {
	case Tex:
		if (v->type != Tex)
			error("in the test e in t, t is a text but e is not");
		if (Length(v) != 1)
			error("in the test e in t, e is a text but not a character");
		return index((string) tp, *Str(v)) != 0;
	case ELT:
		return No;
	case Lis:
		return found(list_elem, t, v, &where);
	case Tab:
		Overall if (compare(v, Dts(*tp++)) == 0) return Yes;
		return No;
	default:
		syserr("e in t with non text, list or table");
		return No;
	}
}
