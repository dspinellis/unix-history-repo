/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1tlt.c,v 1.4 85/08/22 16:53:20 timo Exp $
*/

/* generic routines for B texts, lists and tables */

#include "b.h"
#include "b0fea.h"
#include "b1obj.h"
#ifndef INTEGRATION
#include "b0con.h"
#include "b1btr.h"
#include "b1val.h"
#endif
#include "b1tlt.h"
#include "b3err.h"

#ifndef INTEGRATION

/* From b1lta.c */
int l2size();
value l2min(), l2max();

Visible value mk_elt() { /* {}, internal only */
	value e = grab_tlt(ELT, Lt);
	Root(e) = Bnil;
	return e;
}

Visible bool empty(v) value v; { /* #v=0, internal only */
	switch (Type(v)) {
	case ELT:
	case Lis:
	case Tex:
	case Tab:
		return Root(v) EQ Bnil;
	default:
		return No;
		/* Some routines must test empty(t) end return an error
		   message if it fails, before testing Type(t).
		   In this way, they won't give the wrong error message. */
	}
}

/* return size of (number of items in) dependent tree */

Hidden value treesize(pnode) btreeptr pnode; {
    int psize;
    value vsize, childsize, u;
    intlet l;
    psize = Size(pnode);
    if (psize EQ Bigsize) {
	switch (Flag(pnode)) {        
	case Inner:
	    vsize = mk_integer((int) Lim(pnode));
	    for (l = 0; l <= Lim(pnode); l++) {
		childsize = treesize(Ptr(pnode, l));
		u = vsize;
		vsize = sum(vsize, childsize);
		release(u);
		release(childsize);
	    }
	    break;
	case Irange: 
	    u = diff(Upbval(pnode), Lwbval(pnode));
	    vsize = sum(u, one);
	    release(u);
	    break;
	case Bottom: 
	case Crange: 
	    syserr(MESS(1700, "Bigsize in Bottom or Crange"));
	}
	return(vsize);
    }
    return mk_integer(psize);
}

Visible value size(t) value t; { /* #t */
	int tsize;
	switch (Type(t)) {
	case ELT:
	case Lis:
	case Tex:
	case Tab:
		tsize = Tltsize(t);
		if (tsize EQ Bigsize) return treesize(Root(t));
		return mk_integer(tsize);
	default:
		reqerr(MESS(1701, "in #t, t is not a text, list or table"));
		return zero;
	}
}

Visible value th_of(num, v) value num, v; { /* num th'of v */
	value m= Vnil;
	if (!Is_tlt(v))
		error(MESS(1702, "in n th'of t, t is not a text, list or table"));
	else if (!Is_number(num))
		error(MESS(1703, "in n th'of t, n is not a number"));
	else if (empty(v))
		error(MESS(1704, "in n th'of t, t is empty"));
	else if (numcomp(num, one) < 0)
		error(MESS(1705, "in n th'of t, n is < 1"));
	else {
		/*RANGES?*/
		m= thof(intval(num), v);
		if (m == Vnil && still_ok)
			error(MESS(1706, "in n th'of t, n exceeds #t"));
	}
	return m;
}

/*
 * 'Walktree' handles functions on texts and associates of tables.
 * The actual function performed is determined by the 'visit' function.
 * The tree is walked (possibly recursively) and all items are visited.
 * The return value of walktree() and visit() is used to determine whether
 * the walk should continue (Yes == continue, No == stop now).
 * Global variables are used to communicate the result, and the parameters
 * of the function. The naming convention is according to "e func t".
 */

Hidden intlet tt;		/* type of walked value t */
Hidden intlet wt;		/* width of items in walked value t */
Hidden value ve; 		/* value of e, if func is dyadic */
Hidden char ce; 		/* C char in e, if t is a text */

Hidden int count; 		/* result of size2 */
Hidden bool found; 		/* result for in */
Hidden intlet m_char; 		/* result for min/max on texts */
Hidden value m_val;		/* result for min/max on tables */

#define Lowchar (-Maxintlet)	/* -infinity for characters */
#define Highchar (Maxintlet)	/* +infinity */

Hidden bool walktree(p, visit) btreeptr p; bool (*visit)(); {
	intlet l;
	
	if (p EQ Bnil) return Yes; /* i.e., not found (used by in() !) */
	for (l=0; l < Lim(p); l++) {
		switch (Flag(p)) {
		case Inner:
			if (!walktree(Ptr(p, l), visit) || !still_ok)
				return No;
			if (!(*visit)(Piitm(p, l, wt)) || !still_ok)
				return No;
			break;
		case Bottom:
			if (!(*visit)(Pbitm(p, l, wt)) || !still_ok)
				return No;
		}
	}
	return Flag(p) EQ Bottom || walktree(Ptr(p, l), visit);
}

/* Common code for min/max-1/2, size2, in. */

Hidden Procedure tlt_func(e, t, where, li_func, te_visit, ta_visit)
	value e, t; 			/* [e] func t */
	string where; 			/* "in [e] func_name t" */
	value (*li_func)(); 		/* func for lists */
	bool (*te_visit)(), (*ta_visit)(); /* 'visit' for walktree */
{
	m_val = Vnil;
	if (empty(t)) {
		error3(MESSMAKE(where), Vnil, MESS(1707, ", t is empty"));
		return;
	}
	wt = Itemwidth(Itemtype(t));
	tt = Type(t);
	switch (tt) {
	case Lis:
		m_val = (*li_func)(e, t);
		break;
	case Tex:
		if (e NE Vnil) {
			if (!Character(e)) {
				error3(MESSMAKE(where), Vnil,
			MESS(1708, ", t is a text, but e is not a character"));
				return;
			}
			ce = Bchar(Root(e), 0);
		}
		found = !walktree(Root(t), te_visit);
		if (m_char NE Lowchar && m_char NE Highchar)
			m_val = mkchar(m_char);
		break;
	case Tab:
		ve = e;
		found = !walktree(Root(t), ta_visit);
		break;
	default:
		error3(MESSMAKE(where), Vnil,
			MESS(1709, ", t is not a text list or table"));
	}
}

Hidden value li2size(e, t) value e, t; {
	count = l2size(e, t);
	return Vnil;
}

Hidden bool te2size(pitm) itemptr pitm; {
	if (ce EQ Charval(pitm))
		count++;
	return Yes;
}

Hidden bool ta2size(pitm) itemptr pitm; {
	if (compare(ve, Ascval(pitm)) EQ 0)
		count++;
	return Yes;
}

Visible value size2(e, t) value e, t; { /* e#t */
	if (empty(t)) /* Must check here because tlt_func would complain */
		return copy(zero);
	m_char = Lowchar;
	count = 0;
	tlt_func(e, t, "in e#t", li2size, te2size, ta2size);
	return mk_integer(count);
}

Hidden value li_in(e, t) value e, t; {
	found = in_keys(e, t);
	return Vnil;
}
	
Hidden bool te_in(pitm) itemptr pitm; {
	return Charval(pitm) NE ce;
}

Hidden bool ta_in(pitm) itemptr pitm; {
	return compare(ve, Ascval(pitm)) NE 0;
}

Visible bool in(e, t) value e, t; {
	if (empty(t)) /* Must check here because tlt_func would complain */
		return No;
	m_char = Lowchar;
	found = No;
	tlt_func(e, t, "in the test e in t", li_in, te_in, ta_in);
	return found;
}

Hidden value li_min(e, t) value e, t; {
	return th_of(one, t);
}

Hidden bool te_min(pitm) itemptr pitm; {
	if (m_char > Charval(pitm))
		m_char = Charval(pitm);
	return Yes;
}

Hidden bool ta_min(pitm) itemptr pitm; {
	if (m_val EQ Vnil || compare(m_val, Ascval(pitm)) > 0) {
		release(m_val);
		m_val = copy(Ascval(pitm));
	}
	return Yes;
}

Visible value min1(t) value t; {
	m_char = Highchar;
	tlt_func(Vnil, t, "in min t", li_min, te_min, ta_min);
	return m_val;
}

Hidden value li_max(e, t) value e, t; {
	value v= size(t);
	m_val = th_of(v, t);
	release(v);
	return m_val;
}

Hidden bool te_max(pitm) itemptr pitm; {
	if (m_char < Charval(pitm))
		m_char = Charval(pitm);
	return Yes;
}

Hidden bool ta_max(pitm) itemptr pitm; {
	if (m_val EQ Vnil || compare(Ascval(pitm), m_val) > 0) {
		release(m_val);
		m_val = copy(Ascval(pitm));
	}
	return Yes;
}

Visible value max1(t) value t; {
	m_char = Lowchar;
	tlt_func(Vnil, t, "in max t", li_max, te_max, ta_max);
	return m_val;
}

Hidden bool te2min(pitm) itemptr pitm; {
	if (m_char > Charval(pitm) && Charval(pitm) > ce) {
		m_char = Charval(pitm);
	}
	return Yes;
}

Hidden bool ta2min(pitm) itemptr pitm; {
	if (compare(Ascval(pitm), ve) > 0
	    &&
	    (m_val EQ Vnil || compare(m_val, Ascval(pitm)) > 0)) {
		release(m_val);
		m_val = copy(Ascval(pitm));
	}
	return Yes;
}

Visible value min2(e, t) value e, t; {
	m_char = Highchar;
	tlt_func(e, t, "in e min t", l2min, te2min, ta2min);
	if (m_val EQ Vnil && still_ok)
		reqerr(MESS(1710, "in e min t, no element of t exceeds e"));
	return m_val;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Hidden bool te2max(pitm) itemptr pitm; {
	if (ce > Charval(pitm) && Charval(pitm) > m_char) {
		m_char = Charval(pitm);
	}
	return Yes;
}

Hidden bool ta2max(pitm) itemptr pitm; {
	if (compare(ve, Ascval(pitm)) > 0
	    &&
	    (m_val EQ Vnil || compare(Ascval(pitm), m_val) > 0)) {
		release(m_val);
		m_val = copy(Ascval(pitm));
	}
	return Yes;
}

Visible value max2(e, t) value e, t; {
	m_char = Lowchar;
	tlt_func(e, t, "in e max t", l2max, te2max, ta2max);
	if (m_val EQ Vnil && still_ok)
		reqerr(MESS(1711, "in e max t, no element of t is less than e"));
	return m_val;
}

#else INTEGRATION

Visible value mk_elt() { return grab_elt(); }

Visible value size(x) value x; { /* monadic # operator */
	if (!Is_tlt(x))
		error(MESS(1712, "in #t, t is not a text, list or table"));
	return mk_integer((int) Length(x));
}

#define Lisent(tp,k) (*(tp+(k)))

Visible value size2(v, t) value v, t; { /* Dyadic # operator */
	intlet len= Length(t), n= 0, k; value *tp= Ats(t);
	if (!Is_tlt(t)) {
		error(MESS(1713, "in e#t, t is not a text, list or table"));
		return mk_integer((int) n);
	}
	switch (Type(t)) {
	case Tex:
		{string cp= (string)tp; char c;
			if (Type(v) != Tex)
				error(MESS(1714, "in e#t, t is a text but e is not"));
			if (Length(v) != 1)
				error(MESS(1715, "in e#t, e is a text but not a character"));
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
		syserr(MESS(1716, "e#t with non text, list or table"));
		break;
	}
	return mk_integer((int) n);
}

Hidden bool less(r) relation r;    { return r<0; }
Hidden bool greater(r) relation r; { return r>0; }

Hidden value mm1(t, rel) value t; bool (*rel)(); {
	intlet len= Length(t), k; value m, *tp= Ats(t);
	switch (Type(t)) {
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
		syserr(MESS(1717, "min or max t, with non text, list or table"));
	}
	return m;
}

#ifdef NO_ABS

Hidden int abs(i) int i; {
	return i >= 0 ? i : -i;
}

#endif

Hidden value mm2(v, t, rel) value v, t; bool (*rel)(); {
	intlet len= Length(t), k; value m= Vnil, *tp= Ats(t);
	switch (Type(t)) {
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
		syserr(MESS(1718, "min2 or max2 with non text, list or table"));
		break;
	}
	return m;
}

Visible value min1(t) value t; { /* Monadic min */
	value m= Vnil;
	if (!Is_tlt(t))
		error(MESS(1719, "in min t, t is not a text, list or table"));
	else if (Length(t) == 0)
		error(MESS(1720, "in min t, t is empty"));
	else m= mm1(t, less);
	return m;
}

Visible value min2(v, t) value v, t; {
	value m= Vnil;
	if (!Is_tlt(t))
		error(MESS(1721, "in e min t, t is not a text, list or table"));
	else if (Length(t) == 0)
		error(MESS(1722, "in e min t, t is empty"));
	else if (Is_text(t)) {
		if (!Is_text(v))
			error(MESS(1723, "in e min t, t is a text but e is not"));
		else if (Length(v) != 1)
			error(MESS(1724, "in e min t, e is a text but not a character"));
	}
	if (still_ok) {
		m= mm2(v, t, less);
		if (m == Vnil)
			error(MESS(1725, "in e min t, no element of t exceeds e"));
	}
	return m;
}

Visible value max1(t) value t; {
	value m= Vnil;
	if (!Is_tlt(t))
		error(MESS(1726, "in max t, t is not a text, list or table"));
	else if (Length(t) == 0)
		error(MESS(1727, "in max t, t is empty"));
	else m= mm1(t, greater);
	return m;
}

Visible value max2(v, t) value v, t; {
	value m= Vnil;
	if (!Is_tlt(t))
		error(MESS(1728, "in e max t, t is not a text, list or table"));
	else if (Length(t) == 0)
		error(MESS(1729, "in e max t, t is empty"));
	else if (Is_text(t)) {
		if (!Is_text(v))
			error(MESS(1730, "in e max t, t is a text but e is not"));
		else if (Length(v) != 1)
			error(MESS(1731, "in e max t, e is a text but not a character"));
	}
	if (still_ok) {
		m= mm2(v, t, greater);
		if (m == Vnil)
			error(MESS(1732, "in e max t, no element of t is less than e"));
	}
	return m;
}

Visible value th_of(n, t) value n, t; {
	return thof(intval(n), t);
}

Visible value thof(n, t) int n; value t; {
	intlet len= Length(t); value w= Vnil;
	if (!Is_tlt(t))
		error(MESS(1733, "in n th'of t, t is not a text, list or table"));
	else if (n <= 0 || n > len)
		error(MESS(1734, "in n th'of t, n is out of bounds"));
	else {
		switch (Type(t)) {
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
			syserr(MESS(1735, "th'of with non text, list or table"));
		}
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
	if (!Is_tlt(t)) {
		error(MESS(1736, "in the test e in t, t is not a text, list or table"));
		return No;
	}
	switch (Type(t)) {
	case Tex:
		if (Type(v) != Tex)
			error(MESS(1737, "in the test e in t, t is a text but e is not"));
		else if (Length(v) != 1)
			error(MESS(1738, "in the test e in t, e is a text but not a character"));
		else return index((string) tp, *Str(v)) != 0;
		return No;
	case ELT:
		return No;
	case Lis:
		return found(list_elem, t, v, &where);
	case Tab:
		Overall if (compare(v, Dts(*tp++)) == 0) return Yes;
		return No;
	default:
		syserr(MESS(1739, "e in t with non text, list or table"));
		return No;
	}
}

Visible bool empty(v) value v; {
	switch (Type(v)) {
	case Tex:
	case Lis:
	case Tab:
	case ELT:
		return (Length(v) == 0);
	default:
		syserr(MESS(1740, "empty() on non tlt value"));
		return (No);
	}
}

#endif INTEGRATION
