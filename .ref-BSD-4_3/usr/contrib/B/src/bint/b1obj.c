/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1obj.c,v 1.4 85/08/22 16:52:13 timo Exp $
*/

/* Generic routines for all values */

#include "b.h"
#include "b1obj.h"
#ifndef INTEGRATION
#include "b1btr.h"
#include "b1val.h"
#endif
#include "b1tlt.h"
#include "b3err.h"
#include "b3typ.h"

#ifndef INTEGRATION

Visible bool comp_ok = Yes; 		/* Temporary, to catch type errors */

relation comp_tlt(), comp_text();	/* From b1lta.c */

Hidden Procedure incompatible(v, w) value v, w; {
	value message, m1, m2, m3, m4, m5, m6;
	message= concat(m1= convert(m2= (value) valtype(v), No, No),
		 m3= concat(m4= mk_text(" and "),
		 m5= convert(m6= (value) valtype(w), No, No)));
	error2(MESS(1400, "incompatible types "), message);
	release(message);
	release(m1); release(m2); release(m3);
	release(m4); release(m5); release(m6);
}

Visible relation compare(v, w) value v, w; {
	literal vt, wt;
	int i;
	relation rel;
	
	comp_ok = Yes;

	if (v EQ w) return(0);
	if (IsSmallInt(v) && IsSmallInt(w))
		return SmallIntVal(v) - SmallIntVal(w);
	vt = Type(v);
	wt = Type(w);
	switch (vt) {
	case Num:
		if (wt != Num) {
 incomp:
			/*Temporary until static checks are implemented*/
 			incompatible(v, w);
			comp_ok= No;
			return -1;
 		}
		return(numcomp(v, w));
	case Com:
		if (wt != Com || Nfields(v) != Nfields(w)) goto incomp;
		for (i = 0; i < Nfields(v); i++) {
			rel = compare(*Field(v, i), *Field(w, i));
			if (rel NE 0) return(rel);
		}
		return(0);
	case Tex:
		if (wt != Tex) goto incomp;
		return(comp_text(v, w));
	case Lis:
		if (wt != Lis && wt != ELT) goto incomp;
		return(comp_tlt(v, w));
	case Tab:
		if (wt != Tab && wt != ELT) goto incomp;
		return(comp_tlt(v, w));
	case ELT:
		if (wt != Tab && wt != Lis && wt != ELT) goto incomp;
		return(Root(w) EQ Bnil ? 0 : -1);
	default: 
		syserr(MESS(1401, "comparison of unknown types"));
		/*NOTREACHED*/
	}
}

/* Used for set'random. Needs to be rewritten so that for small changes in v */
/* you get large changes in hash(v) */

Visible double hash(v) value v; {
	if (Is_number(v)) return numhash(v);
	else if (Is_compound(v)) {
		int len= Nfields(v), k; double d= .404*len;
		k_Overfields {
			d= .874*d+.310*hash(*Field(v, k));
		}
		return d;
	} else {
		int len= length(v), k; double d= .404*len;
		if (len == 0) return .909;
		else if (Is_text(v)) {
			value ch;
			k_Over_len {
				ch= thof(k+1, v);
				d= .987*d+.277*charval(ch);
				release(ch);
			}
			return d;
		} else if (Is_list(v)) {
			value el;
			k_Over_len {
				d= .874*d+.310*hash(el= thof(k+1, v));
				release(el);
			}
			return d;
		} else if (Is_table(v)) {
			k_Over_len {
				d= .874*d+.310*hash(*key(v, k))
					 +.123*hash(*assoc(v, k));
			}
			return d;
		} else {
			syserr(MESS(1402, "hash called with unknown type"));
			return (double) Dummy;
		}
	}
}

Hidden Procedure concato(v, t) value* v; value t; {
	value v1= *v;
	*v= concat(*v, t);
	release(v1);
}

Visible value convert(v, coll, outer) value v; bool coll, outer; {
	value t, quote, c, cv, sep, th, open, close; int k, len; char ch;
	switch (Type(v)) {
	case Num:
		return mk_text(convnum(v));
	case Tex:
		if (outer) return copy(v);
		quote= mk_text("\"");
		len= length(v);
		t= copy(quote);
		for (k=1; k<=len; k++) {
			c= thof(k, v);
			ch= charval(c);
			concato(&t, c);
			if (ch == '"' || ch == '`') concato(&t, c);
			release(c);
		}
		concato(&t, quote);
		release(quote);
		break;
	case Com:
		len= Nfields(v);
		outer&= coll;
		sep= mk_text(outer ? " " : ", ");
		t= mk_text(coll ? "" : "(");
		k_Over_len {
			concato(&t, cv= convert(*Field(v, k), No, outer));
			release(cv);
			if (!Last(k)) concato(&t, sep);
		}
		release(sep);
		if (!coll) {
			concato(&t, cv= mk_text(")"));
			release(cv);
		}
		break;
	case Lis:
	case ELT:
		len= length(v);
		t= mk_text("{");
		sep= mk_text("; ");
		for (k=1; k<=len; k++) {
			concato(&t, cv= convert(th= thof(k, v), No, No));
			release(cv); release(th);
			if (k != len) concato(&t, sep);
		}
		release(sep);
		concato(&t, cv= mk_text("}"));
		release(cv);
		break;
	case Tab:
		len= length(v);
		open= mk_text("[");
		close= mk_text("]: ");
		sep= mk_text("; ");
		t= mk_text("{");
		k_Over_len {
			concato(&t, open);
			concato(&t, cv= convert(*key(v, k), Yes, No));
			release(cv);
			concato(&t, close);
			concato(&t, cv= convert(*assoc(v, k), No, No));
			release(cv);
			if (!Last(k)) concato(&t, sep);
		}
		concato(&t, cv= mk_text("}")); release(cv);
		release(open); release(close); release(sep);
		break;
	default:
		if (bugs || testing) {
			t= mk_text("?");
			concato(&t, cv= mkchar(Type(v))); release(cv);
			concato(&t, cv= mkchar('$')); release(cv);
			break;
		}
		syserr(MESS(1403, "unknown type in convert"));
	}
	return t;
}

Hidden value adj(v, w, side) value v, w; char side; {
	value t, c, sp, r, i;
	int len, wid, diff, left, right;
	c= convert(v, Yes, Yes);
	len= length(c);
	wid= intval(w);
	if (wid<=len) return c;
	else {
		diff= wid-len;
		if (side == 'L') { left= 0; right= diff; }
		else if (side == 'R') { left= diff; right= 0; }
		else {left= diff/2; right= (diff+1)/2; }
		sp= mk_text(" ");
		if (left == 0) t= c;
		else {
			t= repeat(sp, i= mk_integer(left)); release(i);
			concato(&t, c);
			release(c);
		}
		if (right != 0) {
			r= repeat(sp, i= mk_integer(right)); release(i);
			concato(&t, r);
			release(r);
		}
		release(sp);
		return t;
	}
}

Visible value adjleft(v, w) value v, w; {
	return adj(v, w, 'L');
}

Visible value adjright(v, w) value v, w; {
	return adj(v, w, 'R');
}

Visible value centre(v, w) value v, w; {
	return adj(v, w, 'C');
}

#else INTEGRATION

#define Sgn(d) (d)

Visible relation compare(v, w) value v, w; {
	literal vt= Type(v), wt= Type(w);
	register intlet vlen, wlen, len, k;
	value message;
	vlen= IsSmallInt(v) ? 0 : Length(v);
	wlen= IsSmallInt(w) ? 0 : Length(w);
	if (v == w) return 0;
	if (!(vt == wt && !(vt == Com && vlen != wlen) ||
			    vt == ELT && (wt == Lis || wt == Tab) ||
			    wt == ELT && (vt == Lis || vt == Tab))) {
		message= concat(convert((value) valtype(v), No, No),
			 concat(mk_text(" and "),
			 convert((value) valtype(w), No, No)));
		error2(MESS(1404, "incompatible types "), message);
		       /*doesn't return: so can't release message*/
	}
	if (vt != Num && (vlen == 0 || wlen == 0))
		return Sgn(vlen-wlen);
	switch (vt) {
	case Num: return numcomp(v, w);
	case Tex: return strcmp(Str(v), Str(w));

	case Com:
	case Lis:
	case Tab:
	case ELT:
		{value *vp= Ats(v), *wp= Ats(w);
		 relation c;
			len= vlen < wlen ? vlen : wlen;
			Overall if ((c= compare(*vp++, *wp++)) != 0) return c;
			return Sgn(vlen-wlen);
		}
	default:
		syserr(MESS(1405, "comparison of unknown types"));
		/* NOTREACHED */
	}
}

Visible double hash(v) value v; {
	literal t= Type(v); intlet len= Length(v), k; double d= t+.404*len;
	switch (t) {
	case Num: return numhash(v);
	case Tex:
		{string vp= Str(v);
			Overall d= .987*d+.277*(*vp++);
			return d;
		}
	case Com:
	case Lis:
	case Tab:
	case ELT:
		{value *vp= Ats(v);
			if (len == 0) return .909;
			Overall d= .874*d+.310*hash(*vp++);
			return d;
		}
	default:
		syserr(MESS(1406, "hash called with unknown type"));
		/* NOTREACHED */
	}
}

#endif INTEGRATION
