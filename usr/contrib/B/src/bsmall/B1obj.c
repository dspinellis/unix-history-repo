/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1obj.c,v 1.1 84/06/28 00:48:57 timo Exp $ */

/* General operations on objects */

#include "b.h"
#include "b1obj.h"
#include "B1tlt.h"

#define Sgn(d) (d)

Visible relation compare(v, w) value v, w; {
	literal vt= v->type, wt= w->type;
	register intlet vlen= Length(v), wlen= Length(w), len, k;
	value message;
	if (v == w) return 0;
	if (!(vt == wt && !(vt == Com && vlen != wlen) ||
			    vt == ELT && (wt == Lis || wt == Tab) ||
			    wt == ELT && (vt == Lis || vt == Tab))) {
		message= concat(mk_text("incompatible types: "),
			 concat(convert((value) valtype(v), No, No),
			 concat(mk_text(" and "),
			 convert((value) valtype(w), No, No))));
		error(Str(message)); /*doesn't return: so can't release message*/
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
		syserr("comparison of unknown types");
		return (intlet) Dummy;
	}
}

Visible double hash(v) value v; {
	literal t= v->type; intlet len= Length(v), k; double d= t+.404*len;
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
		syserr("hash called with unknown type");
		return (double) Dummy;
	}
}
