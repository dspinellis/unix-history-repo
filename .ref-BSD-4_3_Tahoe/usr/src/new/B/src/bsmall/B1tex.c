/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1tex.c,v 1.1 84/06/28 00:48:59 timo Exp $ */

/* B texts */
#include "b.h"
#include "b1obj.h"
#include "B1tlt.h" /* for Cts */

Visible value mk_text(m) string m; {
	value v; intlet len= strlen(m);
	v= grab_tex(len);
	strcpy(Str(v), m);
	return v;
}

Visible bool character(v) value v; {
	if (Is_text(v) && Length(v) == 1) return Yes;
	else return No;
}

Visible char charval(v) value v; {
	if (!Is_text(v) || Length(v) != 1) error("value not a character");
	return *Str(v);
}

Visible string strval(v) value v; {
	return Str(v);
}

Visible value concat(s, t) value s, t; {
	value c;
	if (s->type != Tex) error("in t^u, t is not a text");
	else if (t->type != Tex) error("in t^u, t is a text, but u is not");
	c= grab_tex(Length(s)+Length(t));
	strcpy(Str(c), Str(s)); strcpy(Str(c)+Length(s), Str(t));
	return c;
}

Hidden Procedure concato(s, t) value *s; string t; {
	if ((*s)->type != Tex) error("attempt to join text with non-text");
	xtndtex(s, strlen(t));
	strcat(Str(*s), t);
}

Visible value trim(v, B, C) value v; intlet B, C; {
	intlet len= Length(v), k; value w;
	string vp= Str(v)+B, wp;
	if (v->type != Tex) error("trim (@ or |) applied to non-text");
	if (B < 0 || C < 0 || B+C > len)
		error("trim (@ or |) out of bounds");
	w= grab_tex(len-=(B+C)); wp= Str(w);
	Overall *wp++= *vp++; *wp= '\0';
	return w;
}

Visible value repeat(x, y) value x, y; {
	value r; intlet i= propintlet(intval(y)); intlet xl= Length(x), p, q;
	string rp, xp;
	if (x->type != Tex) error("in t^^n, t is not a text");
	if (i < 0) error("in t^^n, n is negative");
	r= grab_tex(propintlet(i*xl)); rp= Str(r);
	for (p= 0; p < i; p++) {
		xp= Str(x);
		for (q= 0; q < xl; q++) *rp++= *xp++;
	}
	*rp= '\0';
	return r;
}

#define Left 'L'
#define Right 'R'
#define Centre 'C'

Hidden value adj(x, y, side) value x, y; literal side; {
	value r, v= convert(x, Yes, Yes); int i= intval(y);
	intlet lv= Length(v), la, k, ls, rs;
	string rp, vp;
	la= propintlet(i) - lv;
	if (la <= 0) return v;
	r= grab_tex(lv+la); rp= Str(r); vp= Str(v);

	if (side == Left) { ls= 0; rs= la; }
	else if (side == Centre) { ls= la/2; rs= (la+1)/2; }
	else { ls= la; rs= 0; }

	for (k= 0; k < ls; k++) *rp++= ' ';
	for (k= 0; k < lv; k++) *rp++= *vp++;
	for (k= 0; k < rs; k++) *rp++= ' ';
	*rp= 0;
	release(v);
	return r;
}

Visible value adjleft(x, y) value x, y; {
	return adj(x, y, Left);
}

Visible value centre(x, y) value x, y; {
	return adj(x, y, Centre);
}

Visible value adjright(x, y) value x, y; {
	return adj(x, y, Right);
}

/* For reasons of efficiency, wri does not always call convert but writes
   directly on the standard output. Modifications in convert should
   be mirrored by changes in wri and vice versa. */

Visible value convert(v, coll, outer) value v; bool coll, outer; {
	literal type= v->type; intlet len= Length(v), k; value *vp= Ats(v);
	value t, cv;
	switch (type) {
	case Num: 
		return mk_text(convnum(v));
	case Tex:
		if (outer) return copy(v);
		else {string tp= (string) vp; char cs[2];
			cs[1]= '\0';
			t= mk_text("'");
			Overall {
				cs[0]= *tp++;
				concato(&t, cs);
				if (cs[0] == '\'' || cs[0] == '`')
					concato(&t, cs);
			}
			concato(&t, "'");
			return t;
		}
	case Com:
		outer&= coll;
		t= mk_text(coll ? "" : "(");
		Overall {
			concato(&t, Str(cv= convert(*vp++, No, outer)));
			release(cv);
			if (k != len-1) concato(&t, outer ? " " : ", ");
		}
		if (!coll) concato(&t, ")");
		return t;
	case Lis: case ELT:
		t= mk_text("{");
		Overall {
			concato(&t, Str(cv= convert(*vp++, No, No)));
			release(cv);
			if (k != len-1) concato(&t, "; ");
		}
		concato(&t, "}");
		return t;
	case Tab:
		t= mk_text("{");
		Overall {
			concato(&t, "[");
			concato(&t, Str(cv= convert(Cts(*vp), Yes, No)));
			release(cv);
			concato(&t, "]: ");
			concato(&t, Str(cv= convert(Dts(*vp++), No, No)));
			release(cv);
			if (k != len-1) concato(&t, "; ");
		}
		concato(&t, "}");
		return t;
	default:
		syserr("converting value of unknown type");
		return (value) Dummy;
	}
}
