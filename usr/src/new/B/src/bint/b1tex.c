/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
 * $Header: b1tex.c,v 1.4 85/08/22 16:52:36 timo Exp $
 */

/* B texts */

#include "b.h"
#include "b1obj.h"
#ifndef INTEGRATION
#include "b0con.h"
#include "b1mem.h"
#include "b1btr.h"
#include "b1val.h"
#endif
#include "b1tlt.h"
#include "b3err.h"

#ifndef INTEGRATION

/*
 * Operations on texts represented as B-trees.
 *
 * Comments:
 * - The functions with 'i' prepended (ibehead, etc.) do no argument
 *   checking at all.  They actually implement the planned behaviour
 *   of | and @, where out-of-bounds numerical values are truncated
 *   rather than causing errors ("abc"|100 = "abc"@-100 = "abc").
 * - The 'size' field of all texts must fit in a C int.  If the result of
 *   ^ or ^^ would exceed Maxint in size, a user error is signalled.  If
 *   the size of the *input* value(s) of any operation is Bigsize, a syserr
 *   is signalled.
 * - Argument checking: trims, concat and repeat must check their arguments
 *   for user errors.
 * - t^^n is implemented with an algorithm similar to the 'square and
 *   multiply' algorithm for x**n, using the binary representation of n,
 *   but it uses straightforward 'concat' operations.  A more efficient
 *   scheme is possible [see IW219], but small code seems more important.
 * - Degenerated cases (e.g. t@1, t|0, t^'' or t^^n) are not optimized,
 *   but produce the desired result by virtue of the algorithms used.
 *   The extra checking does not seem worth the overhead for the
 *   non-degenerate cases.
 * - The code for PUT v IN t@h|l is still there, but it is not compiled,
 *   as the interpreter implements the same strategy directly.
 * - 'trim()' is only used by f_uname in "b3fil.c".
 * - Code for outputting texts has been added.	This is called from wri()
 *   to output a text, and has running time O(n), compared to O(n log n)
 *   for the old code in wri().
 *
 * *** WARNING ***
 * - The 'zip' routine and its subroutine 'copynptrs' assume that items and
 *   pointers are stored contiguously, so that &Ptr(p, i+1) == &Ptr(p, i)+1
 *   and &[IB]char(p, i+1) == &[IB]char(p, i)+1.  For pointers, the order
 *   might be reversed in the future; then change the macro Incr(pp, n) below
 *   to *decrement* the pointer!
 * - Mkbtext and bstrval make the same assumption about items (using strncpy
 *   to move charaters to/from a bottom node).
 */

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#define IsInner(p) (Flag(p) == Inner)
#define IsBottom(p) (Flag(p) == Bottom)

#define Incr(pp, n) ((pp) += (n))

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* make a B text out of a C char */

Visible value mkchar(c) char c; {
	char buf[2];
	buf[0] = c;
	buf[1] = '\0';
	return mk_text(buf);
}

Visible char charval(v) value v; {
	if (!Character(v))
		syserr(MESS(1600, "charval on non-char"));
	return Bchar(Root(v), 0);
}

Visible bool character(v) value v; {
	return Character(v);
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Hidden btreeptr mkbtext(s, len) string s; int len; {
	btreeptr p; int chunk, i, n, nbig;

	/*
	 * Determine level of tree.
	 * This is done for each inner node anew, to avoid having
	 * to keep an explicit stack.
	 * Problem is: make sure that for each node at the same
	 * level, the computation indeed finds the same level!
	 * (Don't care about efficiency here; in practice the trees
	 * built by mk_text rarely need more than two levels.)
	 */
	chunk = 0;
	i = Maxbottom; /* Next larger chunk size */
	while (len > i) {
		chunk = i;
		i = (i+1) * Maxinner + Maxinner;
	}
	n = len / (chunk+1); /* Number of items at this level; n+1 subtrees */
	chunk = len / (n+1); /* Use minimal chunk size for subtrees */
	p = grabbtreenode(chunk ? Inner : Bottom, Ct);
	Size(p) = len;
	Lim(p) = n;
	if (!chunk)
		strncpy(&Bchar(p, 0), s, len);
	else {
		nbig = len+1 - (n+1)*chunk;
			/* There will be 'nbig' nodes of size 'chunk'. */
			/* The remaining 'n-nbig' will have size 'chunk-1'. */
		for (i = 0; i < n; ++i) {
			Ptr(p, i) = mkbtext(s, chunk);
			s += chunk;
			Ichar(p, i) = *s++;
			len -= chunk+1;
			if (--nbig == 0)
				--chunk; /* This was the last 'big' node */
		}
		Ptr(p, i) = mkbtext(s, len);
	}
	return p;
}

Visible value mk_text(s) string s; {
	value v; int len = strlen(s);

	v = grab_tlt(Tex, Ct);
	if (len == 0)
		Root(v) = Bnil;
	else
		Root(v) = mkbtext(s, len);
	return v;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Hidden string bstrval(buf, p) string buf; btreeptr p; {
	/* Returns *next* available position in buffer */
	int i, n = Lim(p);
	if (IsInner(p)) {
		for (i = 0; i < n; ++i) {
			buf = bstrval(buf, Ptr(p, i));
			*buf++ = Ichar(p, i);
		}
		return bstrval(buf, Ptr(p, i));
	}
	strncpy(buf, &Bchar(p, 0), n);
	return buf+n;
}

Visible string strval(v) value v; {
	static char *buffer; int len = Tltsize(v);
	if (len == Bigsize) syserr(MESS(1601, "strval on big text"));
	if (len == 0) return "";
	if (buffer != NULL)
		regetmem(&buffer, (unsigned) len+1);
	else
		buffer = getmem((unsigned) len+1);
	*bstrval(buffer, Root(v)) = '\0';
	return buffer;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

typedef struct stackelem {
	btreeptr s_ptr;
	int s_lim;
} stackelem;

typedef stackelem stack[Maxheight];
typedef stackelem *stackptr;

#define Snil ((stackptr)0)

#define Push(s, p, l) ((s)->s_ptr = (p), ((s)->s_lim = (l)), (s)++)
#define Pop(s, p, l) (--(s), (p) = (s)->s_ptr, (l) = (s)->s_lim)

extern stackptr unzip();
extern Procedure cpynptrs();
extern int movnptrs();

Hidden btreeptr zip(s1, sp1, s2, sp2) stackptr s1, sp1, s2, sp2; {
	btreeptr p1, p2, newptr[2]; int l1, l2, i, n, n2;
#define q1 newptr[0]
#define q2 newptr[1]
	char newitem; bool overflow, underflow, inner;
	char *cp; btreeptr *pp;
	char cbuf[2*Maxbottom]; btreeptr pbuf[2*Maxinner+2];

	while (s1 < sp1 && s1->s_lim == 0)
		++s1;
	while (s2 < sp2 && s2->s_lim == Lim(s2->s_ptr))
		++s2;
	inner = overflow = underflow = No;
	q1 = Bnil;
	while (s1 < sp1 || s2 < sp2) {
		if (s1 < sp1)
			Pop(sp1, p1, l1);
		else
			p1 = Bnil;
		if (s2 < sp2)
			Pop(sp2, p2, l2);
		else
			p2 = Bnil;
		cp = cbuf;
		if (p1 != Bnil) {
			strncpy(cp, (inner ? &Ichar(p1, 0) : &Bchar(p1, 0)), l1);
			cp += l1;
		}
		if (overflow)
			*cp++ = newitem;
		n = cp - cbuf;
		if (p2 != Bnil) {
			strncpy(cp, (inner ? &Ichar(p2, l2) : &Bchar(p2, l2)), Lim(p2)-l2);
			n += Lim(p2)-l2;
		}
		if (inner) {
			pp = pbuf; /***** Change if reverse direction! *****/
			if (p1 != Bnil) {
				cpynptrs(pp, &Ptr(p1, 0), l1);
				Incr(pp, l1);
			}
			movnptrs(pp, newptr, 1+overflow);
			Incr(pp, 1+overflow);
			if (p2 != Bnil) {
				cpynptrs(pp, &Ptr(p2, l2+1), Lim(p2)-l2);
				Incr(pp, Lim(p2)-l2);
			}
			if (underflow) {
				underflow= No;
				n= uflow(n, p1 ? l1 : 0, cbuf, pbuf, Ct);
			}
		}
		overflow = No;
		if (n > (inner ? Maxinner : Maxbottom)) {
			overflow = Yes;
			n2 = (n-1)/2;
			n -= n2+1;
		}
		else if (n < (inner ? Mininner : Minbottom))
			underflow = Yes;
		q1 = grabbtreenode(inner ? Inner : Bottom, Ct);
		Lim(q1) = n;
		cp = cbuf;
		strncpy((inner ? &Ichar(q1, 0) : &Bchar(q1, 0)), cp, n);
		cp += n;
		if (inner) {
			pp = pbuf;
			i = movnptrs(&Ptr(q1, 0), pp, n+1);
			Incr(pp, n+1);
			n += i;
		}
		Size(q1) = n;
		if (overflow) {
			newitem = *cp++;
			q2 = grabbtreenode(inner ? Inner : Bottom, Ct);
			Lim(q2) = n2;
			strncpy((inner ? &Ichar(q2, 0) : &Bchar(q2, 0)), cp, n2);
			if (inner)
				n2 += movnptrs(&Ptr(q2, 0), pp, n2+1);
			Size(q2) = n2;
		}
		inner = Yes;
	}
	if (overflow)
		q1 = mknewroot(q1, (itemptr)&newitem, q2, Ct);
	return q1;
#undef q1
#undef q2
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Hidden value ibehead(v, h) value v; int h; { /* v@h */
	stack s; stackptr sp;
	sp = (stackptr) unzip(Root(v), h-1, s);
	v = grab_tlt(Tex, Ct);
	Root(v) = zip(Snil, Snil, s, sp);
	return v;
}

Hidden value icurtail(v, t) value v; int t; { /* v|t */
	stack s; stackptr sp;
	sp = (stackptr) unzip(Root(v), t, s);
	v = grab_tlt(Tex, Ct);
	Root(v) = zip(s, sp, Snil, Snil);
	return v;
}

Hidden value iconcat(v, w) value v, w; { /* v^w */
	stack s1, s2;
	stackptr sp1 = (stackptr) unzip(Root(v), Tltsize(v), s1);
	stackptr sp2 = (stackptr) unzip(Root(w), 0, s2);
	v = grab_tlt(Tex, Ct);
	Root(v) = zip(s1, sp1, s2, sp2);
	return v;
}

#define Odd(n) (((n)&1) != 0)

Hidden value irepeat(v, n) value v; int n; { /* v^^n */
	value x, w = grab_tlt(Tex, Ct);
	Root(w) = Bnil;
	v = copy(v);
	while (n > 0) {
		if (Odd(n)) {
			w = iconcat(x = w, v);
			release(x);
		}
		n /= 2;
		if (n == 0)
			break;
		v = iconcat(x = v, v);
		release(x);
	}
	release(v);
	return w;
}

#ifdef UNUSED_CODE
Hidden value jrepeat(v, n) value v; int n; { /* v^^n, recursive solution */
	value w, x;
	if (n <= 1) {
		if (n == 1)
			return copy(v);
		w = grab_tlt(Tex, Ct);
		Root(w) = Bnil;
		return w;
	}
	w = jrepeat(v, n/2);
	w = iconcat(x = w, w);
	release(x);
	if (Odd(n)) {
		w = iconcat(x = w, v);
		release(x);
	}
	return w;
}
#endif UNUSED_CODE

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Visible value curtail(t, after) value t, after; {
	int syzcurv, syztext;

	if (!Is_text(t)) {
		reqerr(MESS(1602, "in t|n, t is not a text"));
		return Vnil;
	}
	if (!Is_number(after)) {
		reqerr(MESS(1603, "in t|n, n is not a number"));
		return Vnil;
	}
	syztext = Tltsize(t);
	if (syztext == Bigsize)
		syserr(MESS(1604, "curtail on very big text"));
	if (large(after) || (syzcurv = intval(after)) < 0
		|| syztext < syzcurv) {
		reqerr(MESS(1605, "in t|n, n is out of bounds"));
		return Vnil;
	}
	return icurtail(t, syzcurv);
}

Visible value behead(t, before) value t, before; {
	int syzbehv, syztext;

	if (!Is_text(t)) {
		reqerr(MESS(1606, "in t@n, t is not a text"));
		return Vnil;
	}
	if (!Is_number(before)) {
		reqerr(MESS(1607, "in t@n, n is not a number"));
		return Vnil;
	}
	syztext = Tltsize(t);
	if (syztext == Bigsize) syserr(MESS(1608, "behead on very big text"));
	if (large(before) || (syzbehv = intval(before)) <= 0
		|| syztext < syzbehv-1) {
		reqerr(MESS(1609, "in t@n, n is out of bounds"));
		return Vnil;
	}
	return ibehead(t, syzbehv);
}

#ifdef NOT_USED
Visible value trim(v, b, c) value v; intlet b, c; { /*temporary*/
	/* Only used in f_uname */
	int len= Tltsize(v);
	value r= ibehead(v, b+1), s;
	s= icurtail(r, len-b-c); release(r);
	return s;
}
#endif

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Visible value concat(tleft, tright) value tleft, tright; {
	int syzleft, syzright;
	if (!Is_text(tleft) || !Is_text(tright)) {
		reqerr(MESS(1610, "in t^u, t or u is not a text"));
		return Vnil;
	}
	syzleft = Tltsize(tleft);
	syzright =  Tltsize(tright);
	if (syzleft == Bigsize || syzright == Bigsize)
		syserr(MESS(1611, "concat on very big text"));
	if (syzleft > Maxint-syzright
		|| syzright > Maxint-syzleft) {
		reqerr(MESS(1612, "in t^u, the result is too long"));
		return Vnil;
	}
	return iconcat(tleft, tright);
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Visible value repeat(t, n) value t, n; {
	int tsize, k;

	if (!Is_text(t)) {
		reqerr(MESS(1613, "in t^^n, t is not a text"));
		return Vnil;
	}
	if (!Is_number(n)) {
		reqerr(MESS(1614, "in t^^n, n is not a number"));
		return Vnil;
	}
	if (numcomp(n, zero) < 0) {
		reqerr(MESS(1615, "in t^^n, n is negative"));
		return Vnil;
	}
	tsize = Tltsize(t);
	if (tsize == 0) return copy(t);

	if (large(n) || Maxint/tsize < (k = intval(n))) {
		reqerr(MESS(1616, "in t^^n, the result is too long"));
		return Vnil;
	}
	return irepeat(t, k);
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Visible Procedure wrtext(putch, v, quote) int (*putch)(); value v; char quote; {
	if (v == Vnil || !Is_text(v)) {
		(*putch)('?');
		return;
	}
	if (quote) (*putch)(quote);
	if (Root(v) != Bnil) wrbtext(putch, Root(v), quote);
	if (quote) (*putch)(quote);
}

Hidden Procedure wrbtext(putch, p, quote)
 int (*putch)(); btreeptr p; char quote; {
	int i, n = Lim(p); char c;
	if (IsInner(p)) {
		for (i = 0; still_ok && i < n; ++i) {
			wrbtext(putch, Ptr(p, i), quote);
			c = Ichar(p, i);
			(*putch)(c);
			if (quote && (c == quote || c == '`')) (*putch)(c);
		}
		wrbtext(putch, Ptr(p, i), quote);
	}
	else if (quote) {
		for (i = 0; i < n; ++i) {
			c = Bchar(p, i);
			(*putch)(c);
			if (c == quote || c == '`') (*putch)(c);
		}
	}
	else {
		for (i = 0; i < n; ++i) (*putch)(Bchar(p, i));
	}
}

#else INTEGRATION

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
	if (!Is_text(v) || Length(v) != 1) error(MESS(1617, "value not a character"));
	return *Str(v);
}

Visible string strval(v) value v; {
	return Str(v);
}

Visible value concat(s, t) value s, t; {
	if (Type(s) != Tex)
		error(MESS(1618, "in t^u, t is not a text"));
	else if (Type(t) != Tex)
		error(MESS(1619, "in t^u, t is a text, but u is not"));
	else {
		value c= grab_tex(Length(s)+Length(t));
		strcpy(Str(c), Str(s)); strcpy(Str(c)+Length(s), Str(t));
		return c;
	}
	return grab_tex(0);
}

#define VERSION2

Visible Procedure concato(s, t) value *s; string t; {
	if (Type(*s) != Tex)
		error(MESS(1620, "attempt to join text with non-text"));
	else {
#ifdef VERSION1
		xtndtex(s, strlen(t));
		strcat(Str(*s), t);
#endif
#ifdef VERSION2
		value v= mk_text(t);
		value w= concat(*s, v);
		release(*s); release(v);
		*s= w;
#endif
	}
}

Visible value trim(v, B, C) value v; intlet B, C; {
	intlet len= Length(v), k;
	if (Type(v) != Tex)
		error(MESS(1621, "trim (@ or |) applied to non-text"));
	else if (B < 0 || C < 0 || B+C > len)
		error(MESS(1622, "trim (@ or |) out of bounds"));
	else {
		value w= grab_tex(len-=(B+C));
		string vp= Str(v)+B, wp= Str(w);
		Overall *wp++= *vp++; *wp= '\0';
		return w;
	}
	return grab_tex(0);
}

Visible Procedure
putintrim(pn, head, tail, str)
	value *pn;
	intlet head, tail;
	string str;
{
	value v = *pn;
	intlet len= Length(v);

	if (Type(v) != Tex)
		error(MESS(1623, "putintrim (@ or |) applied to non-text"));
	else if (head < 0 || tail < 0 || head+tail > len)
		error(MESS(1624, "putintrim (@ or |) out of bounds"));
	else {
		value w = head == 0 ? mk_text("") :
			head == len ? copy(v) : trim(v, 0, len - head);
		if (*str)
			concato(&w, str);
		if (tail > 0)
			concato(&w, Str(v)+(len - tail));
		release(v);
		*pn = w;
	}
}

Visible value curtail(v, n) value v, n; {
	intlet c= intval(n);
	v= trim(v, 0, Length(v) - c);
	return v;
}

Visible value behead(v, n) value v, n; {
	intlet b= intval(n);
	v= trim(v, b-1, 0);
	return v;
}

Visible value repeat(x, y) value x, y; {
	intlet i= propintlet(intval(y));
	if (Type(x) != Tex)
		error(MESS(1625, "in t^^n, t is not a text"));
	if (i < 0)
		error(MESS(1626, "in t^^n, n is negative"));
	else {
		value r; string xp, rp; intlet p, q, xl= Length(x);
		r= grab_tex(propintlet(i*xl));
		rp= Str(r);
		for (p= 0; p < i; p++) {
			xp= Str(x);
			for (q= 0; q < xl; q++) *rp++= *xp++;
		}
		*rp= '\0';
		return r;
	}
	return grab_tex(0);
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
	literal type= Type(v); intlet len= Length(v), k; value *vp= Ats(v);
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
		syserr(MESS(1627, "converting value of unknown type"));
		return (value) Dummy;
	}
}

#endif INTEGRATION
