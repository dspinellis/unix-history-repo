/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
 * $Header: b1lta.c,v 1.4 85/08/22 16:49:05 timo Exp $
 */

/* Access and update lists and tables */

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#ifndef INTEGRATION
#include "b1btr.h"
#include "b1val.h"
#include "b3err.h"
#include "b3scr.h" /* For at_nwl */
#endif
#include "b1tlt.h"

#ifndef INTEGRATION

#ifndef DEBUG
#define check(v, where) /*nothing*/
#endif DEBUG

#define IsInner(p) (Flag(p) == Inner)
#define IsBottom(p) (Flag(p) == Bottom)

#define _Pxitm(p, l, iw) (IsInner(p) ? Piitm(p, l, iw) : Pbitm(p, l, iw))

Hidden itemptr Pxitm(p, l, iw) btreeptr p; intlet l, iw; {
	return _Pxitm(p, l, iw);
}

#define Inil ((itemptr)0)

#define Incr(p, n) ((p) += (n))

Visible width itemwidth[4]= {Cw, Lw, Tw, Kw};

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

typedef struct {
	btreeptr s_ptr;
	int s_lim;
} finger[Maxheight], *fingertip;

#define Snil ((fingertip)0)

#define Push(s, p, l) ((s)->s_ptr= (p), ((s)->s_lim= (l)), (s)++)
#define Top(s, p, l) ((p)= ((s)-1)->s_ptr, (l)= ((s)-1)->s_lim)
#define Drop(s) (--(s))
#define Pop(s, p, l) (--(s), (p)= (s)->s_ptr, (l)= (s)->s_lim)
	/* Pop(s, p, l) is equivalent to Top(s, p, l); Drop(s) */

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Visible fingertip unzip(p, at, s) btreeptr p; int at; fingertip s; {
	int syz; intlet l;
	if (p == Bnil) return s;
	for (;;) {
		if (at <= 0) l= 0;
		else if (at >= Size(p)) l= Lim(p);
		else if (IsInner(p)) {
			l= 0;
			while (at > (syz= Size(Ptr(p, l)))) {
				++l;
				at -= syz+1;
			}
		}
		else if (at >= Lim(p)) l= Lim(p) - 1; /* for Irange/Crange */
		else l= at; /* Assume Bottom */
		Push(s, p, l);
		if (!IsInner(p)) break;
		p= Ptr(p, l);
	}
	return s;
}

Visible Procedure cpynptrs(to, from, n) btreeptr *to, *from; int n; {
	while (--n >= 0) {
		*to= copybtree(*from);
		Incr(to, 1);
		Incr(from, 1);
	}
}

Visible int movnptrs(to, from, n) btreeptr *to, *from; int n; {
	int syz= 0; /* Collects sum of sizes */
	while (--n >= 0) {
		*to= *from;
		syz += Size(*from);
		Incr(to, 1);
		Incr(from, 1);
	}
	return syz;
}

/* The following two routines may prove machine-dependent when moving
   N pointers is not equivalent to moving N*sizeof(pointer) characters.
   Also, the latter may be slower. */

Visible Procedure movnitms(to, from, n, iw) itemptr to, from; intlet n, iw; {
	register char *t= (char *)to, *f= (char *)from;
	n *= iw;
	while (--n >= 0) *t++ = *f++;
}

Hidden Procedure shift(p, l, iw) btreeptr p; intlet l, iw; {
	/* Move items and pointers from l upwards one to the right */
	btreeptr *to, *from;
	intlet n= (Lim(p)-l) * iw; bool inner= IsInner(p);
	char *f= (char *) Pxitm(p, Lim(p), iw);
	char *t= f+iw;
	while (--n >= 0) *--t = *--f;
	if (inner) {
		from= &Ptr(p, Lim(p));
		to= from;
		Incr(to, 1);
		n= Lim(p)-l;
		while (--n >= 0) {
			*to= *from;
			Incr(to, -1);
			Incr(from, -1);
		}
	}
}

Visible Procedure cpynitms(to, from, n, it) itemptr to, from; intlet n, it; {
	intlet i, iw= Itemwidth(it);
	movnitms(to, from, n, iw);
	switch (it) {
	case Lt:
	case Kt:
	case Tt:
		for (i= 0; i < n; ++i) {
			copy(Keyval(to));
			if (it == Tt) copy(Ascval(to));
			else if (it == Kt) Ascval(to)= Vnil;
			to= (itemptr) ((char*)to + iw);
		}
	}
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* Uflow uses a character array to hold the items.  This may be wrong. */

Visible Procedure uflow(n, l, cbuf, pbuf, it)
 intlet n, l; char cbuf[]; btreeptr pbuf[]; intlet it; {
	char ncbuf[3*Maxbottom*sizeof(item)], *cp= ncbuf;
	btreeptr npbuf[3*Maxinner], *pp= npbuf, q;
	intlet iw= Itemwidth(it); bool inner= IsInner(pbuf[0]);
	intlet i, j, k, nn, l1= l>0 ? l-1 : l, l2= l<n ? l+1 : l;
	for (i= l1; i <= l2; ++i) {
		q= pbuf[i]; j= Lim(q);
		cpynitms((itemptr)cp, Pxitm(q, 0, iw), j, it);
		cp += j*iw;
		if (inner) {
			cpynptrs(pp, &Ptr(q, 0), j+1);
			Incr(pp, j+1);
		}
		if (i < l2) {
			movnitms((itemptr)cp, (itemptr)(cbuf+i*iw), 1, iw);
			cp += iw;
		}
		relbtree(q, it);
	}
	nn= (cp-ncbuf)/iw;
	k= inner ? Maxinner : Maxbottom;
	if (nn <= k) k= 1;
	else if (nn <= 2*k) k= 2;
	else k= 3;
	/* (k <= l2-l1+1) */
	cp= ncbuf; pp= npbuf;
	for (i= 0; i < k; ++i) {
		if (i > 0) {
			movnitms((itemptr)(cbuf+(l1+i-1)*iw), (itemptr)cp, 1, iw);
			cp += iw;
			--nn;
		}
		pbuf[l1+i]= q= grabbtreenode(inner ? Inner : Bottom, it);
		Lim(q)= Size(q)= j= nn/(k-i); nn -= j;
		movnitms(Pxitm(q, 0, iw), (itemptr)cp, j, iw);
		cp += j*iw;
		if (inner) {
			Size(q) += movnptrs(&Ptr(q, 0), pp, j+1);
			Incr(pp, j+1);
		}
	}
	if (k < l2-l1+1) {
		movnitms((itemptr)(cbuf+(l1+k-1)*iw), (itemptr)(cbuf+l2*iw), n-l2, iw);
		VOID movnptrs(pbuf+l1+k, pbuf+l2+1, n-l2);
		n -= l2-l1+1 - k;
	}
	return n;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* Low level access routines */

/* Meaning of 'flags' parameter to searchkey: */
#define NORMAL 0
#define UNIQUE 1 /* uniquify visited nodes */
#define DYAMAX 2 /* special for dyadic max (= previous element) */
#define DYAMIN 4 /* special for dyadic min (= next element) */

Hidden bool searchkey(v, pw, flags, ft)
 value v, *pw; int flags; fingertip *ft; {
	btreeptr p, *pp;
	intlet l, mid, h, it= Itemtype(*pw), iw= Itemwidth(it);
	bool inner; relation r;
	pp= &Root(*pw);
	if (*pp == Bnil) return No;
	if (flags&UNIQUE) {
		killranges(pw);
		uniql(pw);
		pp= &Root(*pw);
	}
	for (;;) {
		if (flags&UNIQUE) uniqlbtreenode(pp, it);
		p= *pp;
		inner= IsInner(p);
		l= 0; h= Lim(p);
		r= 1; /* For the (illegal?) case that there are no items */
		while (l < h) { /* Binary search in {l..h-1} */
			mid= (l+h)/2;
			r= compare(v, Keyval(Pxitm(p, mid, iw)));
			if (!comp_ok) return No;
			if (r == 0) { /* Found it */
				if (flags&(DYAMIN|DYAMAX)) {
					/* Pretend not found */
					if (flags&DYAMIN) r= 1;
					else r= -1;
				}
				else { /* Normal case, report success */
					l= mid;
					break;
				}
			}
			if (r < 0) h= mid; /* Continue in {l..mid-1} */
			else if (r > 0) l= mid+1; /* Cont. in {mid+1..h-i} */
		}
		Push(*ft, p, l);
		if (r == 0) return Yes;
		if (!inner) {
			switch (Flag(p)) {
			case Irange: return h > 0 && l < Lim(p) && integral(v);
			case Crange: return h > 0 && l < Lim(p) && character(v);
			default: case Bottom: return No;
			}
		}
		pp= &Ptr(p, l);
	}
}

Hidden Procedure killranges(pv) value *pv; {
	btreeptr p= Root(*pv);
	if (p == Bnil) return;
	switch (Flag(p)) {
	case Crange: killCrange(p, pv); break;
	case Irange: killIrange(p, pv); break;
	}
}

Hidden Procedure killCrange(p, pv) btreeptr p; value *pv; {
	value w; intlet lwbchar= Lwbchar(p), upbchar= Upbchar(p);
	release(*pv);
	*pv= mk_elt();
	do {
		w= mkchar(lwbchar);
		insert(w, pv);
		release(w);
	} while (++lwbchar <= upbchar);
}

Hidden Procedure killIrange(p, pv) btreeptr p; value *pv; {
	value w, lwb= copy(Lwbval(p)), upb= copy(Upbval(p));
	release(*pv);
	*pv= mk_elt();
	do {
		insert(lwb, pv);
		if (compare(lwb, upb) >= 0) break;
		w= lwb;
		lwb= sum(lwb, one);
		release(w);
	} while (still_ok);
	release(lwb);
	release(upb);
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Hidden btreeptr rem(f, ft, it) fingertip f, ft; intlet it; {
	btreeptr p, q, *pp; itemptr ip; intlet l, iw= Itemwidth(it);
	bool inner, underflow;
	Pop(ft, p, l);
	inner= IsInner(p);
	if (!inner) ip= Pbitm(p, l, iw);
	else {
		ip= Piitm(p, l, iw);
		do {
			Push(ft, p, l);
			uniqlbtreenode(pp= &Ptr(p, l), it);
			p= *pp;
			l= Lim(p);
		} while (IsInner(p));
		inner= No;
		l -= 2; /* So the movnitms below works fine */
	}
	release(Keyval(ip));
	if (it == Tt || it == Kt) release(Ascval(ip));
	--Lim(p);
	movnitms(ip, Pbitm(p, l+1, iw), Lim(p)-l, iw);
	for (;;) {
		underflow= Lim(p) < (inner ? Mininner : Minbottom);
		--Size(p);
		if (ft == f) break;
		Pop(ft, p, l);
		if (underflow)
			Lim(p)= uflow(Lim(p), l, (string)Piitm(p, 0, iw), &Ptr(p, 0), it);
		inner= Yes;
	}
	if (Lim(p) == 0) { /* Reduce tree level */
		q= p;
		p= inner ? copybtree(Ptr(p, 0)) : Bnil;
		relbtree(q, it);
	}
	return p;
}

Hidden btreeptr ins(ip, f, ft, it) itemptr ip; fingertip f, ft; intlet it; {
	item new, old; btreeptr p, q= Bnil, pq, oldq, *pp;
	intlet l, iw= Itemwidth(it), nn, np, nq; bool inner, overflow;
	if (ft == f) {
		/* unify with rest? */
		p= grabbtreenode(Bottom, it);
		movnitms(Pbitm(p, 0, iw), ip, 1, iw);
		Lim(p)= Size(p)= 1;
		return p;
	}
	Pop(ft, p, l);
	while (IsInner(p)) {
		Push(ft, p, l);
		uniqlbtreenode(pp= &Ptr(p, l), it);
		p= *pp;
		l= Lim(p);
	}
	overflow= Yes; inner= No;
	for (;;) {
		pq= p;
		if (overflow) {
			oldq= q;
			movnitms(&old, ip, 1, iw);
			ip= &new;
			overflow= Lim(p) == (inner ? Maxinner : Maxbottom);
			if (overflow) {
				nn= Lim(p); np= nn/2; nq= nn-np-1;
				q= grabbtreenode(inner ? Inner : Bottom, it);
				Size(q)= Lim(q)= nq;
				movnitms(&new, Pxitm(p, np, iw), 1, iw);
				movnitms(Pxitm(q, 0, iw), Pxitm(p, np+1, iw), nq, iw);
				if (inner) 
					Size(q) += movnptrs(&Ptr(q, 0), &Ptr(p, np+1), nq+1);
				Lim(p)= np;
				Size(p) -= Size(q)+1;
				if (l > np) {
					l -= np+1;
					pq= q;
				}
			}
			shift(pq, l, iw);
			movnitms(Pxitm(pq, l, iw), &old, 1, iw);
			++Lim(pq);
			if (inner) {
				Size(p) -= Size(oldq);
				Size(pq) += movnptrs(&Ptr(pq, l+1), &oldq, 1);
			}
		}
		++Size(pq);
		if (ft == f) break;
		Pop(ft, p, l);
		inner= Yes;
	}
	if (overflow)
		p= mknewroot(p, ip, q, it);
	return p;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* Tables */

Visible Procedure replace(a, pt, k) value a, *pt, k; {
	item new; finger f; fingertip ft= f; btreeptr p; value *pp;
	intlet it, iw, l;
	check(*pt, " (replace in)");
	if (Is_ELT(*pt)) { (*pt)->type= Tab; Itemtype(*pt)= Tt; }
	it= Itemtype(*pt);
	if (searchkey(k, pt, UNIQUE, &ft)) {
		iw= Itemwidth(it);
		Pop(ft, p, l);
		pp= &Ascval(Pxitm(p, l, iw));
		release(*pp);
		*pp= copy(a);
	}
	else {
		if (!comp_ok) return;
		Keyval(&new)= copy(k); Ascval(&new)= copy(a);
		Root(*pt)= ins(&new, f, ft, it);
	}
	check(*pt, " (replace out)");
}

Visible /*bool*/ delete(pt, k) value *pt, k; {
	finger f; fingertip ft= f; intlet it= Itemtype(*pt);
	check(*pt, " (delete in)");
	if (!searchkey(k, pt, UNIQUE, &ft)) return No;
	Root(*pt)= rem(f, ft, it);
	check(*pt, " (delete out)");
	return Yes;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* Lists */

Visible Procedure insert(v, pl) value v, *pl; {
	item new; finger f; fingertip ft= f; intlet it= Itemtype(*pl);
	check(*pl, " (insert in)");
	if (Is_ELT(*pl)) (*pl)->type= Lis;
	VOID searchkey(v, pl, UNIQUE, &ft);
	if (!comp_ok) return;
	Keyval(&new)= copy(v); Ascval(&new)= Vnil;
	Root(*pl)= ins(&new, f, ft, it);
	check(*pl, " (insert out)");
}

Visible Procedure remove(v, pl) value v, *pl; {
	if (!delete(pl, v) && still_ok)
		error(MESS(100, "removing non-existent list entry"));
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* Miscellaneous accesses */

Hidden itemptr findkey(key, pv, flags) value key, *pv; int flags; {
	finger f; fingertip ft= f; btreeptr p;
	intlet it= Itemtype(*pv), iw= Itemwidth(it), l;
	if (!searchkey(key, pv, flags, &ft)) return Inil;
	Pop(ft, p, l);
	return Pxitm(p, l, iw);
}

Visible value associate(t, k) value t, k; { /* t[k] */
	itemptr ip;
	if (!Is_table(t)) {
		error(MESS(101, "in t[k], t is not a table"));
		return Vnil;
	}
	ip= findkey(k, &t, NORMAL);
	if (!ip) {
		if (still_ok) /* Could be type error; then shut up! */
			error(MESS(102, "key not in table"));
		return Vnil;
	}
	return copy(Ascval(ip));
}

Visible value* adrassoc(t, k) value t, k; { /* &t[k] */
	itemptr ip= findkey(k, &t, NORMAL);
	if (!ip) return Pnil;
	return &Ascval(ip);
}

Visible bool uniq_assoc(t, k) value t, k; { /* uniql(&t[k]) */
	itemptr ip= findkey(k, &t, UNIQUE);
	if (ip == Inil) return No;
	uniql(&Ascval(ip));
	return Yes;
}

Visible bool in_keys(k, t) value k, t; { /* k in keys t */
	return findkey(k, &t, NORMAL) != Inil;
}

Visible value keys(t) value t; { /* keys t */
	value v;
	if (!Is_table(t)) {
		error(MESS(103, "in keys t, t is not a table"));
		return Vnil;
	}
	v= grab_tlt(Lis, Kt);
	Root(v)= copybtree(Root(t));
	return v;
}

/* WARNING!  The following routine is not reentrant, since (for range lists)
   it may return a pointer to static storage. */

Hidden itemptr getkth(k, v) int k; value v; {
	finger f; fingertip ft; btreeptr p;
	intlet it= Itemtype(v), iw= Itemwidth(it), l;
	static item baked; value vk;
	if (Root(v) == Bnil) return Inil;
	ft= unzip(Root(v), k, f);
	do {
		if (ft == f) return Inil;
		Pop(ft, p, l);
	} while (l >= Lim(p));
	switch (Flag(p)) {
		default:
		case Inner:
		case Bottom:
			return Pxitm(p, l, iw);
		case Irange:
			release(Keyval(&baked));
			Keyval(&baked)= sum(Lwbval(p), vk= mk_integer(k));
			release(vk);
			return &baked;
		case Crange:
			release(Keyval(&baked));
			Keyval(&baked)= mkchar(Lwbchar(p) + k);
			return &baked;
	}
}

Visible value* key(v, k) value v; intlet k; { /* &(++k th'of keys v) */
	itemptr ip= getkth(k, v);
	return ip ? &Keyval(ip) : Pnil;
}

Visible value* assoc(v, k) value v; intlet k; { /* &v[++k th'of keys v] */
	itemptr ip= getkth(k, v);
	return ip ? &Ascval(ip) : Pnil;
}

Visible value thof(k, v) int k; value v; { /* k th'of v */
	itemptr ip= getkth(k-1, v);
	if (!ip) return Vnil;
	switch (Type(v)) {
	case Tex: return mkchar(Charval(ip));
	case Lis: return copy(Keyval(ip));
	case Tab: return copy(Ascval(ip));
	default: return Vnil;
	}
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* Compare B-trees.  Should use fingers, but to keep things simple
   (especially in the presence of range type nodes), doesn't.  This
   makes its behaviour O(N log N), where it could be O(N), alas. */

/* WARNING!  getkth may return a pointer to static storage (when retrieving
   elements from a range list).  Therefore after the second call to getkth,
   the return value of the first may be invalid, but only for lists.
   So we extract the 'Key' values immediately after the call to getkth. */

Visible relation comp_tlt(u, v) value u, v; {
	itemptr up, vp; int k, ulen, vlen, len; relation r= 0;
	bool tex= Is_text(u), tab= Is_table(u);
	value key_u;
	len= ulen= Tltsize(u); vlen= Tltsize(v);
	if (vlen < len) len= vlen;
	for (k= 0; k < len; ++k) {
		up= getkth(k, u);
		if (!tex) key_u= copy(Keyval(up));
		vp= getkth(k, v);
		if (tex) r= Charval(up) - Charval(vp);
		else {
			r= compare(key_u, Keyval(vp));
			release(key_u);
			if (tab && r == 0)
				r= compare(Ascval(up), Ascval(vp));
		}
		if (r != 0) break;
	}
	if (r == 0) r= ulen - vlen;
	return r;
}

/* Compare texts.  When both texts are bottom nodes, compare with
   strncmp(), to speed up the most common use (look-up by the
   system of tags in a symbol table).  Otherwise, call comp_tlt(). */

Visible relation comp_text(u, v) value u, v; {
	btreeptr p, q; int len; relation r;
	if (!Is_text(u) || !Is_text(v)) syserr(MESS(104, "comp_text"));
	p= Root(u), q= Root(v);
	if (p EQ Bnil) return (q EQ Bnil) ? 0 : -1;
	if (q EQ Bnil) return 1;
	if (Flag(p) EQ Bottom && Flag(q) EQ Bottom) {
		len= Lim(p);
		if (Lim(q) < len) len= Lim(q);
		r= strncmp(&Bchar(p, 0), &Bchar(q, 0), len);
		if (r NE 0) return r;
		return Lim(p) - Lim(q);
	}
	return comp_tlt(u, v);
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* Range type nodes */

Visible value mk_numrange(lwb, upb) value lwb, upb; {
	value lis;
	btreeptr proot;

	lis= grab_tlt(Lis, Lt);
	if (numcomp(lwb, upb) > 0)
		Root(lis)= Bnil;
	else {
		Root(lis)= proot= grabbtreenode(Irange, Lt);
		Lwbval(proot)= copy(lwb);
		Upbval(proot)= copy(upb);
		set_size_and_lim(proot);
	}
	return(lis);
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

Visible value mk_charrange(lwb, upb) value lwb, upb; {
	value lis;
	btreeptr proot;
	intlet rsyz;

	lis= grab_tlt(Lis, Lt);
	rsyz= Bchar(Root(upb), 0) - Bchar(Root(lwb), 0) + 1;
	if (rsyz <= 0)
		Root(lis)= Bnil;
	else {
		Root(lis)= proot= grabbtreenode(Crange, Lt);
		Size(proot)= rsyz;
		Lim(proot)= rsyz > 1 ? 2 : 1;
		Lwbval(proot)= copy(lwb);
		Upbval(proot)= copy(upb);
	}
	return lis;
}


/* set size and lim for integer range node */
 
Hidden Procedure set_size_and_lim(pnode) btreeptr pnode; {
	value uml, uml1;

	uml= diff(Upbval(pnode), Lwbval(pnode));
	uml1= sum(uml, one);
	if (large(uml1)) {
		Size(pnode)= Bigsize;
		Lim(pnode)= 2;
		error(MESS(105, "creating list of too many entries"));
	}
	else {
		Size(pnode)= intval(uml1);
		Lim(pnode)= Size(pnode) > 1 ? 2 : 1;
	}
	release(uml);
	release(uml1);
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* Dyadic min, max, size of lists */

Visible value l2min(e, v) value e, v; { /* e min v */
	finger f; fingertip ft= f; btreeptr p;
	intlet it= Itemtype(v), iw= Itemwidth(it), l;
	VOID searchkey(e, &v, DYAMIN, &ft);
	for (;;) {
		if (ft == f) return Vnil;
		Top(ft, p, l);
		if (l < Lim(p)) {
			switch (Flag(p)) {
			case Inner:
				return copy(Keyval(Piitm(p, l, iw)));
			case Bottom:
				return copy(Keyval(Pbitm(p, l, iw)));
			case Irange:
				if (l == 0) return copy(Lwbval(p));
				if (integral(e)) return sum(e, one);
				return ceilf(e);
			case Crange:
				if (l == 0) return copy(Lwbval(p));
				return mkchar(Bchar(Root(e), 0) + 1);
			}
		}
		Drop(ft);
	}
}

Visible value l2max(e, v) value e, v; { /* e max v */
	finger f; fingertip ft= f; btreeptr p;
	intlet it= Itemtype(v), iw= Itemwidth(it), l;
	VOID searchkey(e, &v, DYAMAX, &ft);
	for (;;) {
		if (ft == f) return Vnil;
		Top(ft, p, l);
		--l;
		if (l >= 0) {
			switch (Flag(p)) {
			case Inner:
				return copy(Keyval(Piitm(p, l, iw)));
			case Bottom:
				return copy(Keyval(Pbitm(p, l, iw)));
			case Irange:
				if (l == 1) return copy(Upbval(p));
				if (integral(e)) return diff(e, one);
				return floorf(e);
			case Crange:
				if (l == 1) return copy(Upbval(p));
				return mkchar(Bchar(Root(e), 0) - 1);
			}
		}
		Drop(ft);
	}
}

Visible int l2size(e, v) value e, v; { /* e#v */
	finger f; fingertip ft= f; btreeptr p;
	int count= 0; intlet it= Itemtype(v), iw= Itemwidth(it), l, r;
	VOID searchkey(e, &v, DYAMIN, &ft);
	for (;;) {
		if (ft == f) return count;
		Pop(ft, p, l);
		while (--l >= 0) {
			r= compare(Keyval(Pxitm(p, l, iw)), e);
			if (r != 0) {
				switch (Flag(p)) {
				case Irange: /* See footnote */
					if (l==0 && count==0 && integral(e))
						++count;
					break;
				case Crange: /* See footnote */
					if (l==0 && count==0 && !character(e))
						++count;
					break;
				}
				return count;
			}
			++count;
			while (IsInner(p)) {
				Push(ft, p, l);
				p= Ptr(p, l);
				l= Lim(p);
			}
		}
	}
}

/* Clarification of what happens for x#{a..b}:
 * Consider these five cases: x<a; x=a; a<x<b; x=b; b<x.
 * Only the case a<x<b need be treated specially.  How do we find which
 * case we're in?
 * Searchkey gives us the following values for l on the stack, respectively:
 * 0; 1; 1; 2; 2.  After --l, this becomes -1; 0; 0; 1; 1.
 * In cases x=a or x=b, the compare returns 0, and we go another time
 * through the loop.  So when the compare returns r!=0, the value of l
 * is, respectively: -1; -1; 0; 0; 1.  The -1 cases in fact don't even
 * get at the compare, and the correct count is returned automatically.
 * So we need to do extra work only if l==0, except if x==b.
 * The latter condition is cared for by count==0 (if x==b, count is
 * surely >= 1; if a<x<b, count is surely 0).  This works even when
 * range nodes may be mixed with other node types in one tree.
 */

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#ifdef DEBUG
/* Debug code */

Hidden Procedure check(v, whence) value v; string whence; {
	if (!still_ok) return;
	switch (Type(v)) {
	case ELT:
		return;
	case Lis:
	case Tab:
		break;
	default:
		error3(MESS(106, "value not a list or table"), Vnil,
			MESSMAKE(whence));
		return;
	}
	if (Root(v) != Bnil)
		VOID cktree(Inil, Root(v), Inil, Itemtype(v), whence);
	if (!still_ok && !interrupted) {
		dumptree(Root(v), 0, Itemtype(v));
		printf("\n");
		fflush(stdout);
	}
}

Hidden int cktree(left, p, right, it, whence)
 itemptr left; btreeptr p; itemptr right; intlet it; string whence; {
	/* returns size of checked subtree */
	intlet i, iw= Itemwidth(it); int sz= 0;
	if (!still_ok) return 0;
	if (p == Bnil) {
		error3(MESS(107, "unexpected nil subtree"), Vnil,
			MESSMAKE(whence));
		return 0;
	}
	switch (Flag(p)) {
	case Inner:
		for (i= 0; i < Lim(p); ++i) {
			sz += 1 +
			  cktree(left, Ptr(p, i), Piitm(p, i, iw), it, whence);
			if (!still_ok) return;
			left= Piitm(p, i, iw);
		}
		sz += cktree(left, Ptr(p, i), right, it, whence);
		if (still_ok && sz != Size(p))
			error3(MESS(108, "size mismatch"), Vnil,
				MESSMAKE(whence));
		break;
	case Bottom:
		for (i= 0; i < Lim(p); ++i) {
			if (left != Inil && compare(Keyval(left),
					Keyval(Pbitm(p, i, iw))) > 0) {
				error3(MESS(109, "bottom items out of order"),
					Vnil, MESSMAKE(whence));
				break;
			}
			left= Pbitm(p, i, iw);
			sz++;
		}
		if (still_ok && right != Inil
			&& compare(Keyval(left), Keyval(right)) > 0)
			error3(MESS(110, "bottom items out of order"),
				Vnil, MESSMAKE(whence));
		return sz;
	case Irange:
		if (left != Inil && compare(Keyval(left), Lwbval(p)) > 0
			|| right != Inil
				&& compare(Upbval(p), Keyval(right)) > 0)
			error3(MESS(111, "irange items out of order"), Vnil,
				MESSMAKE(whence));
		sz= Size(p);
	default:
		error3(MESS(112, "bad node type"), Vnil, MESSMAKE(whence));
	}
	return sz;
}
#endif DEBUG

#ifdef NOT_USED
Visible Procedure e_dumptree(v) value v; {
	check(v, "");
	if (still_ok) {
		if (!at_nwl) printf("\n");
		dumptree(Root(v), 0, Itemtype(v));
		printf("\n");
		fflush(stdout);
		at_nwl= Yes;
	}
}
#endif

Hidden Procedure dumptree(p, indent, it) btreeptr p; intlet indent, it; {
	intlet i, iw= Itemwidth(it);
	if (interrupted) return;
	printf("%*s", 3*indent, "");
	if (p == Bnil) { printf("<nil>"); return; }
	switch (Flag(p)) {
	case Inner:
		printf("(\n");
		for (i= 0; !interrupted && i <= Lim(p); ++i) {
			if (i > 0) {
				printf("%*s", 3*indent, "");
				dumpval(Keyval(Piitm(p, i-1, iw)));
				printf("\n");
			}
			dumptree(Ptr(p, i), indent+1, it);
			printf("\n");
		}
		printf("%*s", 3*indent, "");
		printf(")");
		break;
	case Bottom:
		printf("[");
		for (i= 0; i < Lim(p); ++i) {
			if (i > 0) printf(" ");
			dumpval(Keyval(Pbitm(p, i, iw)));
		}
		printf("]");
		break;
	case Irange:
		printf("{");
		dumpval(Lwbval(p));
		printf(" .. ");
		dumpval(Upbval(p));
		printf("}");
		break;
	default:
		printf("?type='%c'?", Flag(p));
		break;
	}
}

Hidden Procedure dumpval(v) value v; {
	if (interrupted) return;
	if (v == Vnil) printf("(nil)");
	else switch(Type(v)) {
	case Num: case Tex: case Lis: case Tab: case ELT: case Com:
		wri(v, No, No, No);
		break;
	default:
		printf("0x%lx", (long)v);
	}
}

#else INTEGRATION

/* B lists */

Visible value list_elem(l, i) value l; intlet i; {
	return List_elem(l, i);
}

Visible insert(v, ll) value v, *ll; {
	intlet len= Length(*ll); register value *lp, *lq;
	intlet k; register intlet kk;
	if (!Is_list(*ll)) {
		error(MESS(113, "inserting in non-list"));
		return;
	}
	VOID found(list_elem, *ll, v, &k);
	if (Unique(*ll) && !Is_ELT(*ll)) {
		xtndlt(ll, 1);
		lq= Ats(*ll)+len; lp= lq-1;
		for (kk= len; kk > k; kk--) *lq--= *lp--;
		*lq= copy(v);
	} else {
		lp= Ats(*ll);
		release(*ll);
		*ll= grab_lis(++len);
		lq= Ats(*ll);
		for (kk= 0; kk < len; kk++) *lq++= copy (kk == k ? v : *lp++);
	}
}

Visible remove(v, ll) value v; value *ll; {
	register value *lp, *lq;
	intlet k, len= Length(*ll);
	if (!Is_list(*ll)) 
		error(MESS(114, "removing from non-list"));
	else if (len == 0)
		error(MESS(115, "removing from empty list"));
	else if (!found(list_elem, *ll, v, &k))
		error(MESS(116, "removing non-existing list entry"));
	else {
		lp= Ats(*ll); /* lp[k] = v */
		if (Unique(*ll)) {
			release(*(lp+=k));
			for (k= k; k < len; k++) {*lp= *(lp+1); lp++;}
			xtndlt(ll, -1);
		} else {
			intlet kk= k;
			lq= Ats(*ll);
			release(*ll);
			*ll= grab_lis(--len);
			lp= Ats(*ll);
			Overall {
				*lp++= copy (*lq++);
				if (k == kk) lq++;
			}
		}
	}
}

Visible value mk_numrange(a, z) value a, z; {
	value l= mk_elt(), m= copy(a), n;

	while (compare(m, z)<=0) {
		insert(m, &l);
		m= sum(n=m, one);
		release(n);
	}
	release(m);
	return l;
}

Visible value mk_charrange(av, zv) value av, zv; {
	char a= charval(av), z= charval(zv);
	value l= grab_lis((intlet) (z-a+1)); register value *ep= Ats(l);
	char m[2];
	m[1]= '\0';
	for (m[0]= a; m[0] <= z; m[0]++) {
		*ep++= mk_text(m);
	}
	return l;
}

/**********************************************************************/

/* B tables */

Visible value* key(v, k) value v; intlet k; { /* k in {0..size-1}; no copy */
	return Key(v, k);
}

Visible value* assoc(v, k) value v; intlet k; { /* k in {0..size-1}; no copy */
	return Assoc(v, k);
}

Visible value associate(v, k) value v; value k; {
	value *p= adrassoc(v, k);
	if (p) return copy(*p);
	error(MESS(117, "key not in table"));
	return Vnil;
}

Visible value keys(ta) value ta; {
	
	if(!Is_table(ta)) {
		error(MESS(118, "in keys t, t is not a table"));
		return grab_lis(0);
	} else {
		value li= grab_lis(Length(ta)), *le, *te= (value *)Ats(ta);
		int k, len= Length(ta);
		le= (value *)Ats(li);
		Overall { *le++= copy(Cts(*te++)); }
		return li;
	}
}

Visible value key_elem(t, i) value t; intlet i; { /*The key of the i-th entry*/
	return *Key(t, i);
}

/* adrassoc returns a pointer to the associate, rather than
   the associate itself, so that the caller can decide if a copy
   should be taken or not. If the key is not found, Pnil is returned. */
Visible value* adrassoc(t, ke) value t, ke; {
	intlet where;
	if (Type(t) != Tab && Type(t) != ELT) {
		error(MESS(119, "selection on non-table"));
		return Pnil;
	}
	return found(key_elem, t, ke, &where) ? Assoc(t, where) : Pnil;
}

Visible Procedure uniq_assoc(ta, ke) value ta, ke; {
	intlet k;
	if (found(key_elem, ta, ke, &k)) {
		uniql(Ats(ta)+k);
		uniql(Assoc(ta,k));
	} else syserr(MESS(120, "uniq_assoc called for non-existent table entry"));
}

Visible Procedure replace(v, ta, ke) value *ta, ke, v; {
	intlet len= Length(*ta); value *tp, *tq;
	intlet k, kk;
	uniql(ta);
	if (Type(*ta) == ELT) (*ta)->type = Tab;
	else if (Type(*ta) != Tab) {
		error(MESS(121, "replacing in non-table"));
		return;
	}
	if (found(key_elem, *ta, ke, &k)) {
		value *a;
		uniql(Ats(*ta)+k);
		a= Assoc(*ta, k);
		uniql(a);
		release(*a);
		*a= copy(v);
		return;
	} else {
		xtndlt(ta, 1);
		tq= Ats(*ta)+len; tp= tq-1;
		for (kk= len; kk > k; kk--) *tq--= *tp--;
		*tq= grab_com(2);
		Cts(*tq)= copy(ke);
		Dts(*tq)= copy(v);
	}
}

Visible bool in_keys(ke, tl) value ke, tl; {
	intlet dummy;
	if (Type(tl) == ELT) return No;
	if (Type(tl) != Tab) syserr(MESS(122, "in_keys applied to non-table"));
	return found(key_elem, tl, ke, &dummy);
}

Visible Procedure delete(tl, ke) value *tl, ke; {
	intlet len, k; value *tp;
	if (Type(*tl) == ELT) syserr(MESS(123, "deleting table entry from empty table"));
	if (Type(*tl) != Tab) syserr(MESS(124, "deleting table entry from non-table"));
	tp= Ats(*tl); len= Length(*tl);
	if (!found(key_elem, *tl, ke, &k))
		syserr(MESS(125, "deleting non-existent table entry"));
	if (Unique(*tl)) {
		release(*(tp+=k));
		for (k= k; k < len; k++) {*tp= *(tp+1); tp++;}
		xtndlt(tl, -1);
	} else {
		intlet kk; value *tq= Ats(*tl);
		release(*tl);
		*tl= grab_tab(--len);
		tp= Ats(*tl);
		for (kk= 0; kk < len; kk++) {
			*tp++= copy (*tq++);
			if (kk == k) tq++;
		}
	}
}

#endif INTEGRATION
