/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1val.c,v 1.4 85/08/22 16:53:49 timo Exp $
*/

/* General operations for objects */

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b1mem.h"
#ifndef INTEGRATION
#include "b1btr.h"
#include "b1val.h"
#endif
#include "b1tlt.h"
#include "b2nod.h" /* for _Nbranches */
#include "b3scr.h" /* TEMPORARY for at_nwl */
#include "b1num.h" /* for ccopy, rrelease, grab, grab_num, grab_rat, grab_approx */
#ifdef INTEGRATION
#include "node.h"
#endif INTEGRATION

#ifdef vax
/* 4.2 BSD malloc already takes care of using a small number of sizes */
#define Len len
#else
#define Len (len < 200 ? len : ((len-1)/8+1)*8)
#endif

#define Hdrsize (sizeof(struct value)-sizeof(string))
#define Tsize (sizeof(a_telita))
#define Adj(s) (unsigned) (Hdrsize+(s))
#define Unadj(s) (unsigned) ((s)-Hdrsize)
#define NodOffset (sizeof(int) + 2*sizeof(intlet))

#define Grabber() {if(len>Maxintlet)syserr(MESS(1800, "big grabber"));}
#define Regrabber() {if(len>Maxintlet)syserr(MESS(1801, "big regrabber"));}

/*************************** Grabbing ***********************************/

#ifdef NOT_USED
long gr= 0;

Visible Procedure prgr() {at_nwl=No;printf(" gr:%ld",gr);gr=0;}
#endif

Hidden unsigned
getsyze(type, len, pnptrs)
	literal type; intlet len; int *pnptrs;
{
	register unsigned syze= 0;
	register int nptrs= 0;
	switch (type) {
	case Num:
		if (len >= 0) syze= Len*sizeof(digit);	   /* Integral */
		else if (len == -1) {
#ifdef EXT_RANGE
			syze= 2*sizeof(double);		   /* Approximate */
#else
			syze= sizeof(double);		   /* Approximate */
#endif
		}
		else { syze= 2*sizeof(value); nptrs= 2; }  /* Rational */
		break;
	case Ptn: len= _Nbranches(len);
		syze= (len+2)*sizeof(value); nptrs= len; break;
	case Com: syze= len*sizeof(value); nptrs= len; break;

	case Sim: syze= sizeof(simploc); nptrs= 1; break;
	case Tri: syze= sizeof(trimloc); nptrs= 3; break;
	case Tse: syze= sizeof(tbseloc); nptrs= 2; break;
	case How: syze= sizeof(how); nptrs= 1; break;
	case For: syze= sizeof(formal); nptrs= 1; /*uname!*/ break;
	case Per: syze= sizeof(per); nptrs= 1; break;
	case Fun:
	case Prd: syze= sizeof(funprd); nptrs= 1; break;
	case Ref: syze= sizeof(ref); nptrs= 1; break;
#ifndef INTEGRATION
	case Tex:
	case ELT:
	case Lis:
	case Tab: syze= sizeof(value); nptrs= 1; break;
#else
	case Tex: syze= (len+1)*sizeof(char); break;
	case ELT:
	case Lis:
	case Tab: syze = Len*sizeof(value); nptrs= len; break;
	case Pat: syze= sizeof(struct path) - Hdrsize; nptrs= 2; break;
	case Nod: syze= sizeof(struct node) - Hdrsize - sizeof(node)
			+ len*sizeof(node);
		  nptrs= len; break;
#endif
	default:
		printf("\ngetsyze{%c}\n", type);
		syserr(MESS(1803, "getsyze called with unknown type"));
	}
	if (pnptrs != NULL) *pnptrs= nptrs;
	return syze;
}

Hidden value
grab(type, len)
	literal type; intlet len;
{
	unsigned syze= getsyze(type, len, (int*)NULL);
	value v;
	Grabber();
	v= (value) getmem(Adj(syze));
	v->type= type; v->len= len; v->refcnt= 1;
#ifdef NOT_USED
 gr+=1;
#endif
	return v;
}

#ifndef INTEGRATION

Visible value grab_tlt(type, it) literal type, it; { return grab(type, it); }

#else

Visible value grab_tex(len) intlet len; { return grab(Tex, len); }

Visible value grab_elt() { return grab(ELT, 0); }

Visible value grab_lis(len) intlet len; { return grab(Lis, len); }

Visible value grab_tab(len) intlet len; { return grab(Tab, len); }

#endif

Visible value
grab_num(len)
	register int len;
{
	integer v;
	register int i;

	if (len > Maxintlet) {
		error(MESS(1804, "exceptionally large number"));
		return Vnil;
	}
	if (len < -Maxintlet) len = -2;
	v = (integer) grab(Num, len);
	for (i = Length(v)-1; i >= 0; --i) Digit(v, i) = 0;
	return (value) v;
}

Visible value grab_rat() { return grab(Num, -2); }

Visible value
regrab_num(v, len)
	value v; register int len;
{
	register unsigned syze;

	syze = Len * sizeof(digit);
	uniql(&v);
	regetmem((ptr*)&v, Adj(syze));
	Length(v) = len;
	return v;
}

Visible value grab_com(len) intlet len; { return grab(Com, len); }

Visible value grab_ptn(len) intlet len; { return grab(Ptn, len); }

Visible value grab_sim() { return grab(Sim, 0); }

Visible value grab_tri() { return grab(Tri, 0); }

Visible value grab_tse() { return grab(Tse, 0); }

Visible value grab_how() { return grab(How, 0); }

Visible value grab_for() { return grab(For, 0); }

Visible value grab_per() { return grab(Per, 0); }

Visible value grab_fun() { return grab(Fun, 0); }

Visible value grab_prd() { return grab(Prd, 0); }

Visible value grab_ref() { return grab(Ref, 0); }

#ifdef INTEGRATION

/*
 * Allocate a node with nch children.
 */

Visible node
grab_node(nch)
	register int nch;
{
	register node n = (node) grab(Nod, nch);
	register int i;

	n->n_marks = 0;
	n->n_width = 0;
	n->n_symbol = 0;
	for (i = nch-1; i >= 0; --i)
		n->n_child[i] = Nnil;
	return n;
}

/*
 * Allocate a path.
 */

Visible path
grab_path()
{
	register path p = (path) grab(Pat, 0);

	p->p_parent = PATHnil;
	p->p_tree = Nnil;
	p->p_ichild = 0;
	p->p_ycoord = 0;
	p->p_xcoord = 0;
	p->p_level = 0;
	p->p_addmarks = 0;
	p->p_delmarks = 0;
	return p;
}

#endif INTEGRATION


/******************************* Copying and releasing *********************/

Visible value
copy(v)
	value v;
{
	if (IsSmallInt(v)) return v;
	if (v != Vnil && v->refcnt < Maxrefcnt) (v->refcnt)++;
#ifdef NOT_USED
 gr+=1;
#endif
	return v;
}

Visible Procedure
release(v)
	value v;
{
#ifdef IBMPC
	literal *r;
#else
	intlet *r;
#endif
	if (IsSmallInt(v)) return;
	if (v == Vnil) return;
	r= &(v->refcnt);
	if (*r == 0) syserr(MESS(1805, "releasing unreferenced value"));
	if (bugs) {
		printf("releasing: ");
		if (Type(v) == Num) bugs= No;
		wri(v,No,No,No); newline();
		bugs= Yes;
	}
	if (*r < Maxrefcnt && --(*r) == 0) rrelease(v);
#ifdef NOT_USED
 gr-=1;
#endif
}

Hidden value
ccopy(v)
	value v;
{
	literal type= v->type; intlet len; value w;
	int nptrs; unsigned syze; register string from, to, end;
	register value p, *pp, *pend;
	len= Length(v);
	syze= getsyze(type, len, &nptrs);
	Grabber();
	w= (value) getmem(Adj(syze));
	w->type= type; w->len= len; w->refcnt= 1;
	from= Str(v); to= Str(w); end= to+syze;
	while (to < end) *to++ = *from++;
	pp= Ats(w);
#ifdef INTEGRATION
	if (type == Nod) pp= (value*) ((char*)pp + NodOffset);
#endif
	pend= pp+nptrs;
	while (pp < pend) {
		p= *pp++;
		if (p != Vnil && !IsSmallInt(p) && Refcnt(p) < Maxrefcnt)
			++Refcnt(p);
	}
	return w;
}

Visible Procedure
uniql(ll)
	value *ll;
{
	if (*ll != Vnil && !IsSmallInt(*ll) && (*ll)->refcnt > 1) {
		value c= ccopy(*ll);
		release(*ll);
		*ll= c;
	}
}

Hidden Procedure
rrelease(v)
	value v;
{
	literal type= v->type; intlet len;
	int nptrs; register value *pp, *pend;
	len= Length(v);
#ifndef INTEGRATION
	switch (type) {
	case Tex:
	case Tab:
	case Lis:
	case ELT:
		relbtree(Root(v), Itemtype(v));
		break;
	default:
#endif
		VOID getsyze(type, len, &nptrs);
		pp= Ats(v);
#ifdef INTEGRATION
		if (type == Nod) pp= (value*) ((char*)pp + NodOffset);
#endif
		pend= pp+nptrs;
		while (pp < pend) release(*pp++);
#ifndef INTEGRATION
	}
#endif
	v->type= '\0'; freemem((ptr) v);
}

#ifdef INTEGRATION

Visible Procedure
xtndtex(a, d)
	value *a; intlet d;
{
	intlet len= Length(*a)+d;
	Regrabber();
	regetmem((ptr *) a, Adj((len+1)*sizeof(char)));
	(*a)->len= len;
}

Visible Procedure
xtndlt(a, d)
	value *a; intlet d;
{
	intlet len= Length(*a); intlet l1= Len, l2;
	len+= d; l2= Len;
	if (l1 != l2) {
		Regrabber();
		regetmem((ptr *) a, Adj(l2*sizeof(value)));
	}
	(*a)->len= len;
}

/*
 * Set an object's refcnt to infinity, so it will never be released.
 */

Visible Procedure
fix_refcnt(v)
	register value v;
{
	register int i;
	register node n;
	register path p;

	Assert(v->refcnt > 0);
	v->refcnt = Maxrefcnt;
	switch (v->type) {
	case Tex:
		break;
	case Nod:
		n = (node)v;
		for (i = v->len - 1; i >= 0; --i)
			if (n->n_child[i])
				fix_refcnt((value)(n->n_child[i]));
		break;
	case Pat:
		p = (path)v;
		if (p->p_parent)
			fix_refcnt((value)(p->p_parent));
		if (p->p_tree)
			fix_refcnt((value)(p->p_tree));
		break;
	default:
		Abort();
	}
}

#endif INTEGRATION

#ifndef INTEGRATION

/*********************************************************************/
/* grab, copy, release of btree(node)s
/*********************************************************************/

Visible btreeptr
grabbtreenode(flag, it)
	literal flag; literal it;
{
	btreeptr pnode; unsigned syz;
	static intlet isize[]= {
		sizeof(itexnode), sizeof(ilisnode),
		sizeof(itabnode), sizeof(itabnode)};
	static intlet bsize[]= {
		sizeof(btexnode), sizeof(blisnode),
		sizeof(btabnode), sizeof(btabnode)};
	switch (flag) {
	case Inner:
		syz= isize[it];
		break;
	case Bottom:
		syz= bsize[it];
		break;
	case Irange:
	case Crange:
		syz = sizeof(rangenode);
		break;
	}
	pnode = (btreeptr) getmem((unsigned) syz);
	Refcnt(pnode) = 1;
	Flag(pnode) = flag;
	return(pnode);
}

/* ----------------------------------------------------------------- */

Visible btreeptr copybtree(pnode) btreeptr pnode; {
	if (pnode != Bnil && Refcnt(pnode) < Maxrefcnt) ++Refcnt(pnode);
	return(pnode);
}

Visible Procedure uniqlbtreenode(pptr, it) btreeptr *pptr; literal it; {
	if (*pptr NE Bnil && Refcnt(*pptr) > 1) {
		btreeptr qnode = *pptr;
		*pptr = ccopybtreenode(*pptr, it);
		relbtree(qnode, it);
	}
}

Visible btreeptr ccopybtreenode(pnode, it) btreeptr pnode; literal it; {
	intlet limp;
	btreeptr qnode;
	intlet iw;
	
	iw = Itemwidth(it);
	qnode = grabbtreenode(Flag(pnode), it);
	Lim(qnode) = limp = Lim(pnode);
	Size(qnode) = Size(pnode);
	switch (Flag(qnode)) {
	case Inner:
		cpynitms(Piitm(qnode, 0, iw), Piitm(pnode, 0, iw), limp, it);
		cpynptrs(&Ptr(qnode, 0), &Ptr(pnode, 0), limp+1);
		break;
	 case Bottom:
		cpynitms(Pbitm(qnode, 0, iw), Pbitm(pnode, 0, iw), limp, it);
		break;
	case Irange:
	case Crange:
		Lwbval(qnode) = copy(Lwbval(pnode));
		Upbval(qnode) = copy(Upbval(pnode));
		break;
	default:
		syserr(MESS(1808, "unknown flag in ccopybtreenode"));
	}
	return(qnode);
}

/* make a new root (after the old ptr0 split) */

Visible btreeptr mknewroot(ptr0, pitm0, ptr1, it)
	btreeptr ptr0, ptr1; itemptr pitm0; literal it;
{
	int r;
	intlet iw = Itemwidth(it);
	btreeptr qnode = grabbtreenode(Inner, it);
	Ptr(qnode, 0) = ptr0;
	movnitms(Piitm(qnode, 0, iw), pitm0, 1, iw);
	Ptr(qnode, 1) = ptr1;
	Lim(qnode) = 1;
	r= Sincr(Size(ptr0));
	Size(qnode) = Ssum(r, Size(ptr1));
	return(qnode);
}

/* ----------------------------------------------------------------- */

/* release btree */

Visible Procedure relbtree(pnode, it) btreeptr pnode; literal it; {
	width iw;
	
	iw = Itemwidth(it);
	if (pnode EQ Bnil)
		return;
	if (Refcnt(pnode) EQ 0) {
		syserr(MESS(1809, "releasing unreferenced btreenode"));
		return;
	}
	if (Refcnt(pnode) < Maxrefcnt && --Refcnt(pnode) EQ 0) {
		intlet l;
		switch (Flag(pnode)) {
		case Inner:
			for (l = 0; l < Lim(pnode); l++) {
				relbtree(Ptr(pnode, l), it);
				switch (it) {
				case Tt:
				case Kt:
					release(Ascval(Piitm(pnode, l, iw)));
				case Lt:
					release(Keyval(Piitm(pnode, l, iw)));
				}
			}
			relbtree(Ptr(pnode, l), it);
			break;
		case Bottom:
			for (l = 0; l < Lim(pnode); l++) {
				switch (it) {
				case Tt:
				case Kt:
					release(Ascval(Pbitm(pnode, l, iw)));
				case Lt:
					release(Keyval(Pbitm(pnode, l, iw)));
				}
			}
			break;
		case Irange:
		case Crange:
			release(Lwbval(pnode));
			release(Upbval(pnode));
			break;
		default:
			syserr(MESS(1810, "wrong flag in relbtree()"));
		}
		freemem((ptr) pnode);
	}
}

#endif !INTEGRATION
