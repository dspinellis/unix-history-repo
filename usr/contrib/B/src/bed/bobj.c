/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: bobj.c,v 2.5 85/08/22 15:59:59 timo Exp $";

/*
 * B editor -- A shrunken version of the B interpreter's run-time system.
 */

#include "b.h"
#include "bobj.h"
#include "node.h"

#define COMPOUNDS

string malloc();
string calloc();
string realloc();
string strcpy();

extern bool dflag;

struct head {
	char type;
	intlet refcnt;
	intlet len;
};
#define Intsize (sizeof(int))
#define Hsize (sizeof(struct head))
#define Headsize (((Hsize-1)/Intsize + 1) * Intsize)

#define Field(v, i) (((value *)&(v)->cts)[i])

#ifndef NDEBUG

/* Statistics on allocation/sharing */

int nobjs;
int nrefs;

#define Increfs ++nrefs
#define Decrefs --nrefs

#else NDEBUG

#define Increfs 
#define Decrefs 

#endif NDEBUG


#define Copy(v) if ((v) && Refcnt(v) < Maxintlet) { ++Refcnt(v); Increfs; }
#define Release(v) if (!(v) || Refcnt(v) == Maxintlet) ; else RRelease(v)
#define RRelease(v) \
	if (Refcnt(v) > 1) { --Refcnt(v); Decrefs; } else release(v)


/*
 * Allocate a value with nbytes of data after the usual type, len, refcnt
 * fields.
 */

value
grabber(nbytes)
	register int nbytes;
{
	register value v = (value) malloc((unsigned) (Headsize + nbytes));

	if (!v)
		syserr("grabber: malloc");
#ifndef NDEBUG
	if (dflag)
		newval(v);
#endif
#ifndef NDEBUG
	++nobjs;
#endif
	Increfs;
	v->refcnt = 1;
	return v;
}


/*
 * Reallocate a value with nbytes of data after the usual type, len, refcnt
 * fields.
 */

value
regrabber(v, nbytes)
	register value v;
	register int nbytes;
{
	Assert(v && v->refcnt == 1);
	v = (value) realloc((char*)v, (unsigned) (Headsize + nbytes));
	if (!v)
		syserr("regrabber: realloc");
	return v;
}


/*
 * Set an object's refcnt to infinity, so it will never be released.
 */

fix(v)
	register value v;
{
	register int i;
	register node n;
	register path p;

	Assert(v->refcnt > 0);
#ifndef NDEBUG
	if (v->refcnt < Maxintlet)
		nrefs -= v->refcnt;
#endif
	v->refcnt = Maxintlet;
#if OBSOLETE
	switch (v->type) {
	case Tex:
		break;
	case Nod:
		n = (node)v;
		for (i = v->len - 1; i >= 0; --i)
			if (n->n_child[i])
				fix((value)(n->n_child[i]));
		break;
	case Pat:
		p = (path)v;
		if (p->p_parent)
			fix((value)(p->p_parent));
		if (p->p_tree)
			fix((value)(p->p_tree));
		break;
#ifdef COMPOUNDS
	case Com:
		for (i = v->len-1; i >= 0; --i)
			if (Field(v, i))
				fix(Field(v, i));
		break;
#endif COMPOUNDS
#ifdef SLOW_INTS
	case Num:
#endif SLOW_INTS
	default:
		Abort();
	}
#endif OBSOLETE
}


#ifdef COMPOUNDS
/*
 * Allocate a compound with n fields.
 */

Visible value
grab_com(n)
	int n;
{
	value v = grabber(n*sizeof(value));

	v->type = Com;
	v->len = n;
	for (--n; n >= 0; --n)
		Field(v, n) = Vnil;
	return v;
}
#endif COMPOUNDS


/*
 * Allocate a node with nch children.
 */

node
grab_node(nch)
	register int nch;
{
	register node n = (node) grabber(
			sizeof(struct node) - Headsize +
			sizeof(value) * (nch-1));
	register int i;

	n->type = Nod;
	n->len = nch;
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

path
grab_path()
{
	register path p = (path) grabber(
			sizeof(struct path) - Headsize);

	p->type = Pat;
	p->p_parent = Pnil;
	p->p_tree = Nnil;
	p->p_ichild = 0;
	p->p_ycoord = 0;
	p->p_xcoord = 0;
	p->p_level = 0;
	p->p_addmarks = 0;
	p->p_delmarks = 0;
	return p;
}


#ifdef SLOW_INTS
/*
 * Make an integer.
 */

value
mk_integer(i)
	int i;
{
	value v;
	static value tab[128];

	if (!i)
		return Vnil;
	if (!(i&~127) && tab[i])
		return tab[i];

	v = grabber(sizeof(value));
	v->type = Num;
	Field(v, 0) = (value) i;
	if (!(i&~127)) {
		tab[i] = v;
		v->refcnt = Maxintlet;
	}
	return v;
}
#endif SLOW_INTS


/*
 * Make a text object out of a C string.
 */

value
mk_text(str)
	register string str;
{
	register int len = strlen(str);
	register value v = grabber(len+1);

	v->type = Tex;
	v->len = len;
	strcpy(Str(v), str);
	return v;
}


/*
 * Concatenate a C string to a text object (at the end).
 */

concato(pv, str)
	register value *pv;
	register string str;
{
	register value v = *pv;
	register int vlen = v->len;
	register int len = strlen(str);

	Assert(v && v->refcnt > 0);
	if (!len)
		return;

	len += vlen;
	if (v->refcnt == 1)
		v = regrabber(v, len+1);
	else {
		v = grabber(len+1);
		v->type = Tex;
		strcpy(Str(v), Str(*pv));
		Release(*pv);
	}
	strcpy(Str(v) + vlen, str);
	v->len = len;
	*pv = v;
}


/*
 * Return a substring (trim) of a text object.
 */

value
trim(v, behead, curtail)
	register value v;
	register int behead;
	register int curtail;
{
	register value w;
	register int c;

	Assert(v && v->refcnt > 0);
	Assert(behead >= 0 && curtail >= 0 && behead+curtail <= v->len);
	if (behead + curtail == 0) {
		Copy(v);
		return v;
	}

	c = Str(v)[v->len - curtail];
	Str(v)[v->len - curtail] = 0; /* TEMPORARILY */
	w = mk_text(Str(v) + behead);
	Str(v)[v->len - curtail] = c;
	return w;
}


#ifdef SLOW_INTS
/*
 * Return the C value if an integer object.
 */

int
intval(v)
	register value v;
{
	if (!v)
		return 0;
	return (int) Field(v, 0);
}
#endif SLOW_INTS


/*
 * Make sure a location (pointer variable) contains a unique object.
 */

uniql(pv)
	register value *pv;
{
	register value v = *pv;
	register value w;
	register path p;
	register node n;
	register int i;

	Assert(v && v->refcnt > 0);
	if (v->refcnt == 1)
		return;

	switch (v->type) {

	case Nod:
		n = grab_node(v->len);
		for (i = v->len - 1; i >= 0; --i) {
			w = (value) (n->n_child[i] = ((node)v)->n_child[i]);
			Copy(w); /* This is ugly */
		}
		n->n_marks = ((node)v)->n_marks;
		n->n_width = ((node)v)->n_width;
		n->n_symbol = ((node)v)->n_symbol;
		w = (value)n;
		break;

	case Pat:
		p = grab_path();
		p->p_parent = ((path)v)->p_parent;
		Copy(p->p_parent);
		p->p_tree = ((path)v)->p_tree;
		Copy(p->p_tree);
		p->p_ichild = ((path)v)->p_ichild;
		p->p_ycoord = ((path)v)->p_ycoord;
		p->p_xcoord = ((path)v)->p_xcoord;
		p->p_level = ((path)v)->p_level;
		w = (value)p;
		break;

#ifdef SLOW_INTS
	case Num:
		w = mk_integer(intval(v));
		break;
#endif SLOW_INTS

#ifdef COMPOUNDS
	case Com:
		w = grab_com(v->len);
		for (i = v->len - 1; i >= 0; --i) {
			n = (node) (Field(w, i) = Field(v, i));
			Copy(n); /* This is uglier */
		}
		break;
#endif COMPOUNDS

	case Tex:
		w = mk_text(Str(v));
		break;

	default:
		Abort();

	}
	Release(v);
	*pv = w;
}


/*
 * Increase the reference count of an object, unless it is infinite.
 */

value
copy(v)
	value v;
{
	if (!v)
		return v;

	Assert(v->refcnt > 0);
	if (v->refcnt < Maxintlet) {
		++v->refcnt;
		Increfs;
	}
	return v;
}


/*
 * Decrease the reference count of an object, unless it is infinite.
 * If it reaches zero, free the storage occupied by the object.
 */

release(v)
	register value v;
{
	register int i;
	register value w;

	if (!v)
		return;
	Assert(v->refcnt > 0);
	if (v->refcnt == Maxintlet)
		return;

	Decrefs;
	--v->refcnt;
	if (v->refcnt == 0) {
		switch (v->type) {
#ifdef SLOW_INTS
		case Num:
#endif SLOW_INTS
		case Tex:
			break;
#ifdef COMPOUNDS
		case Com:
			for (i = v->len - 1; i >= 0; --i) {
				w = Field(v, i);
				Release(w);
			}
			break;
#endif COMPOUNDS
		case Nod:
			for (i = v->len - 1; i >= 0; --i) {
				w = (value)(((node)v)->n_child[i]);
				Release(w);
			}
			break;
		case Pat:
			w = (value)(((path)v)->p_parent);
			Release(w);
			w = (value)(((path)v)->p_tree);
			Release(w);
			break;
		default:
			Abort();
		}
#ifndef NDEBUG
		if (dflag)
			delval(v);
		--nobjs;
#endif NDEBUG
		free((string)v);
	}
}

objstats()
{
#ifndef NDEBUG
	fprintf(stderr, "*** Object statistics: %d objects, %d references\n",
		nobjs, nrefs);
#ifdef MSTATS
	mstats("(at end)"); /* A routine which some malloc versions have to print
	             memory statistics. Remove if your malloc hasn't. */
#endif MSTATS
#endif NDEBUG
}

#ifndef NDEBUG
valdump(v)
	value v;
{
	if (!v)
		fputs("(nil)", stderr);
	else {
		fprintf(stderr, "v=0x%x, type='%c', len=%d, refcnt=",
			v, v->type, v->len);
		if (v->refcnt == Maxintlet)
			putc('*', stderr);
		else
			fprintf(stderr, "%d", v->refcnt);
		fputs(": ", stderr);
		wrval(v);

	}
	putc('\n', stderr);
}

#define QUOTE '\''

wrval(v)
	value v;
{
	register string cp;
	register int c;

	if (!v) {
		fputs("nil", stderr);
		return;
	}

	switch (v->type) {

#ifdef SLOW_INTS
	case Num:
		fprintf(stderr, "%d", intval(v));
		break;
#endif SLOW_INTS

	case Tex:
		putc(QUOTE, stderr);
		for (cp = Str(v); c = *cp; ++cp) {
			if (' ' <= c && c < 0177) {
				putc(c, stderr);
				if (c == QUOTE)
					putc(c, stderr);
			}
			else if (0 <= c && c < ' ')
				putc('^', stderr), putc(c + '@', stderr);
			else
				fprintf(stderr, "\\%03o", c);
		}
		putc(QUOTE, stderr);
		break;

#ifdef COMPOUNDS
	case Com:
	  {
	  	int i;
		value f;
		putc('(', stderr);
		for (i = 0; i < v->len; ++i) {
			if (i)
				putc(',', stderr), putc(' ', stderr);
			f = Field(v, i);
			if (!f || f->refcnt == 1 || f->type != Com) {
				if (f && f->type == Com)
					fprintf(stderr, "0x%x=", f);
				wrval(f);
			}
			else
				fprintf(stderr, "0x%x", f);
		}
		putc(')', stderr);
		break;
	  }
#endif COMPOUNDS

	default:
		fprintf(stderr, "0x%x", v);

	}
}

static struct list {
	struct list *link;
	value val;
} head;
#endif NDEBUG

objdump()
{
#ifndef NDEBUG
	struct list *l;

	for (l = head.link; l; l = l->link)
		valdump(l->val);
#endif NDEBUG
}

objcheck()
{
#ifndef NDEBUG
	struct list *l;

	for (l = head.link; l; l = l->link)
		if (l->val->refcnt != Maxintlet)
			valdump(l->val);
#endif NDEBUG
}

#ifndef NDEBUG
newval(v)
	register value v;
{
	register struct list *l =
			(struct list *) malloc((unsigned) sizeof(struct list));

	if (!l)
		syserr("newval: malloc");
	l->link = head.link;
	l->val = v;
	head.link = l;
}

delval(v)
	register value v;
{
	register struct list *l;
	register struct list *p;

	for (p = &head, l = head.link; l; p = l, l = l->link) {
		if (l->val == v) {
			p->link = l->link;
			free((string)l);
			return;
		}
	}
	Abort();
}
#endif NDEBUG
