/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: node.c,v 2.4 85/08/22 16:05:27 timo Exp $";

/*
 * B editor -- Parse tree and Focus stack.
 */

#include "b.h"
#include "bobj.h"

#include "node.h"

#define Register register
	/* Used for registers 4-6.  Define as empty macro on PDP */


/*
 * Lowest level routines for 'node' data type.
 */

#define Isnode(n) ((n) && (n)->type == Nod)

#define Nchildren(n) ((n)->len)
#define Symbol(n) ((n)->n_symbol)
#define Child(n, i) ((n)->n_child[(i)-1])
#define Marks(n) ((n)->n_marks)
#define Width(n) ((n)->n_width)


/*
 * Routines which are macros for the compiler but real functions for lint,
 * so it will check the argument types more strictly.
 */

#ifdef lint
node
nodecopy(n)
	node n;
{
	return (node) copy((value) n);
}

noderelease(n)
	node n;
{
	release((value)n);
}

nodeuniql(pn)
	node *pn;
{
	uniql((value*)pn);
}
#endif lint


/*
 * Allocate a new node.
 */

Visible node
newnode(nch, sym, children)
	register int nch;
	Register int sym;
	register node children[];
{
	register node n = (node) grab_node(nch); /* Must preset with zeros! */

	Symbol(n) = sym;
	for (; nch > 0; --nch)
		Child(n, nch) = children[nch-1];
	Width(n) = evalwidth(n);
	return n;
}


/*
 * Macros to change the fields of a node.
 */

#define Locchild(pn, i) \
	(Refcnt(*(pn)) == 1 || nodeuniql(pn), &Child(*(pn), i))
#define Setmarks(pn, x) \
	(Refcnt(*(pn)) == 1 || nodeuniql(pn), Marks(*(pn))=(x))
#define Setwidth(pn, w) (Refcnt(*(pn)) == 1 || nodeuniql(pn), Width(*(pn))=w)


/*
 * Change a child of a node.
 * Like replace(), it does not increase the reference count of n.
 */

Visible Procedure
setchild(pn, i, n)
	register node *pn;
	register int i;
	Register node n;
{
	register node *pch;
	register node oldchild;

	Assert(Isnode(*pn));
	pch = Locchild(pn, i);
	oldchild = *pch;
	*pch = n;
	repwidth(pn, oldchild, n);
	noderelease(oldchild);
}


/*
 * Lowest level routines for 'path' data type.
 */

#define NPATHFIELDS 6

#define Parent(p) ((p)->p_parent)
#define Tree(p) ((p)->p_tree)
#define Ichild(p) ((p)->p_ichild)
#define Ycoord(p) ((p)->p_ycoord)
#define Xcoord(p) ((p)->p_xcoord)
#define Level(p) ((p)->p_level)


/*
 * Routines which are macros for the compiler but real functions for lint,
 * so it will check the argument types more strictly.
 */

#ifdef lint
Visible path
pathcopy(p)
	path p;
{
	return (path) copy((value) p);
}

Visible Procedure
pathrelease(p)
	path p;
{
	release((value)p);
}

Visible Procedure
pathuniql(pp)
	path *pp;
{
	uniql((value*)pp);
}
#endif lint


/*
 * Allocate a new path entry.
 */

Visible path
newpath(pa, n, i)
	register path pa;
	register node n;
	Register int i;
{
	register path p = (path) grab_path();

	Parent(p) = pa;
	Tree(p) = n;
	Ichild(p) = i;
	Ycoord(p) = Xcoord(p) = Level(p) = 0;
	return p;
}


/*
 * Macros to change the fields of a path entry.
 */

#define Uniqp(pp) (Refcnt(*(pp)) == 1 || pathuniql(pp))

#define Setcoord(pp, y, x, level) (Uniqp(pp), \
	(*(pp))->p_ycoord = y, (*(pp))->p_xcoord = x, (*(pp))->p_level = level)

#define Locparent(pp) (Uniqp(pp), &Parent(*(pp)))

#define Loctree(pp) (Uniqp(pp), &Tree(*(pp)))

#define Addmarks(pp, x) (Uniqp(pp), \
	(*(pp))->p_addmarks |= (x), (*(pp))->p_delmarks &= ~(x))

#define Delmarks(pp, x) (Uniqp(pp), \
	(*(pp))->p_delmarks |= (x), (*(pp))->p_addmarks &= ~(x))


Hidden Procedure
connect(pp)
	path *pp;
{
	register path p = *pp;
	register path pa = Parent(p);
	register path *ppa;
	register node n;
	register node npa;
	register node *pn;
	node oldchild;
	node *pnpa;
	int i;
	markbits add;
	markbits del;

	if (!pa)
		return;
	i = ichild(p);
	n = Tree(p);
	if (Child(Tree(pa), i) == n)
		return; /* Still connected */

	n = nodecopy(n);
	ppa = Locparent(pp);
	pnpa = Loctree(ppa);
	pn = Locchild(pnpa, i);
	oldchild = *pn;
	*pn = n;
	repwidth(pnpa, oldchild, n);
	noderelease(oldchild);

	add = p->p_addmarks;
	del = p->p_delmarks;
	if (add|del) {
		p = *pp;
		p->p_addmarks = 0;
		p->p_delmarks = 0;
		if (add)
			Addmarks(ppa, add);
		npa = *pnpa;
		if (del) {
			for (i = Nchildren(npa); i > 0; --i)
				if (i != ichild(p))
					del &= ~marks(Child(npa, i));
			Delmarks(ppa, del);
		}
		Setmarks(pnpa, Marks(npa)&~del|add);
	}
}


/*
 * The following procedure sets the new width of node *pn when child
 * oldchild is replaced by child newchild.
 * This was added because the original call to evalwidth seemed to
 * be the major caller of noderepr() and fwidth().
 */

Hidden Procedure
repwidth(pn, old, new)
	register node *pn;
	Register node old;
	Register node new;
{
	register int w = Width(*pn);
	register int oldwidth = width(old);
	register int newwidth = width(new);

	if (w < 0) {
		if (oldwidth > 0)
			oldwidth = 0;
		if (newwidth > 0)
			newwidth = 0;
	}
	else {
		Assert(oldwidth >= 0);
		if (newwidth < 0) {
			Setwidth(pn, newwidth);
			return;
		}
	}
	newwidth -= oldwidth;
	if (newwidth)
		Setwidth(pn, w + newwidth);
}


Visible Procedure
markpath(pp, new)
	register path *pp;
	register markbits new;
{
	register node *pn;
	register markbits old;

	Assert(Type(Tree(*pp)) == Nod);
	old = Marks(Tree(*pp));
	if ((old|new) == old)
		return; /* Bits already set */

	pn = Loctree(pp);
	Setmarks(pn, old|new);
	Addmarks(pp, new&~old);
}


Visible Procedure
unmkpath(pp, del)
	register path *pp;
	register int del;
{
	register node *pn;
	register markbits old;

	Assert(Type(Tree(*pp)) == Nod);
	old = Marks(Tree(*pp));
	if ((old&~del) == del)
		return;

	pn = Loctree(pp);
	Setmarks(pn, old&~del);
	Delmarks(pp, del&old);
}


Hidden Procedure
clearmarks(pn)
	register node *pn;
{
	register int i;

	if (!Marks(*pn))
		return;
	if (Isnode(*pn)) {
		Setmarks(pn, 0);
		for (i = Nchildren(*pn); i > 0; --i)
			clearmarks(Locchild(pn, i));
	}
}


/*
 * Replace the focus' tree by a new node.
 * WARNING: n's reference count is not increased!
 * You can also think of this as: replace(pp, n) implies noderelease(n).
 * Mark bits are copied from the node being replaced.
 */

Visible Procedure
replace(pp, n)
	register path *pp;
	register node n;
{
	register node *pn;
	register markbits old;

	pn = Loctree(pp);
	if (Type(*pn) == Nod)
		old = Marks(*pn);
	else
		old = 0;
	noderelease(*pn);
	*pn = n;
	if (Type(n) == Nod) {
		clearmarks(pn);
		if (old)
			Setmarks(pn, old);
	}
	else if (old)
		Addmarks(pp, old);
}


Visible bool
up(pp)
	register path *pp;
{
	register path p = *pp;

	if (!Parent(p))
		return No;

	connect(pp);
	p = pathcopy(Parent(*pp));
	pathrelease(*pp);
	*pp = p;
	return Yes;
}


Visible bool
downi(pp, i)
	register path *pp;
	register int i;
{
	register node n;
	auto int y;
	auto int x;
	auto int level;

	n = Tree(*pp);
	if (!Isnode(n) || i < 1 || i > Nchildren(n))
		return No;

	y = Ycoord(*pp);
	x = Xcoord(*pp);
	level = Level(*pp);
	*pp = newpath(*pp, nodecopy(Child(n, i)), i);
	evalcoord(n, i, &y, &x, &level);
	Setcoord(pp, y, x, level);
	return Yes;
}


Visible bool
downrite(pp)
	register path *pp;
{
	if (!Isnode(Tree(*pp)))
		return No;
	return downi(pp, Nchildren(Tree(*pp)));
}


Visible bool
left(pp)
	register path *pp;
{
	register int i;

	i = ichild(*pp) - 1;
	if (i <= 0)
		return No;
	if (!up(pp))
		return No;
	return downi(pp, i);
}


Visible bool
rite(pp)
	register path *pp;
{
	register int i;
	register path pa = Parent(*pp);

	i = ichild(*pp) + 1;
	if (!pa || i > Nchildren(Tree(pa)))
		return No;
	if (!up(pp))
		return No;
	return downi(pp, i);
}


/*
 * Highest level: small utilities.
 *
 * WARNING: Several of the following routines may change their argument
 * even if they return No.
 * HINT: Some of these routines are not used; they are included for
 * completeness of the provided set of operators only.  If you have
 * space problems (as, e.g., on a PDP-11), you can delete the superfluous
 * ones (lint will tell you which they are).
 */

Visible Procedure
top(pp)
	register path *pp;
{
	while (up(pp))
		;
}


Visible bool
nextnode(pp)
	register path *pp;
{
	while (!rite(pp)) {
		if (!up(pp))
			return No;
	}
	return Yes;
}


Visible Procedure
firstleaf(pp)
	register path *pp;
{
	while (down(pp))
		;
}

#if NOT_USED

Visible bool
nextleaf(pp)
	register path *pp;
{
	if (!nextnode(pp))
		return No;
	firstleaf(pp);
	return Yes;
}

#endif NOT_USED

Visible bool
prevnode(pp)
	register path *pp;
{
	while (!left(pp)) {
		if (!up(pp))
			return No;
	}
	return Yes;
}

Visible Procedure
lastleaf(pp)
	register path *pp;
{
	while (downrite(pp))
			;
}

#ifdef NOT_USED

Visible bool
prevleaf(pp)
	register path *pp;
{
	if (!prevnode(pp))
		return No;
	lastleaf(pp);
	return Yes;
}


Visible bool
nextmarked(pp, x)
	register path *pp;
	register markbits x;
{
	do {
		if (!nextnode(pp))
			return No;
	} while (!marked(*pp, x));
	while (down(pp)) {
		while (!marked(*pp, x)) {
			if (!rite(pp)) {
				up(pp) || Abort();
				return Yes;
			}
		}
	}
	return Yes;
}

#endif NOT_UED

Visible bool
firstmarked(pp, x)
	register path *pp;
	register markbits x;
{
	while (!marked(*pp, x)) {
		if (!up(pp))
			return No;
	}
	while (down(pp)) {
		while (Type(tree(*pp)) == Tex || !marked(*pp, x)) {
			if (!rite(pp)) {
				up(pp) || Abort();
				return Yes;
			}
		}
	}
	return Yes;
}


Visible bool
prevmarked(pp, x)
	register path *pp;
	register markbits x;
{
	do {
		if (!prevnode(pp))
			return No;
	} while (!marked(*pp, x));
	while (downrite(pp)) {
		while (!marked(*pp, x)) {
			if (!left(pp)) {
				up(pp) || Abort();
				return Yes;
			}
		}
	}
	return Yes;
}


/*
 * Deliver the path length to the root.
 */


Visible Procedure
pathlength(p)
	register path p;
{
	register int n;

	for (n = 0; p; ++n)
		p = parent(p);
	return n;
}


/*
 * Put a C string in a trimmed location (this name should change,
 * the 'official' routine of this name has quite different parameters).
 */


Visible Procedure
putintrim(pn, head, tail, str)
	register value *pn;
	register int head;
	Register int tail;
	Register string str;
{
	register value v = *pn; 
	value w = head == 0 ? mk_text("") :
		head == Length(v) ? copy(v) : trim(v, 0, Length(v) - head);

	Assert(head >= 0 && tail >= 0 && head + tail <= Length(v));
	if (*str)
		concato(&w, str);
	if (tail > 0)
		concato(&w, Str(v)+(Length(v) - tail));
	release(v);
	*pn = w;
}


/*
 * Touch the node in focus.
 */

Visible Procedure
touchpath(pp)
	register path *pp;
{
	nodeuniql(Loctree(pp));
}
