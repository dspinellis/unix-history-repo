#include "sno.h"
/*
 * sno4
 */


and(ptr)
struct node *ptr;
{
	register struct node *a, *p;

	p = ptr;
	a = p->p1;
	if (p->typ == 0) {
		switch (a->typ) {
		case0:
		case 0:
			a->typ = 1;
		case 1:
			goto l1;
		case 3:
			flush();
			return(syspit());
		case 5:
			a = a->p2->p1;
			goto l1;
		case 6:
			return(binstr(nfree()));
		}
		writes("attempt to take an illegal value");
		goto case0;
	l1:
		a = copy(a->p2);
	}
	return(a);
}

eval(e, t)
struct node *e;
{
	struct node *list, *a2, *a3, *a4, *a3base;
	register struct node *a1, *stack, *op;

	if (rfail == 1)
		return(0);
	stack = 0;
	list = e;
	goto l1;
advanc:
	list = list->p1;
l1:
	op = list->typ;
	switch (op) {
		default:
		case 0:
		if (t == 1) {
			a1 = and(stack);
			goto e1;
		}
		if (stack->typ == 1)
			writes("attempt to store in a value");
		a1 = stack->p1;
	e1:
		stack = pop(stack);
		if (stack)
			writes("phase error");
		return(a1);
	case 12:
		a1 = and(stack);
		stack->p1 = look(a1);
		delete(a1);
		stack->typ = 0;
		goto advanc;
	case 13:
		if (stack->typ)
			writes("illegal function");
		a1 = stack->p1;
		if (a1->typ!=5)
			writes("illegal function");
		a1 = a1->p2;
		op = a1->p1;
		a3base = a3 = alloc();
		a3->p2 = op->p2;
		op->p2 = 0;
		a1 = a1->p2;
		a2 = list->p2;
	f1:
		if (a1!=0 & a2!=0)
			goto f2;
		if (a1!=a2)
			writes("parameters do not match");
		op = op->p1;
		goto f3;
	f2:
		a3->p1 = a4 = alloc();
		a3 = a4;
		a3->p2 = and(a1);
		assign(a1->p1, eval(a2->p2, 1));/* recursive */
		a1 = a1->p2;
		a2 = a2->p1;
		goto f1;
	f3:
		op = execute(op); /* recursive */
		if (op)
			goto f3;
		a1 = stack->p1->p2;
		op = a1->p1;
		a3 = a3base;
		stack->p1 = op->p2;
		stack->typ = 1;
		op->p2 = a3->p2;
	f4:
		a4 = a3->p1;
		free(a3);
		a3 = a4;
		a1 = a1->p2;
		if (a1 == 0)
			goto advanc;
		assign(a1->p1, a3->p2);
		goto f4;
	case 11:
	case 10:
	case 9:
	case 8:
	case 7:
		a1 = and(stack);
		stack = pop(stack);
		a2 = and(stack);
		a3 = doop(op, a2, a1);
		delete(a1);
		delete(a2);
		stack->p1 = a3;
		stack->typ = 1;
		goto advanc;
	case 15:
		a1 = copy(list->p2);
		a2 = 1;
		goto l3;
	case 14:
		a1 = list->p2;
		a2 = 0;
	l3:
		stack = push(stack);
		stack->p1 = a1;
		stack->typ = a2;
		goto advanc;
	}
}

doop(op, arg1, arg2)
{
	register int a1, a2;

	a1 = arg1;
	a2 = arg2;
	switch (op) {

	case 11:
		return(div(a1, a2));
	case 10:
		return(mult(a1, a2));
	case 8:
		return(add(a1, a2));
	case 9:
		return(sub(a1, a2));
	case 7:
		return(cat(a1, a2));
	}
	return(0);
}

execute(e)
struct node *e;
{
	register struct node *r, *b, *c;
	struct node *m, *ca, *d, *a;

	r = e->p2;
	lc = e->ch;
	switch (e->typ) {
	case 0: /*  r g */
		a = r->p1;
		delete(eval(r->p2, 1));
		goto xsuc;
	case 1: /*  r m g */
		m = r->p1;
		a = m->p1;
		b = eval(r->p2, 1);
		c = search(m, b);
		delete(b);
		if (c == 0)
			goto xfail;
		free(c);
		goto xsuc;
	case 2: /*  r a g */
		ca = r->p1;
		a = ca->p1;
		b = eval(r->p2, 0);
		assign(b, eval(ca->p2, 1));
		goto xsuc;
	case 3: /*  r m a g */
		m = r->p1;
		ca = m->p1;
		a = ca->p1;
		b = eval(r->p2, 0);
		d = search(m, b->p2);
		if (d == 0)
			goto xfail;
		c = eval(ca->p2, 1);
		if (d->p1 == 0) {
			free(d);
			assign(b, cat(c, b->p2));
			delete(c);
			goto xsuc;
		}
		if (d->p2 == b->p2->p2) {
			assign(b, c);
			free(d);
			goto xsuc;
		}
		(r=alloc())->p1 = d->p2->p1;
		r->p2 = b->p2->p2;
		assign(b, cat(c, r));
		free(d);
		free(r);
		delete(c);
		goto xsuc;
	}
xsuc:
	if (rfail)
		goto xfail;
	b = a->p1;
	goto xboth;
xfail:
	rfail = 0;
	b = a->p2;
xboth:
	if (b == 0) {
		return(e->p1);
	}
	b = eval(b, 0);
	if (b == lookret)
		return(0);
	if (b == lookfret) {
		rfail = 1;
		return(0);
	}
	if (b->typ!=2)
		writes("attempt to transfer to non-label");
	return(b->p2);
}

assign(adr, val)
struct node *adr, *val;
{
	register struct node *a, *addr, *value;

	addr = adr;
	value = val;
	if (rfail == 1) {
		delete(value);
		return;
	}
	switch (addr->typ) {
	default:
		writes("attempt to make an illegal assignment");
	case 0:
		addr->typ = 1;
	case 1:
		delete(addr->p2);
		addr->p2 = value;
		return;
	case 4:
		sysput(value);
		return;
	case 5:
		a = addr->p2->p1;
		delete(a->p2);
		a->p2 = value;
		return;
	}
}
