#include "sno.h"

/*
 * sno3
 */


bextend(str, last)
struct node *str, *last;
{
	register struct node *a, *s;
	register int b;
	int c, d;

	s = str;
	if ((c = s->p1) == 0)
		goto bad;
	b = d = 0;
	a = s->p2;
	if(a == 0) {
		a = c;
		goto eb2;
	}
eb1:
	if (a == last)
		goto bad;
	a = a->p1;
eb2:
	d++;
	c = class(a->ch);
	if (c == 1) { /* rp */
		if (b == 0)
			goto bad;
		b--;
		goto eb3;
	}
	if (c == 2) { /* lp */
		b++;
		goto eb1;
	}
eb3:
	if (b == 0) {
		s->p2= a;
		return(d);
	}
	goto eb1;
bad:
	return(0);
}

ubextend(str, last)
struct node *str, *last;
{
	register struct node *a, *b, *s;

	s = str;
	a = s->p1;
	if(a == 0)
		goto bad;
	b = s->p2;
	if(b == 0)
		goto good;
	if (b == last)
		goto bad;
	a = b->p1;
good:
	s->p2 = a;
	return(1);
bad:
	return(0);
}

search(arg, r)
struct node *arg, *r;
{
	struct node *list, *back, *str,
		*etc, *next, *last, *base, *e;
	register struct node *a, *b, *var;
	int c, d;

	a = arg->p2;
	list = base = alloc();
	last = next = 0;
	goto badv1;
badvanc:
	a = a->p1;
	if (a->typ == 0) {
		list->p1 = 0;
		if (rfail == 1) {
			a = 0;
			goto fail;
		}
		list = base;
		if (r == 0)
			next = last = 0; else {
			next = r->p1;
			last = r->p2;
		}
		goto adv1;
	}
	b = alloc();
	list->p1 = b;
	list = b;
badv1:
	list->p2 = back = alloc();
	back->p1 = last;
	b = a->p2;
	c = a->typ;
	list->typ = c;
	if (c < 2) {
		back->p2 = eval(b, 1);
		goto badvanc;
	}
	last = list;
	str = alloc();
	etc = alloc();
	back->p2 = var = alloc();
	var->typ = b->typ;
	var->p1 = str;
	var->p2 = etc;
	e = b->p1;
	if (e == 0)
		etc->p1 = 0; else
		etc->p1 = eval(e, 0);
	e = b->p2;
	if (e == 0)
		etc->p2 = 0; else {
		e = eval(e, 1);
		etc->p2 = strbin(e);
		delete(e);
	}
	goto badvanc;

retard:
	a = back->p1;
	if (a == 0) {
		rfail = 1;
		goto fail;
	}
	list = a;
	back = list->p2;
	var = back->p2;
	str = var->p1;
	etc = var->p2;
	if (etc->p2)
		goto retard;
	if (var->typ == 1) {
		if (bextend(str, last) == 0)
			goto retard;
		goto adv0;
	}
	if (ubextend(str, last) == 0)
		goto retard;
adv0:
	a = str->p2;
adv01:
	if (a == last)
		next = 0; else
		next = a->p1;
advanc:
	a = list->p1;
	if (a == 0) {
		a = alloc();
		if (r == 0) {
			a->p1 = a->p2 = 0;
			goto fail;
		}
		b = r->p1;
		a->p1 = b;
		if (next == 0) {
			a->p2 = r->p2;
			goto fail;
		}
		while(1) {
			e = b->p1;
			if (e == next) {
				a->p2 = b;
				goto fail;
			}
			b = e;
		}
	}
	list = a;
adv1:
	back = list->p2;
	var = back->p2;
	d = list->typ;
	if(d < 2) {
		if (var == 0)
			goto advanc;
		if (next == 0)
			goto retard;
		a = next;
		b = var->p1;
		e = var->p2;
		while(1) {
			if (a->ch != b->ch)
				goto retard;
			if (b == e)
				goto adv01;
			if (a == last)
				goto retard;
			a = a->p1;
			b = b->p1;
		}
	}
	str = var->p1;
	etc = var->p2;
	str->p1 = next;
	str->p2 = 0;
	c = etc->p2;
	if (var->typ == 1) {
		d = bextend(str, last);
		if (d == 0)
			goto retard;
		if (c == 0)
			goto adv0;
		while(1) {
			c =- d;
			if (c == 0)
				goto adv0;
			if (c < 0)
				goto retard;
			d = bextend(str, last);
			if (d == 0)
				goto retard;
		}
	}
	if (c == 0) {
		if(d==3 & next!=0) {
			str->p2 = last;
			goto adv0;
		}
		goto advanc;
	}
	while(c--)
		if (ubextend(str, last) == 0)
			goto retard;
	goto adv0;

fail:
	list = base;
	goto f1;
fadv:
	free(back);
	b = list->p1;
	free(list);
	if (b == 0)
		return(a);
	list = b;
f1:
	back = list->p2;
	var = back->p2;
	if (list->typ < 2) {
		delete(var);
		goto fadv;
	}
	str = var->p1;
	etc = var->p2;
	if (a != 0 & etc->p1 != 0) {
		if (str->p2 == 0) {
			free(str);
			str = 0;
		}
		assign(etc->p1, copy(str));
	}
	if (str)
		free(str);
	free(etc);
	free(var);
	goto fadv;
}
