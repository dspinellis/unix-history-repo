#include "stdio.h"

#define MINSBRK		2048

struct element {
	unsigned size;
	struct	element *link;
};

struct	element avail;
struct	element *rover = &avail;
extern	char *calloc(), *malloc(), *realloc(), *sbrk();
static	struct element *findpred();

char *
calloc(n, s)
	unsigned n, s;
{
	register unsigned cnt = n * s;
	register int *ip, *jp;

	ip = (int *)malloc(cnt);
	if (ip && cnt) {
		cnt /= sizeof (int);
		jp = ip;
		do
			*jp++ = 0;
		while (--cnt);
	}
	return ((char *)ip);
}

char *
malloc(size)
	unsigned size;
{
	register struct element	*p, *q, *x;
	register unsigned n, incr;

	n = (size + sizeof (int) - 1) / sizeof (int) + 1;
	if (n == 1)
		n = 2;
	q = findpred(n);
	p = q->link;
	if (p == 0) {
		incr = (n < MINSBRK/sizeof (int) ? MINSBRK/sizeof(int) : n);
		p = (struct element *) sbrk(incr * sizeof (int));
		if ((int *)q + q->size == (int *)p) {
			q->size += incr;
			q = findpred(n);
			p = q->link;
		} else {
			p->link = 0;
			p->size = incr;
			q->link = p;
		}
	}
	if (p->size == n || p->size == n+1)
		q->link = p->link;
	else {
		x = (struct element *)(((int *) p) + n);
		x->size = p->size - n;
		p->size = n;
		q->link = x;
		x->link = p->link;
	}
	rover = q->link;
	if (rover == 0)
		rover = &avail;
	return ((char *)(&p->link));
}

static struct element *
findpred(n)
	unsigned n;
{
	register struct element *p, *q, *q0;

	for (q = rover; p = (q->link); q = p)
		if (p->size >= n)
			return (q);
	q0 = q;
	if (rover != &avail) {
		q = &avail;
		for (;;) {
			p = q->link;
			if (p->size >= n)
				return (q);
			if (p == rover)
				break;
			q = p;
		}
	}
	return (q0);
}

free(px)
	char *px;
{
	register struct element	*p0, *p, *q;

	p0 = (struct element *)((int *)px - 1);
	q = p0 > rover ? rover : &avail;
	while ((p = q->link) && p <= p0)
		q = p;
	if ((int *)p0 < (int *)q + q->size)
		return;
	rover = q->link = p0;
	if ((int *)p && (int *)p == (int *)p0 + p0->size) {
		p0->size += p->size;
		p0->link = p->link;
	} else
		p0->link = p;
	if ((int *)p0 == (int *)q + q->size) {
		q->size += p0->size;
		q->link = p0->link;
		rover = q;
	}
}

char *
realloc(px, size)
	char *px;
	register unsigned size;
{
	register struct element	*p0, *p, *next;
	struct element *q;
	unsigned n, p0size;

	p0 = (struct element *)((int *)px - 1);
	p0size = p0->size;
	free(px);
	n = (size + sizeof (int) - 1) / sizeof (int) + 1;
	if (n == 1)
		n = 2;
	q = findpred(n);
	p = q->link;
	if (p == 0) {
		q = 0;
		p = (struct element *)((int *)malloc(size) - 1);
	}
	next = p->link;
	if (p != p0)
		bcopy((char *)&p->link, (char *)&p0->link, p0size);
	if (q == 0)
		goto ret;
	if (p->size > n + 1) {
		q->link = (struct element *)((int *)p + n);
		q->link->link = next;
		q->link->size = p->size - n;
		p->size = n;
	} else
		q->link = next;
ret:
	return ((char *)(&p->link));
}

bcopy(to, from, size)
	register char *to, *from;
	register unsigned size;
{

	if (size == 0)
		return;
	do
		*to++ = *from++;
	while (--size);
}
