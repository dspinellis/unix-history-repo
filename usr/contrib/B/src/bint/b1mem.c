/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1mem.c,v 1.4 85/08/27 10:55:51 timo Exp $
*/

/* B memory management */

#include "b.h"
#include "b1obj.h"
#include "b1mem.h"
#include "b3err.h" /* For still_ok */

#ifdef IBMPC
#define RESERVE
#endif

#ifdef RESERVE
Forward char *mymalloc(), *myrealloc();
#else
#define mymalloc malloc
#define myrealloc realloc
#define initreserve() 0 /* Dummy routine */
#endif

#ifdef ebug

Hidden value notel, inotel;
Visible bool noting= Yes;

Hidden Procedure note(p) int p; {
	if (!noting) {
		value ip;
		noting= Yes;
		insert(ip= mk_integer(p), &notel);
		release(ip);
		noting= No;
	}
}

Hidden Procedure denote(p) int p; {
	if (!noting) {
		value ip;
		noting= Yes;
		if (in(ip= mk_integer(p), notel)) {
			remove(ip, &notel);
			if (inotel != Vnil && in(ip, inotel))
				remove(ip, &inotel);
		}
#ifndef IBMPC
		else syserr(MESS(600, "releasing illegally"));
#endif
		release(ip);
		noting= No;
	}
}

Visible Procedure initmem() {
	notel= mk_elt(); noting= No;
	initreserve();
}

Visible Procedure end_init() {
	noting= Yes;
	inotel= copy(notel);
	noting= No;
}

Visible Procedure term_mem() {
	int l, i; value v;
	testing= noting= Yes;
	l= length(notel);
	if (l>length(inotel)) fprintf(stdout, "Unreleased:\n");
	for(i=1; i<=l; i++) {
		v= thof(i, notel);
		if (!in(v, inotel))
			{ wri((value) intval(v), No, No, No); newline(); }
	}
}

Visible Procedure endmem() {
	release(inotel); inotel= Vnil;
	release(notel); notel= Vnil;
}

#else !ebug

#define note(p)
#define denote(p)

Visible Procedure initmem()
{
	initreserve();
}

Visible Procedure end_init() {}
Visible Procedure term_mem() {}
Visible Procedure endmem() {}

#endif ebug

#define Negative MESS(601, "creating value of exceedingly large size")

Visible ptr getmem(syze) unsigned syze; {
	ptr p;
	if ((int) (syze) < 0) error(Negative);
	p= (ptr) mymalloc(syze);
	note(p);
	if (bugs || p==Nil) printf("{%u}",syze);
	if (p == Nil) memexh();
	return p;
}

Visible Procedure regetmem(v, syze) ptr *v; unsigned syze; {
	if ((int) (syze) < 0) error(Negative);
	if (bugs) printf("[%u]",syze);
	denote(*v);
	*v= myrealloc(*v, syze);
	note(*v);
	if (*v == Nil) memexh();
}

Visible Procedure freemem(p) ptr p; {
	denote(p);
	free(p);
}

#ifdef RESERVE

#define RES_SIZE 3000

Hidden char *p_reserve= Nil;

Hidden Procedure initreserve() {
	p_reserve= malloc(RES_SIZE);
}

Hidden char *mymalloc(syze) unsigned syze; {
	char *p;
	p= malloc(syze);
	if (p != Nil && p_reserve != Nil) return p;
	if (p_reserve != Nil) {
		free(p_reserve); p_reserve= Nil;
		p= malloc(syze);
		if (p != Nil) error(MESS(602, "out of memory"));
	}
	if (p_reserve == Nil) initreserve();
	return p;
}

Hidden char *myrealloc(p1, syze) char *p1; unsigned syze; {
	char *p;
	p= realloc(p1, syze);
	if (p != Nil && p_reserve != Nil) return p;
	if (p_reserve != Nil) {
		free(p_reserve); p_reserve= Nil;
		p= realloc(p1, syze);
		if (p != Nil) error(MESS(602, "out of memory"));
	}
	if (p_reserve == Nil) initreserve();
	return p;
}

#endif RESERVE
