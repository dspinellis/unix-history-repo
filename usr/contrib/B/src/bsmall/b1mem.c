/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b1mem.c,v 1.2 84/07/19 14:09:35 guido Exp $ */

/* B memory management */

#include "b.h"
#include "b1obj.h"
#include "b1mem.h"

#define Plausible(syze) {if ((int) (syze) < 0) \
	error("creating value of exceedingly large size");}

#define Note(p) /* note(p) */
#define deNote(p) /* denote(p) */

Visible ptr getmem(syze) unsigned syze; {
	ptr p;
	Plausible(syze);
	p= (ptr) malloc(syze);
	Note(p);
	if (bugs || p==Nil) printf("{%u}",syze);
	if (p == Nil) memexh();
	return p;
}

Visible Procedure regetmem(v, syze) value *v; unsigned syze; {
	Plausible(syze);
	uniql(v);
	if (bugs) printf("[%u]",syze);
	criton();
	deNote((ptr)*v);
	*v= (value) realloc((ptr) *v, syze);
	Note((ptr)*v);
	critoff();
	if ((ptr) *v == Nil) memexh();
}

Visible Procedure freemem(p) ptr p; {
	deNote(p);
	free(p);
}

value notel; bool noting=Yes;

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
		if (!in(ip= mk_integer(p), notel))
			syserr("releasing illegally");
		remove(ip, &notel);
		release(ip);
		noting= No;
	}
}

/*
 * Hack to delay (but not ignore) interrupts during realloc calls.
 */

#include <signal.h>

Hidden int (*inthandler)();
Hidden bool intrupted;

Hidden Procedure sighold(sig) int sig; {
	signal(sig, sighold);
	intrupted= Yes;
}

Hidden Procedure criton() {
	intrupted= No;
	inthandler= signal(SIGINT, sighold);
}

Hidden Procedure critoff() {
	signal(SIGINT, inthandler);
	if (intrupted && inthandler != SIG_IGN && inthandler != SIG_DFL)
		(*inthandler)(SIGINT);
	intrupted= No;
}
