/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)setbp.c 1.1 %G%";

/*
 * Breakpoint/machine interface.
 */

#include "defs.h"
#include <signal.h>
#include "machine.h"
#include "process.h"
#include "main.h"
#include "pxops.h"
#include "process/process.rep"

#define BP_OP		O_BPT		/* breakpoint trap */
#define BP_ERRNO	SIGILL		/* signal received at a breakpoint */

/*
 * Setting a breakpoint at a location consists of saving
 * the half-word at the location and poking a BP_OP there.
 *
 * We save the locations and half-words on a list for use in unsetting.
 */

typedef struct savelist SAVELIST;

struct savelist {
	ADDRESS location;
	short save;
	short refcount;
	SAVELIST *link;
};

LOCAL SAVELIST *savelist;

/*
 * Set a breakpoint at the given address.  Only save the half-word there
 * if it's not already a breakpoint.
 */

setbp(addr)
ADDRESS addr;
{
	short w;
	short save;
	register SAVELIST *newsave, *s;

	if (option('b')) {
		printf("setting breakpoint at %d\n", addr);
		fflush(stdout);
	}
	for (s = savelist; s != NIL; s = s->link) {
		if (s->location == addr) {
			s->refcount++;
			return;
		}
	}
	iread(&save, addr, sizeof(save));
	newsave = alloc(1, SAVELIST);
	newsave->location = addr;
	newsave->save = save;
	newsave->refcount = 1;
	newsave->link = savelist;
	savelist = newsave;
	w = BP_OP;
	iwrite(&w, addr, sizeof(w));
}

/*
 * Unset a breakpoint; unfortunately we have to search the SAVELIST
 * to find the saved value.  The assumption is that the SAVELIST will
 * usually be quite small.
 */

unsetbp(addr)
ADDRESS addr;
{
	register SAVELIST *s, *prev;

	if (option('b')) {
		printf("unsetting breakpoint at %d\n", addr);
		fflush(stdout);
	}
	prev = NIL;
	for (s = savelist; s != NIL; s = s->link) {
		if (s->location == addr) {
			iwrite(&s->save, addr, sizeof(s->save));
			s->refcount--;
			if (s->refcount == 0) {
				if (prev == NIL) {
					savelist = s->link;
				} else {
					prev->link = s->link;
				}
				dispose(s);
			}
			return;
		}
		prev = s;
	}
	panic("unsetbp: couldn't find address %d", addr);
}

/*
 * Predicate to test if the reason the process stopped was because
 * of a breakpoint.
 */

BOOLEAN isbperr()
{
	register PROCESS *p;

	p = process;
	return(p->status==STOPPED && p->signo==BP_ERRNO);
}
