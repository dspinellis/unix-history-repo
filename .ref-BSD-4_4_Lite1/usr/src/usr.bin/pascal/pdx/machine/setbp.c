/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)setbp.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

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
#include "process/pxinfo.h"

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
	unsigned char w;
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
