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
static char sccsid[] = "@(#)trcond.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * trace condition list -- a list of conditions that are to be
 * checked before printing out the current source line or stopping.
 */

#include "defs.h"
#include "breakpoint.h"

typedef struct tr_cond_list {
	TRTYPE trtype;
	NODE *trace_condition;
	struct tr_cond_list *next_condition;
} TR_COND_LIST;

LOCAL TR_COND_LIST *cond_list;

/*
 * add a condition to be checked before giving single stepping information
 */

addcond(trtype, p)
TRTYPE trtype;
NODE *p;
{
	register TR_COND_LIST *c;

	if (p == NIL) {
		return;
	}
	c = alloc(1, TR_COND_LIST);
	c->trtype = trtype;
	c->trace_condition = p;
	c->next_condition = cond_list;
	cond_list = c;
}

/*
 * delete a condition from the list
 */

delcond(trtype, p)
TRTYPE trtype;
NODE *p;
{
	register TR_COND_LIST *c, *last;

	if (p == NIL) {
		return;
	}
	last = NIL;
	for (c = cond_list; c != NIL; c = c->next_condition) {
		if (c->trtype == trtype && c->trace_condition == p) {
			break;
		}
	}
	if (c == NIL) {
		panic("tried to delete non-existent condition");
	}
	if (last == NIL) {
		cond_list = c->next_condition;
	} else {
		last->next_condition = c->next_condition;
	}
	free(c);
}

/*
 * Determine if any trace condition on the list is true.
 * If the list is empty, return TRUE.
 */

BOOLEAN trcond()
{
	register TR_COND_LIST *c;
	BOOLEAN foundcond;

	foundcond = FALSE;
	for (c = cond_list; c != NIL; c = c->next_condition) {
		if (c->trtype == TRPRINT) {
			if (cond(c->trace_condition)) {
				return(TRUE);
			} else {
				foundcond = TRUE;
			}
		}
	}
	return !foundcond;
}

/*
 * Determine if any stop condition on the list is true.
 * If the list is empty, return FALSE.
 */

BOOLEAN stopcond()
{
	register TR_COND_LIST *c;

	for (c = cond_list; c != NIL; c = c->next_condition) {
		if (c->trtype == TRSTOP && cond(c->trace_condition)) {
			return(TRUE);
		}
	}
	return FALSE;
}

/*
 * Free all existing breakpoints.
 * Trace conditions have been freed elsewhere.
 */

condfree()
{
	TR_COND_LIST *c, *next;

	for (c = cond_list; c != NIL; c = next) {
		next = c->next_condition;
		dispose(c);
	}
	cond_list = NIL;
}
