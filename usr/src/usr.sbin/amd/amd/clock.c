/*
 * $Id: clock.c,v 5.2 90/06/23 22:19:21 jsp Rel $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)clock.c	5.1 (Berkeley) 6/29/90
 */

/*
 * Callouts.
 *
 * Modelled on kernel object of the same name.
 * See usual references.
 *
 * Use of a heap-based mechanism was rejected:
 * 1.  more complext implementation needed.
 * 2.  not obvious that a list is too slow for amd.
 */

#include "am.h"

typedef struct callout callout;
struct callout {
	callout	*c_next;		/* List of callouts */
	void	(*c_fn)();		/* Function to call */
	voidp	c_closure;		/* Closure to pass to call */
	time_t	c_time;			/* Time of call */
	int	c_id;			/* Unique identifier */
};

static callout callouts;		/* List of pending callouts */
static callout *free_callouts;		/* Cache of free callouts */
static int nfree_callouts;		/* Number on free list */
static int callout_id;			/* Next free callout identifier */
time_t next_softclock;			/* Time of next call to softclock() */

/*
 * Number of callout slots we keep on the free list
 */
#define	CALLOUT_FREE_SLOP	10

/*
 * Assumption: valid id's are non-zero.
 */
#define	CID_ALLOC()	(++callout_id)
#define	CID_UNDEF	(0)

static callout *alloc_callout()
{
	callout *cp = free_callouts;
	if (cp) {
		--nfree_callouts;
		free_callouts = free_callouts->c_next;
		return cp;
	}
	return ALLOC(callout);
}

static void free_callout(cp)
callout *cp;
{
	if (nfree_callouts > CALLOUT_FREE_SLOP) {
		free((voidp) cp);
	} else {
		cp->c_next = free_callouts;
		free_callouts = cp;
		nfree_callouts++;
	}
}

/*
 * Schedule a callout.
 *
 * (*fn)(closure) will be called at clocktime() + secs
 */
int timeout(secs, fn, closure)
unsigned int secs;
void (*fn)();
voidp closure;
{
	callout *cp, *cp2;
	time_t t = clocktime() + secs;

	/*
	 * Allocate and fill in a new callout structure
	 */
	callout *cpnew = alloc_callout();
	cpnew->c_closure = closure;
	cpnew->c_fn = fn;
	cpnew->c_time = t;
	cpnew->c_id = CID_ALLOC();

	if (t < next_softclock)
		next_softclock = t;

	/*
	 * Find the correct place in the list
	 */
	for (cp = &callouts; cp2 = cp->c_next; cp = cp2)
		if (cp2->c_time >= t)
			break;

	/*
	 * And link it in
	 */
	cp->c_next = cpnew;
	cpnew->c_next = cp2;

	/*
	 * Return callout identifier
	 */
	return cpnew->c_id;
}

/*
 * De-schedule a callout
 */
void untimeout(id)
int id;
{
	callout *cp, *cp2;
	for (cp = &callouts; cp2 = cp->c_next; cp = cp2) {
		if (cp2->c_id == id) {
			cp->c_next = cp2->c_next;
			free_callout(cp2);
			break;
		}
	}
}

/*
 * Clock handler
 */
int softclock()
{
	time_t now;
	callout *cp;

	do {
		if (task_notify_todo)
			task_notify();

		now = clocktime();

		/*
		 * While there are more callouts waiting...
		 */
		while ((cp = callouts.c_next) && cp->c_time <= now) {
			/*
			 * Extract first from list, save fn & closure and
			 * unlink callout from list and free.
			 * Finally call function.
			 *
			 * The free is done first because
			 * it is quite common that the
			 * function will call timeout()
			 * and try to allocate a callout
			 */
			void (*fn)() = cp->c_fn;
			voidp closure = cp->c_closure;

			callouts.c_next = cp->c_next;
			free_callout(cp);
#ifdef DEBUG
			/*dlog("Calling %#x(%#x)", fn, closure);*/
#endif /* DEBUG */
			(*fn)(closure);
		}

	} while (task_notify_todo);

	/*
	 * Return number of seconds to next event,
	 * or 0 if there is no event.
	 */
	if (cp = callouts.c_next)
		return cp->c_time - now;
	return 0;
}
