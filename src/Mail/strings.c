/* Copyright (c) 1979 Regents of the University of California */
#

/*
 * Mail -- a mail program
 *
 * String allocation routines.
 * Strings handed out here are reclaimed at the top of the command
 * loop each time, so they need not be freed.
 */

#include "rcv.h"

/*
 * Allocate size more bytes of space and return the address of the
 * first byte to the caller.  An even number of bytes are always
 * allocated so that the space will always be on a word boundary.
 * The string spaces are of exponentially increasing size, to satisfy
 * the occasional user with enormous string size requests.
 */

char *
salloc(size)
{
	register char *t;
	register int s;
	register struct strings *sp;
	int index;

	s = size;
	s++;
	s &= ~01;
	index = 0;
	for (sp = &stringdope[0]; sp < &stringdope[NSPACE]; sp++) {
		if (sp->s_topFree == NOSTR && (STRINGSIZE << index) >= s)
			break;
		if (sp->s_nleft >= s)
			break;
		index++;
	}
	if (sp >= &stringdope[NSPACE])
		panic("Ran out of memory!");
	if (sp->s_topFree == NOSTR) {
		index = sp - &stringdope[0];
		sp->s_topFree = (char *) calloc(STRINGSIZE << index,
		    (unsigned) 1);
		sp->s_nextFree = sp->s_topFree;
		sp->s_nleft = STRINGSIZE << index;
	}
	sp->s_nleft -= s;
	t = sp->s_nextFree;
	sp->s_nextFree += s;
	return(t);
}

/*
 * Reset the string area to be empty.
 * Called to free all strings allocated
 * since last reset.
 */

sreset()
{
	register struct strings *sp;
	register int index;

	index = 0;
	for (sp = &stringdope[0]; sp < &stringdope[NSPACE]; sp++) {
		if (sp->s_topFree == NOSTR)
			continue;
		sp->s_nextFree = sp->s_topFree;
		sp->s_nleft = STRINGSIZE << index;
		index++;
	}
}
