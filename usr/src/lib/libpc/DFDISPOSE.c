/*-
 * Copyright (c) 1982 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)DFDISPOSE.c	4.4 (Berkeley) 4/9/90";
#endif /* not lint */

/*
 * Close all active files within a dynamic record,
 * then dispose of the record.
 */

#include "h00vars.h"
#include "libpc.h"

DFDISPOSE(var, size)
	char	**var;	/* pointer to pointer being deallocated */
	long	size;	/* sizeof(bletch) */
{
	register struct iorec	*next, *prev;
	struct iorec *start, *end;

	start = (struct iorec *)(*var);
	end = (struct iorec *)(*var + size);
	prev = (struct iorec *)(&_fchain);
	next = _fchain.fchain;
	while(next != FILNIL && (next->flev < GLVL || next < start)) {
		prev = next;
		next = next->fchain;
	}
	while(next != FILNIL && start <= next && next < end)
		next = PFCLOSE(next, TRUE);
	prev->fchain = next;
	DISPOSE(var, size);
}
