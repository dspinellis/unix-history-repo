/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)alloc.c	7.1 (Berkeley) %G%
 *  
 *
 * Copyright (c) 1989, 1990, 1991 Carnegie Mellon University
 * All Rights Reserved.
 *
 * Author: Alessandro Forin
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */

/*
 *	Dynamic memory allocator
 */
struct fl {
	struct fl	*next;
	unsigned	size;
} *freelist = (struct fl *)0;

extern char end[];
static char *top = end;

void *
alloc(size)
	unsigned size;
{
	register struct fl *f = freelist, **prev;

	prev = &freelist;
	while (f && f->size < size) {
		prev = &f->next;
		f = f->next;
	}
	if (f == (struct fl *)0) {
		f = (struct fl *)top;
		top += (size + 3) & ~3;
	} else
		*prev = f->next;
	return ((void *)f);
}

void
free(ptr, size)
	void *ptr;
	unsigned size;
{
	register struct fl *f = (struct fl *)ptr;

	f->size = (size + 3) & ~3;
	f->next = freelist;
	freelist = f;
}
