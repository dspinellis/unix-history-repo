/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stdarg.h	7.1 (Berkeley) %G%
 *
 * from: $Header: stdarg.h,v 1.5 92/06/17 06:10:29 torek Exp $
 */

/*
 * SPARC stdarg.h
 */

#ifndef _MACHINE_STDARG_H
#define _MACHINE_STDARG_H

typedef char *va_list;

/*
 * va_start sets ap to point to the first variable argument.
 * The `last fixed argument' parameter l is ignored (and should
 * never have been included in the ANSI standard!).
 *
 * va_end cleans up after va_start.  There is nothing to do there.
 */
#define va_start(ap, l)	(__builtin_saveregs(), \
			 ap = (char *)__builtin_next_arg())
#define va_end(ap)	/* empty */

/*
 * va_arg picks up the next argument of type `t'.  Appending an
 * asterisk to t must produce a pointer to t (i.e., t may not be,
 * e.g., `int (*)()').  In addition, t must not be any type which
 * undergoes promotion to some other type (e.g., char): it must
 * be the promoted type instead.
 */
#define va_arg(ap, t)	(((t *)(ap += sizeof(t)))[-1])

#endif /* _MACHINE_STDARG_H */
