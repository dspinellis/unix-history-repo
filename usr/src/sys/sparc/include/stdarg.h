/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stdarg.h	7.5 (Berkeley) %G%
 *
 * from: $Header: stdarg.h,v 1.8 93/05/07 18:10:14 torek Exp $
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

#if __GNUC__ == 1
#define __extension__	/* hack for bootstrapping via gcc 1.x */
#endif

/*
 * va_arg picks up the next argument of type `ty'.  Appending an
 * asterisk to ty must produce a pointer to ty (i.e., ty may not be,
 * e.g., `int (*)()').  In addition, ty must not be any type which
 * undergoes promotion to some other type (e.g., char): it must
 * be the promoted type instead.
 *
 * Gcc-2.x tries to use ldd/std for double and quad_t values, but Sun's
 * brain-damaged calling convention does not quad-align these.  Thus,
 * for 8-byte arguments, we have to pick up the actual value four bytes
 * at a time, and use type punning (i.e., a union) to produce the result.
 * (We could also do this with a libc function, actually, by returning
 * 8 byte integers in %o0+%o1 and the same 8 bytes as a double in %f0+%f1.)
 *
 * Note: we cannot use the union trick (which generates better code) for
 * C++, since `ty' might be a type with a constructor (these may not appear
 * in a union).
 *
 * The extraneous casts through `void *' avoid gcc alignment warnings.
 */
#ifdef __cplusplus
#define	__va_8byte(ap, ty) ({ \
	int __va_i[2]; \
	__va_i[0] = ((int *)(void *)(ap))[0]; \
	__va_i[1] = ((int *)(void *)(ap))[1]; \
	(ap) += 8; *(ty *)(void *)__va_i; })
#else
#define	__va_8byte(ap, ty) ({ \
	union { ty __d; int __i[2]; } __va_u; \
	__va_u.__i[0] = ((int *)(void *)(ap))[0]; \
	__va_u.__i[1] = ((int *)(void *)(ap))[1]; \
	(ap) += 8; __va_u.__d; })
#endif /* __cplusplus */

#define va_arg(ap, ty) __extension__ ({ \
    ty __va_temp; /* to check for invisible-ptr struct-valued args */ \
    __builtin_classify_type(__va_temp) >= 12 ? \
	((ty **)(void *)((ap) += sizeof(ty *)))[-1][0] : \
    sizeof(ty) == 8 ? __va_8byte(ap, ty) : \
	((ty *)(void *)(ap += sizeof(ty)))[-1]; })

#endif /* _MACHINE_STDARG_H */
