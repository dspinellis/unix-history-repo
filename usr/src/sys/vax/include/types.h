/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.1 (Berkeley) %G%
 */

#ifndef	_X3J11_H_
#define	_X3J11_H_

/*
 * Types which are fundamental to the implementation and may appear in
 * more than one standard header are defined here.  Standard headers
 * then use:
 *	#ifdef	_SIZE_T_
 *	typedef	_SIZE_T_ size_t;
 *	#undef	_SIZE_T_
 *	#endif
 *
 * Thanks, ANSI!
 */
#define	_PTRDIFF_T_	int			/* ptr1 - ptr2 */
#define	_VA_LIST_	char *			/* va_list */
#define	_WCHAR_T_	unsigned short		/* wchar_t */
#define	_SIZE_T_	unsigned int		/* sizeof() */

#endif	/* _X3J11_H_ */
