/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.2 (Berkeley) %G%
 */

#ifndef	_MACHTYPES_H_
#define	_MACHTYPES_H_

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

#ifndef _ANSI_SOURCE
typedef	struct	_physadr { int r[1]; } *physadr;
typedef	struct	label_t	{
	int	val[6];
} label_t;
#endif

#endif	/* _MACHTYPES_H_ */
