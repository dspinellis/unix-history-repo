/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ansi.h	7.3 (Berkeley) %G%
 */

#ifndef	_ANSI_H_
#define	_ANSI_H_

/*
 * Types which are fundamental to the implementation and may appear in
 * more than one standard header are defined here.  Standard headers
 * then use:
 *	#ifdef	_BSD_SIZE_T_
 *	typedef	_BSD_SIZE_T_ size_t;
 *	#undef	_BSD_SIZE_T_
 *	#endif
 *
 * Thanks, ANSI!
 */
#define	_BSD_CLOCK_T_	unsigned long		/* clock() */
#define	_BSD_PTRDIFF_T_	int			/* ptr1 - ptr2 */
#define	_BSD_SIZE_T_	unsigned int		/* sizeof() */
#define	_BSD_SSIZE_T_	int			/* byte count or error */
#define	_BSD_TIME_T_	long			/* time() */
#define	_BSD_VA_LIST_	char *			/* va_list */
#define	_BSD_WCHAR_T_	unsigned short		/* wchar_t */

#endif	/* _ANSI_H_ */
