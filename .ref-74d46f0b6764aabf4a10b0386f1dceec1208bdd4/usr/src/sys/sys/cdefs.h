/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cdefs.h	7.5 (Berkeley) %G%
 */

#ifndef	_CDEFS_H_
#define	_CDEFS_H_

#if defined(__cplusplus)
#define	__BEGIN_DECLS	extern "C" {
#define	__END_DECLS	};
#else
#define	__BEGIN_DECLS
#define	__END_DECLS
#endif

/*
 * The __CONCAT macro is a bit tricky -- make sure you don't put
 * spaces in between its arguments, also you can't use strings in
 * code that's intended for historical compilers.
 */
#if defined(__STDC__) || defined(__cplusplus)
#define	__P(protos)	protos		/* full-blown ANSI C */
#define	__CONCAT(x,y)	x ## y
#define	__STRING(x)	#x
#else					/* traditional style */
#ifdef __GNUC__
#define	const		__const		/* GCC: ANSI C with -traditional */
#define	inline		__inline
#define	signed		__signed
#define	volatile	__volatile
#else
#define	const				/* delete ANSI C keywords */
#define	inline
#define	signed
#define	volatile
#endif
#define	__P(protos)	()		/* traditional C preprocessor */
#define	__CONCAT(x,y)	x/**/y
#define	__STRING(x)	"x"
#endif

#endif /* !_CDEFS_H_ */
