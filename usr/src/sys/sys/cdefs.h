/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cdefs.h	7.4 (Berkeley) %G%
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

#if defined(__STDC__) || defined(__cplusplus)
#define	__P(protos)	protos
#define	__CONCAT(x,y)	x ## y
#define	__STRING(x)	#x
#else
#ifdef __GNUC__
#define	const		__const
#define	inline		__inline
#define	signed		__signed
#define	volatile	__volatile
#else
#define	const
#define	inline
#define	signed
#define	volatile
#endif
#define	__P(protos)	()
#define	__CONCAT(x,y)	x/**/y
#define	__STRING(x)	"x"
#endif

#endif /* !_CDEFS_H_ */
