/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)assert.h	4.4 (Berkeley) %G%
 */

#ifndef _ASSERT_H_
#define	_ASSERT_H_

#ifdef NDEBUG
#define	assert
#define	_assert
#else
#define	assert(expression) { \
	if (!(expression)) { \
		(void)fprintf(stderr, \
		    "assertion \"%s\" failed: file \"%s\", line %d\n", \
		    "expression", __FILE__, __LINE__); \
		exit(2); \
	} \
}
#define	_assert(expression)	assert(expression)
#endif

#endif /* !_ASSERT_H_ */
