/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sys.h	5.1 (Berkeley) %G%
 */

/*
 * sys.h: Put all the stupid compiler and system dependencies here...
 */
#ifndef _h_sys
#define _h_sys

#ifndef public
# define public		/* Externally visible functions/variables */
#endif

#ifndef private
# define private	static	/* Always hidden internals */
#endif

#ifndef protected
# define protected	/* Redefined from elsewhere to "static" */
			/* When we want to hide everything	*/
#endif

#include <sys/cdefs.h>

#ifndef _PTR_T
# define _PTR_T
# if __STDC__
typedef void* ptr_t;
# else
typedef char* ptr_t;
# endif
#endif

#ifndef _IOCTL_T
# define _IOCTL_T
# if __STDC__
typedef void* ioctl_t;
# else
typedef char* ioctl_t;
# endif
#endif

#include <stdio.h>

#endif /* _h_sys */
