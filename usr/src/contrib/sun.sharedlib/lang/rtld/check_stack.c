/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/* @(#)check_stack.c 1.1 69/12/31 SMI */

/*
 * Copyright (c) 1989, 1991 Sun Microsystems, Inc.
 */

/*
 * Until such time as the kernel can produce accurate core dumps of an
 * address space (rather than just of a traditional address space), ld.so
 * "knows" about the implementation of stack frames in SunOS.  In particular,
 * it "knows" that the environment strings are placed at the "top end" of
 * the stack frame on process initialization.  This program verifies that
 * this knowledge remains "true" -- and will cause a build of ld.so to fail
 * in the event it ever becomes "not true."  (Note: of course, this technique
 * itself depends upon the build environment matching the execution one --
 * not always a guarantee.)
 */

#include <sysexits.h>
#include <machine/vmparam.h>

extern	char **environ;

/*ARGSUSED*/
main(argc, argv)
	int argc;
	char *argv[];
{
	int user_stack;
	int ps = getpagesize();
	char c, *cp, **cpp;

	ps = getpagesize();
	cpp = environ;
	do {
		cp = *cpp++;
	} while (*cpp);
	do {
		cp++;
	} while (*cp);
	user_stack = (int)(cp + ps - 1) & ~(ps - 1); 
	if (user_stack != USRSTACK) {
		printf(
	    "Error: USRSTACK calculated as: 0x%x\nUSRSTACK defined as 0x%x\n",
		    user_stack, USRSTACK);
		exit(EX_DATAERR);
		/*NOTREACHED*/
	}
	exit(0);
	/*NOTREACHED*/
}
