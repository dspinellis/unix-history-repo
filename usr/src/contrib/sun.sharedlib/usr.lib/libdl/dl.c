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

/* @(#)dl.c 1.1 89/09/04 SMI */

/*
 * Copyright (c) 1989, 1991 Sun Microsystems, Inc.
 */

/*
 * Stub library for programmer's interface to the dynamic linker.  Used
 * to satisfy ld processing, and serves as a precedence place-holder at
 * execution-time.  If these routines are actually called, it is considered
 * a program-terminating error -- the dynamic linker has gotten very confused.
 */

#include <stdio.h>

static	char error[] = "%s: stub interception failed\n";

/*ARGSUSED*/
void *
dlopen(file, mode)
	char *file;			/* file to be added */
	int mode;			/* mode of addition */
{

	fprintf(stderr, error, "dlopen");
	abort();
	/*NOTREACHED*/
}

/*ARGSUSED*/
void *
dlsym(dl, cp)
	void *dl;			/* object to be searched */
	char *cp;			/* symbol to be retrieved */
{

	fprintf(stderr, error, "dlsym");
	abort();
	/*NOTREACHED*/
}

/*ARGSUSED*/
int
dlclose(dl)
	void *dl;			/* object to be removed */
{

	fprintf(stderr, error, "dlclose");
	abort();
	/*NOTREACHED*/
}

char *
dlerror()
{

	fprintf(stderr, error, "dlerror");
	abort();
	/*NOTREACHED*/
}
