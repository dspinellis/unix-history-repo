/*  Author: Mark Moraes <moraes@csri.toronto.edu> */

/*LINTLIBRARY*/

#include "defs.h"
#include "globals.h"

RCSID("$Id: emalloc.c,v 1.8 1993/05/23 03:38:27 moraes Exp $")

/*
 *  malloc which dies if it can't allocate enough storage.
 */
univptr_t
emalloc(nbytes)
size_t nbytes;
{
	univptr_t cp = malloc(nbytes);

	if (cp == 0) {
		(void) fputs("No more memory for emalloc\n", stderr);
#ifdef DEBUG
		(void) fflush(stderr);
		(void) fflush(_malloc_statsfile);
		abort();
#else
		exit(EXIT_FAILURE);
#endif
	}

	return(cp);
}

/*
 *  realloc which dies if it can't allocate enough storage.
 */
univptr_t
erealloc(ptr, nbytes)
univptr_t ptr;
size_t nbytes;
{
	univptr_t cp = realloc(ptr, nbytes);

	if (cp == 0) {
		(void) fputs("No more memory for erealloc\n", stderr);
#ifdef DEBUG
		(void) fflush(stderr);
		(void) fflush(_malloc_statsfile);
		abort();
#else
		exit(EXIT_FAILURE);
#endif
	}

	return(cp);
}

/*
 *  calloc which dies if it can't allocate enough storage.
 */
univptr_t
ecalloc(nelem, sz)
size_t nelem, sz;
{
	size_t nbytes = nelem * sz;
	univptr_t cp = emalloc(nbytes);

	(void) memset((univptr_t) cp, 0, (memsize_t) nbytes);
	return(cp);
}
