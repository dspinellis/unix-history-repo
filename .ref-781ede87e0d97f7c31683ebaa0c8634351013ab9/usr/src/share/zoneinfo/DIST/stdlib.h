#ifndef lint
#ifndef NOID
#ifndef STDLIB_H
#define STDLIB_H
static char	stdlibhid[] = "@(#)stdlib.h	4.5";
#endif /* !defined STDLIB_H */
#endif /* !defined NOID */
#endif /* !defined lint */

#ifdef __STDC__
#ifndef __GNUC__
#define LOOK_FOR_STDLIB
#endif /* !defined __GNUC__ */
#endif /* defined __STDC__ */

#ifdef LOOK_FOR_STDLIB
#undef LOOK_FOR_STDLIB

#ifdef STDLIB_RECURSING
#include "/usr/include/stdlib.h"
#else /* !defined STDLIB_RECURSING */
#define STDLIB_RECURSING
#include <stdlib.h>
#undef STDLIB_RECURSING
#endif /* !defined STDLIB_RECURSING */

#ifndef NULL
/*
** Stupid Turbo C doesn't define NULL in stdlib.h
*/
#include <stdio.h>
#endif /* !defined NULL */

#else /* !defined LOOK_FOR_STDLIB */

/*
** size_t
*/

#include "sys/types.h"

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS	0
#endif /* !defined EXIT_SUCCESS */

#ifndef EXIT_FAILURE
#define EXIT_FAILURE	1
#endif /* !defined EXIT_FAILURE */

#ifndef NULL
#include <stdio.h>
#endif /* !defined NULL */

/*
** String conversion functions
*/

#include <math.h>

/*
** Memory management functions
*/

extern char *	calloc();
extern char *	malloc();
extern char *	realloc();

#ifdef USG
extern void	free();
#endif /* defined USG */

/*
** Communication with the environment
*/

extern char *	getenv();

#ifdef USG
extern void	exit();
#endif /* defined USG */

/*
** Searching and sorting functions
*/

#ifdef USG
extern void	qsort();
#endif /* defined USG */

#endif /* !defined LOOK_FOR_STDLIB */
