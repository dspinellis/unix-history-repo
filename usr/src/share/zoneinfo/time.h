#ifndef lint
#ifndef NOID
#ifndef TIME_H
#define TIME_H
static char	timehid[] = "@(#)time.h	4.2";
#endif /* !defined TIME_H */
#endif /* !defined NOID */
#endif /* !defined lint */

#ifdef TIME_RECURSING
#include "/usr/include/time.h"
#else /* !defined TIME_RECURSING */
#define TIME_RECURSING
#include <time.h>
#undef TIME_RECURSING
#endif /* !defined TIME_RECURSING */

#ifndef __STDC__

#ifndef time_t
#ifdef unix
#include <sys/types.h>
#else /* !defined unix */
typedef long	time_t;
#endif /* !defined unix */
#endif /* !defined time_t */

extern time_t	time();

#endif /* !defined __STDC__ */

#ifdef __GNUC__

#ifndef time_t
#ifdef unix
#include <sys/types.h>
#else /* !defined unix */
typedef long	time_t;
#endif /* !defined unix */
#endif /* !defined time_t */

extern time_t	time();

#endif /* defined __GNUC__ */

/*
** UNIX is a registered trademark of AT&T.
*/
