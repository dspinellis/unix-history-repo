#ifndef lint
#ifndef NOID
#ifndef STDIO_H
#define STDIO_H
static char	stdiohid[] = "@(#)stdio.h	4.2";
#endif /* !defined STDIO_H */
#endif /* !defined NOID */
#endif /* !defined lint */

#ifdef STDIO_RECURSING
#include "/usr/include/stdio.h"
#else /* !defined STDIO_RECURSING */
#define STDIO_RECURSING
#include <stdio.h>
#undef STDIO_RECURSING
#endif /* !defined STDIO_RECURSING */

#ifndef FILENAME_MAX

#ifndef MAXPATHLEN
#ifdef unix
#include <sys/param.h>
#endif /* defined unix */
#endif /* !defined MAXPATHLEN */

#ifdef MAXPATHLEN
#define FILENAME_MAX	MAXPATHLEN
#else /* !defined MAXPATHLEN */
#define FILENAME_MAX	1024		/* Pure guesswork */
#endif /* !defined MAXPATHLEN */

#endif /* !defined FILENAME_MAX */

#ifdef __STDC__
#ifdef __GNUC__
#ifndef remove
#define remove(name)	unlink(name)
#endif /* !defined remove */
#endif /* defined __GNUC__ */
#endif /* defined __STDC__ */

#ifndef __STDC__
#ifdef unix

#include <sys/param.h>
#ifndef BSD
extern void	perror();
#endif /* !defined BSD */

#define remove(name)	unlink(name)

#endif /* defined unix */
#endif /* !defined __STDC__ */

/*
** UNIX is a registered trademark of AT&T.
*/
