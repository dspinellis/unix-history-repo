/* strings.h - define standard string functions */
/* @(#)$Id: strings.h,v 1.19 1993/02/26 21:54:00 jromine Exp $ */

#ifndef	_STRINGS		/* once-only... */
#define	_STRINGS

#ifdef AUX
#include <stdlib.h>
#endif

#ifdef _AIX
#include <string.h>
#include <stdlib.h>

#define bcmp(b1,b2,length)	memcmp(b1, b2, length)
#define	bcopy(b1,b2,length)	(void) memcpy (b2, b1, length)
#define	bcpy(b1,b2,length)	memcmp (b1, b2, length)
#define	bzero(b,length)		(void) memset (b, 0, length)

#else /* _AIX */

#ifdef	SYS5
#define	index	strchr
#define	rindex	strrchr
#endif /* SYS5 */

#if	defined(BSD42) || defined(SVR4)
#if !defined(SVR4) && !defined(__386BSD__) && !defined(BSD44)
#include <strings.h>
#else
#include <string.h>
#include <stdlib.h>
#endif
#else	/* not BSD42 || SVR4 */
char   *index ();
char   *mktemp ();
char   *rindex ();
#ifndef	SPRINTFTYPE
#ifndef	ncr		/* NCR compiler complains about re-declaration */
char   *sprintf ();		/* I guess this is the new standard */
#endif
#else
SPRINTFTYPE sprintf ();
#endif
char   *strcat ();
int     strcmp ();
char   *strcpy ();
int	strlen ();
char   *strncat ();
int     strncmp ();
char   *strncpy ();
#endif

#if !defined(SVR4) && !defined(__386BSD__) && !defined(BSD44)
char   *getenv ();
char   *calloc (), *malloc (), *realloc ();
#endif	/* SVR4 */

#if defined(__HIGHC__) || __GNUC__ == 2
#define	bcopy(s,d,l)	memcpy(d,s,l)
#endif

#ifdef	SYS5
#include <memory.h>
#define bcmp(b1,b2,length)	memcmp(b1, b2, length)
#define	bcopy(b1,b2,length)	(void) memcpy (b2, b1, length)
#define	bcpy(b1,b2,length)	memcmp (b1, b2, length)
#define	bzero(b,length)		(void) memset (b, 0, length)
#endif /* SYS5 */
#endif	/* _AIX */
#endif /* not _STRINGS */
