/* strings.h - define standard string functions */

#ifndef	_STRINGS		/* once-only... */
#define	_STRINGS

#ifdef	SYS5
#define	index	strchr
#define	rindex	strrchr
#endif	SYS5

char   *index ();
char   *mktemp ();
char   *rindex ();
#ifndef	SYS5
char   *sprintf ();
#else	SYS5
int     sprintf ();
#endif	SYS5
char   *strcat ();
int     strcmp ();
char   *strcpy ();
int	strlen ();
char   *strncat ();
int     strncmp ();
char   *strncpy ();

char   *getenv ();
char   *calloc (), *malloc (), *realloc ();

#ifdef	SYS5
#include <memory.h>
#define	bcopy(b1,b2,length)	(void) memcpy (b2, b1, length)
#define	bcpy(b1,b2,length)	memcmp (b1, b2, length)
#define	bzero(b,length)		(void) memset (b, 0, length)
#endif	SYS5

#endif	not _STRINGS
