/* System-dependent stuff, for ``normal'' systems */

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#if defined (sparc) && defined (sun)
#include <alloca.h>
#endif
#ifndef alloca				/* May be a macro, with args. */
extern char *alloca ();
#endif
#endif

#include <sys/types.h>			/* Needed by dirent.h */

#if defined (USG) && defined (TIOCGWINSZ)
#include <sys/stream.h>
#if defined (USGr4) || defined (USGr3)
#include <sys/ptem.h>
#endif /* USGr4 */
#endif /* USG && TIOCGWINSZ */

#include <dirent.h>
typedef struct dirent dirent;

/* SVR4 systems should use <termios.h> rather than <termio.h>. */

#if defined (USGr4)
#define _POSIX_VERSION
#endif
