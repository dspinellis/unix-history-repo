/* "unixlib.h"	--  limited substitute for VAX C V3.x's <unixlib.h>,
 * for use with VAX C V2.x and/or GNU C when building gawk.
 */


/* declare the global environ[] array */
#ifdef VAXC
extern char noshare **environ;
#else
# ifdef __GNUC__
#  define environ $$PsectAttributes_NOSHR$$environ
# endif
extern char **environ;
#endif

/* miscellaneous Unix emulation routines available in VAXCRTL */
char *getenv(), *getcwd();

char *ecvt(), *fcvt(), *gcvt();

int getpid(), getppid();

unsigned getuid();
#ifndef _stdlib_h	/* gcc's stdlib.h has these with conflicting types */
unsigned getgid(), getegid(), geteuid();
#endif
int setgid(), setuid();		/* no-ops */
