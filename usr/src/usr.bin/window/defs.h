/*	@(#)defs.h	3.9 84/03/03		*/

#include "ww.h"
#ifndef O_4_1A
#include <sys/time.h>
#include <sys/resource.h>
#endif

#define NWINDOW 9

#ifndef O_4_1A
struct timeval starttime;
#endif
struct timezone timezone;

struct ww *selwin;
struct ww *cmdwin;

char *shell;			/* the shell program */
char *shellname;		/* the shell program name (for argv[0]) */
int nbufline;			/* default buffer size for new windows */
char escapec;			/* the escape character */

	/* flags */
char quit;			/* quit command issued */
char terse;			/* terse mode */
char debug;			/* debug mode */
