/*	@(#)defs.h	3.8 84/01/16		*/

#include "ww.h"
#include <signal.h>
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

int nbufline;			/* number of lines in the buffer */

	/* flags */
char quit;
char terse;
char debug;
char incmd;			/* in command mode */
char escapec;			/* escape character */
