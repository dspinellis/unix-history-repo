/*	@(#)defs.h	3.13 84/04/05		*/

#include "ww.h"
#include <sys/time.h>

#define NWINDOW 9

struct timeval starttime;
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
char incmd;			/* in command mode */
