/*	@(#)defs.h	3.15 84/05/23		*/

#include "ww.h"
#include <sys/time.h>

#define NWINDOW 9

struct timeval starttime;
struct timezone timezone;

struct ww *selwin;
struct ww *cmdwin;

char *shell[128];		/* the shell argv */
char *shellfile;		/* the shell program */
int nbufline;			/* default buffer size for new windows */
char escapec;			/* the escape character */

	/* flags */
char quit;			/* quit command issued */
char terse;			/* terse mode */
char debug;			/* debug mode */
char incmd;			/* in command mode */
