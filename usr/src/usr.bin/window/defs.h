/*	@(#)defs.h	3.5 83/11/02		*/

#include "ww.h"
#include <signal.h>
#ifndef O_4_1A
#include <sys/time.h>
#include <sys/resource.h>
#endif

#define NWINDOW 9

int nread;
int nreade;
int nreadz;
int nreadc;
#ifndef O_4_1A
struct timeval starttime;
#endif
struct timezone timezone;

char ibuf[512];
char *ibufp;
int ibufc;
#define bgetc()		(ibufc ? ibufc--, *ibufp++&0x7f : -1)
#define bpeekc()	(ibufc ? *ibufp&0x7f : -1)
#define bungetc(c)	(ibufp > ibuf ? ibufc++, *--ibufp = (c) : -1)

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
