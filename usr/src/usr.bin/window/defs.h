/*
 *	@(#)defs.h	3.4 83/09/01	
 */

#include "ww.h"
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#define NWINDOW 9

int nread;
int nreade;
int nreadz;
int nreadc;
struct timeval starttime;

	/* things for handling input */
char ibuf[512];
char *ibufp;
int ibufc;
#define bgetc()		(ibufc ? ibufc--, *ibufp++&0x7f : -1)
#define bpeekc()	(ibufc ? *ibufp&0x7f : -1)
#define bungetc(c)	(ibufp > ibuf ? ibufc++, *--ibufp = (c) : -1)

struct ww *window[NWINDOW];	/* the windows */
struct ww *selwin;		/* the selected window */
struct ww *cmdwin;		/* the command window */
struct ww *framewin;		/* the window for framing */
struct ww *boxwin;		/* the window for the box */

char *shell;			/* the shell program */
char *shellname;		/* the shell program name (for argv[0]) */

int nbufline;			/* number of lines in the buffer */

	/* flags */
char quit;
char terse;
char debug;
char incmd;			/* in command mode */
char escapec;			/* escape character */

struct ww *getwin();
struct ww *openwin();
struct ww *idtowin();
struct ww *openiwin();

	/* stuff for long commands */
int argc;
char *argv[100];
int lineno;			/* line number in the source file */
char insource;			/* we're in a source */
