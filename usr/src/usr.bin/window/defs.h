/*	@(#)defs.h	3.20 90/06/02		*/
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)defs.h	3.20 (Berkeley) %G%
 */

#include "ww.h"
#include <sys/time.h>

#define NWINDOW 9

struct timeval starttime;
struct timezone timezone;

struct ww *selwin;
struct ww *cmdwin;

char *default_shell[128];	/* default shell argv */
char *default_shellfile;	/* default shell program */
int default_nline;		/* default buffer size for new windows */
int default_smooth;		/* default "smooth" parameter */
char escapec;			/* the escape character */

	/* flags */
char quit;			/* quit command issued */
char terse;			/* terse mode */
char debug;			/* debug mode */
char incmd;			/* in command mode */
