/*	@(#)defs.h	3.17 88/02/21		*/
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)defs.h	3.17 (Berkeley) %G%
 */

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
