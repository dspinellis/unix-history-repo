/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ed James.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)include.h	5.3 (Berkeley) 4/30/90
 */

/*
 * Copyright (c) 1987 by Ed James, UC Berkeley.  All rights reserved.
 *
 * Copy permission is hereby granted provided that this notice is
 * retained on all partial or complete copies.
 *
 * For more info on this and all of my stuff, mail edjames@berkeley.edu.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <pwd.h>

#ifdef BSD
#include <sgtty.h>
#include <sys/time.h>
#include <sys/file.h>
#endif

#ifdef SYSV
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <sys/utsname.h>
#endif

#include <signal.h>
#include <math.h>

#include <curses.h>

#ifdef SYSV
#define	index	strchr
#define	rindex	strrchr
#define bcopy(a,b,c)	memcpy((b), (a), (c))
#define	bzero(a,b)	memset((a), '\0', (b))
#define	srandom	srand
#define	random	rand
#define	sgttyb	termio
#define	sg_erase c_cc[2]
#define	sg_kill c_cc[3]
#endif

#include "def.h"
#include "struct.h"
#include "extern.h"
#include "tunable.h"
