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
#ifdef SYSV
#include <sys/types.h>
#endif
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
