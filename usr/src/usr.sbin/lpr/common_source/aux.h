/*
 * Copyright (c) 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

/*
 * Auxillary functions to aid portability to other systems.
 * These are 4.4BSD routines that are often not found on other systems.
 *
 * !!!USE THIS FILE ONLY IF YOU ARE NOT RUNNING 4.4BSD!!!
 */

#ifdef PREPOSIX
#define dirent direct
extern int errno;
#endif

#ifdef NO_RINDEX
#define index strchr
#define rindex strrchr
#endif

#ifdef BSDWAIT
#define WAITARG_T(a)    ((int *)(a))
#else
#define WAITARG_T(a)    (a)
#endif

#ifdef SETPGID
#define setpgrp(a, b)   setpgid((pid_t)(a), (pid_t)(b))
#endif

#ifndef FD_COPY
#define FD_COPY(f, t) memcpy((char *)t, (char *)f, sizeof(*(f)))
#endif

#ifdef NO_SNPRINTF
int snprintf __P((char *str, size_t n, const char *fmt, ...));
#endif
