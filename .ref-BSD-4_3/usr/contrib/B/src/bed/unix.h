/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: unix.h,v 2.4 85/08/22 16:09:55 timo Exp $ */

/*
 * B editor -- inventory of available UNIX features.
 */

#ifdef BSD
#define VFORK	/* 4.x BSD vfork() system call available */
#endif

#ifdef BSD4_2
#define SELECT	/* 4.2 BSD select() system call available */
#endif

#define SIGNAL	/* can #include <signal.h> (v7 or any BSD compatible) */
#define SETJMP	/* can #include <setjmp.h> */
#define SGTTY_H	/* can #include <sgtty.h> (at least v7 compatible) */

#define PERROR	/* can use perror(), sys_errlist and sys_nerr */

/* #define PWB */	/* Turn on for PWB/UNIX systems without getenv etc. */
