/* wait.h - header file for remote wait call */

/*  Copyright 1992 Free Software Foundation, Inc.

    This code was donated by Wind River Systems, Inc. */

/*
modification history
--------------------
01a,05jun90,llk  borrowed.
*/

/* Define how to access the structure that the wait system call stores.
   On many systems, there is a structure defined for this.
   But on vanilla-ish USG systems there is not.  */

#ifndef HAVE_WAIT_STRUCT
#define WAITTYPE int
#define WIFSTOPPED(w) (((w)&0377) == 0177)
#define WIFSIGNALED(w) (((w)&0377) != 0177 && ((w)&~0377) == 0)
#define WIFEXITED(w) (((w)&0377) == 0)
#define WRETCODE(w) ((w) >> 8)
#define WSTOPSIG(w) ((w) >> 8)
#define WCOREDUMP(w) (((w)&0200) != 0)
#define WTERMSIG(w) ((w) & 0177)
#define WSETEXIT(w, status) ((w) = (status))
#define WSETSTOP(w,sig)  ((w) = (0177 | ((sig) << 8)))
#else
#if FALSE
#ifndef ORIG

/* don't include sys/wait.h */

#else ORIG
#include <sys/wait.h>
#endif ORIG
#endif FALSE
#define WAITTYPE union wait
#define WRETCODE(w) (w).w_retcode
#define WSTOPSIG(w) (w).w_stopsig
#define WCOREDUMP(w) (w).w_coredump
#define WTERMSIG(w) (w).w_termsig
#define WSETEXIT(w, status) ((w).w_status = (status))
#define WSETSTOP(w,sig)  \
  ((w).w_stopsig = (sig), (w).w_coredump = 0, (w).w_termsig = 0177)
#endif
