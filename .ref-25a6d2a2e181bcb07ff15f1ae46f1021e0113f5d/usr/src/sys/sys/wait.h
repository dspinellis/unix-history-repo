/*
 * Copyright (c) 1982, 1986, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)wait.h	7.5 (Berkeley) %G%
 */

/*
 * This file holds definitions relevent to the wait4 system call
 * and the alternate interfaces that use it (wait, wait3, waitpid).
 */

#ifndef BYTE_ORDER
#include <machine/endian.h>
#endif

/*
 * Tokens for special values of the "pid" parameter to wait4.
 */
#define	WAIT_ANY	(-1)		/* any process */
#define	WAIT_MYPGRP	0		/* any process in my process group */

/*
 * Structure of the information in the status word returned by wait4.
 * If w_stopval==WSTOPPED, then the second structure describes
 * the information returned, else the first.  See WUNTRACED below.
 */
union wait {
	int	w_status;		/* used in syscall */
	/*
	 * Terminated process status.
	 */
	struct {
#if BYTE_ORDER == LITTLE_ENDIAN 
		unsigned short	w_Termsig:7;	/* termination signal */
		unsigned short	w_Coredump:1;	/* core dump indicator */
		unsigned short	w_Retcode:8;	/* exit code if w_termsig==0 */
#endif
#if BYTE_ORDER == BIG_ENDIAN 
		unsigned short	w_Filler;	/* upper bits filler */
		unsigned char	w_Retcode;	/* exit code if w_termsig==0 */
		unsigned char	w_Coredump:1;	/* core dump indicator */
		unsigned char	w_Termsig:7;	/* termination signal */
#endif
	} w_T;
	/*
	 * Stopped process status.  Returned
	 * only for traced children unless requested
	 * with the WUNTRACED option bit.
	 */
	struct {
#if BYTE_ORDER == LITTLE_ENDIAN 
		unsigned short	w_Stopval:8;	/* == W_STOPPED if stopped */
		unsigned short	w_Stopsig:8;	/* signal that stopped us */
#else
		unsigned short	w_Filler;	/* upper bits filler */
		unsigned char	w_Stopsig;	/* signal that stopped us */
		unsigned char	w_Stopval;	/* == W_STOPPED if stopped */
#endif
	} w_S;
};
#define	w_termsig	w_T.w_Termsig
#define w_coredump	w_T.w_Coredump
#define w_retcode	w_T.w_Retcode
#define w_stopval	w_S.w_Stopval
#define w_stopsig	w_S.w_Stopsig


#define	WSTOPPED	0177	/* value of s.stopval if process is stopped */

/*
 * Option bits for the second argument of wait4.  WNOHANG causes the
 * wait to not hang if there are no stopped or terminated processes, rather
 * returning an error indication in this case (pid==0).  WUNTRACED
 * indicates that the caller should receive status about untraced children
 * which stop due to signals.  If children are stopped and a wait without
 * this option is done, it is as though they were still running... nothing
 * about them is returned.   By default, a blocking wait call will be
 * aborted by receipt of a signal that is caught (POSIX); the option
 * WSIGRESTART causes the call to restart instead of failing with error EINTR.
 */
#define WNOHANG		1	/* dont hang in wait */
#define WUNTRACED	2	/* tell about stopped, untraced children */
#define WSIGRESTART	4	/* restart wait if signal is received */

/*
 * Macros to test the exit status returned by wait
 * and extract the relevant values.
 */
#define WIFSTOPPED(x)	((x).w_stopval == WSTOPPED)
#define WSTOPSIG(x)	((x).w_stopsig)

#define WIFSIGNALED(x)	((x).w_stopval != WSTOPPED && (x).w_termsig != 0)
#define WTERMSIG(x)	((x).w_termsig)
#define WCOREDUMP(x)	((x).w_coredump)

#define WIFEXITED(x)	((x).w_stopval != WSTOPPED && (x).w_termsig == 0)
#define WEXITSTATUS(x)	((x).w_retcode)
