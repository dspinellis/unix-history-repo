/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)wait.h	7.4 (Berkeley) %G%
 */

/*
 * This file holds definitions relevent to the wait system call.
 * Some of the options here are available only through the ``wait3''
 * entry point; the old entry point with one argument has more fixed
 * semantics, never returning status of unstopped children, hanging until
 * a process terminates if any are outstanding, and never returns
 * detailed information about process resource utilization (<vtimes.h>).
 */

#ifndef BYTE_ORDER
#include <machine/endian.h>
#endif

/*
 * Structure of the information in the first word returned by both
 * wait and wait3.  If w_stopval==WSTOPPED, then the second structure
 * describes the information returned, else the first.  See WUNTRACED below.
 */
union wait	{
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
 * Option bits for the second argument of wait3.  WNOHANG causes the
 * wait to not hang if there are no stopped or terminated processes, rather
 * returning an error indication in this case (pid==0).  WUNTRACED
 * indicates that the caller should receive status about untraced children
 * which stop due to signals.  If children are stopped and a wait without
 * this option is done, it is as though they were still running... nothing
 * about them is returned.
 */
#define WNOHANG		1	/* dont hang in wait */
#define WUNTRACED	2	/* tell about stopped, untraced children */

#define WIFSTOPPED(x)	((x).w_stopval == WSTOPPED)
#define WIFSIGNALED(x)	((x).w_stopval != WSTOPPED && (x).w_termsig != 0)
#define WIFEXITED(x)	((x).w_stopval != WSTOPPED && (x).w_termsig == 0)
