/*
 * $Id: uwait.h,v 5.2 90/06/23 22:20:31 jsp Rel $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)uwait.h	5.1 (Berkeley) 7/19/90
 */

#if defined(mc68k) || defined(mc68000) || defined(mc68020) || defined(sparc) || defined(hp9000s300)
#define BITS_BIGENDIAN
#endif
#if defined(vax) || defined(i386)
#define BITS_LITTLENDIAN
#endif
#if !defined BITS_BIGENDIAN && !defined BITS_LITTLENDIAN
 #error Do not know my byte ordering
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
#ifdef BITS_LITTLENDIAN
		unsigned short	w_Termsig:7;	/* termination signal */
		unsigned short	w_Coredump:1;	/* core dump indicator */
		unsigned short	w_Retcode:8;	/* exit code if w_termsig==0 */
#endif
#ifdef BITS_BIGENDIAN
		unsigned short	w_Fill1:16;	/* high 16 bits unused */
		unsigned short	w_Retcode:8;	/* exit code if w_termsig==0 */
		unsigned short	w_Coredump:1;	/* core dump indicator */
		unsigned short	w_Termsig:7;	/* termination signal */
#endif
	} w_U;
};
#define	w_termsig	w_U.w_Termsig
#define w_coredump	w_U.w_Coredump
#define w_retcode	w_U.w_Retcode

#define WIFSIGNALED(x)	((x).w_termsig != 0)
#define WIFEXITED(x)	((x).w_termsig == 0)
