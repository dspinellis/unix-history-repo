/* defs.h - include or define things that aren't present on all systems
 * vixie@decwrl 26dec92 [new]
 *
 * $Id: portability.h,v 1.1 1993/06/01 00:45:28 vixie Exp vixie $
 */

/*
 * ++Copyright++
 * -
 * Copyright (c)  Regents of the University of California.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 * 	This product includes software developed by the University of
 * 	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * -
 * Portions Copyright (c) 1993 by Digital Equipment Corporation.
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies, and that
 * the name of Digital Equipment Corporation not be used in advertising or
 * publicity pertaining to distribution of the document or software without
 * specific, written prior permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
 * CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * -
 * --Copyright--
 */

#include <string.h>
#include <sys/types.h>
#include <sys/param.h>

#ifdef __sgi
#define BSD 43
#define _POSIX_SOURCE
#define vfork fork
#define USE_SETSID
#endif

#if defined(host_mips) && defined(SYSTYPE_BSD43)
#define RISCOS_BSD
#endif

/* primative but it will do for now */
#if !defined(SYSV)
#define USE_WAIT3
#endif

#if defined(SYSV) || defined(ultrix) || (defined(BSD) && BSD >= 199306)
#define USE_UTIME
#endif

#if defined(BSD) && BSD >= 199006 && !defined(i386) && !defined(RISCOS_BSD)
#define HAVE_DAEMON
#endif

#if defined(_POSIX_SOURCE)

#include <stdlib.h>
#include <unistd.h>
#include <limits.h>

#else /*POSIX*/

#define STDERR_FILENO 2
extern char *getenv();
extern int errno;

#ifndef DMALLOC
extern char *malloc(), *realloc(), *calloc();
extern void free();
#endif

#endif /*POSIX*/

#ifndef UINT_MAX
#ifdef __STDC__
#define UINT_MAX        4294967295u             /* max value of an "u_int" */
#else
#define UINT_MAX        ((unsigned)4294967295)  /* max value of an "u_int" */
#endif
#define ULONG_MAX       UINT_MAX        /* max decimal value of a "u_long" */
#endif

#ifndef INT_MAX
#define INT_MAX		2147483647	/* max decimal value of an "int" */
#endif

#ifndef	IN_LOOPBACKNET
#define	IN_LOOPBACKNET	127
#endif

#ifndef	INADDR_NONE
#define	INADDR_NONE	0xffffffff
#endif

#if !defined(__STDC__) && !defined(const)
#define const /*constant*/
#endif

#if (!defined(BSD) || (BSD <= 43)) && !defined(NeXT)
extern void syslog();
extern char *ctime();
extern int	close(), setitimer(), recv(), sendto(), sigsetmask(),
		atoi(), getpid(), fork(), read(), ioctl(),
		setsockopt(), socket(), bind();
#endif

#if !defined(bcopy)	/* some machines have their own macros for this */
#if !defined(BSD)
#define bcopy(a,b,c) memcpy(b,a,c)
#define bzero(a,b) memset(a,0,b)
#define bcmp(a,b,c) memcmp(a,b,c)
#else
extern int strcasecmp();
extern int bcmp();
extern void bcopy(), bzero();
#endif /* BSD */
#endif /* bcopy */

#if (!defined(BSD) || (BSD < 43) || defined(RISCOS_BSD)) && !defined(_POSIX_SOURCE)
#define NEED_STRERROR
#endif

#if (!defined(BSD) || (BSD < 43)) && !defined(ultrix)
#define NEED_STRCASECMP
#define NEED_STRTOUL
#endif

/*
 * Attempt to configure for type of function returned by signal-catching
 * functions (which signal and sigvec.sv_handler take a pointer to).
 * This can guess for BSD; otherwise, define SIG_FN externally.
 */
#ifndef	SIG_FN
#ifdef	BSD
#if (BSD >= 199006) || defined(NeXT) || defined(__osf__)
#define SIG_FN	void		/* signal-catching functions return void */
#else
#define SIG_FN	int		/* signal-catching functions return int */
#endif
#else	/* BSD */
#define SIG_FN	void		/* signal-catching functions return void */
#endif	/* BSD */
#endif

#if !defined(ntohl) && !defined(htonl) && defined(BSD) && (BSD <= 43)
/* if these aren't null macros in netinet/in.h, extern them here. */
extern u_short htons(), ntohs();
extern u_long htonl(), ntohl();
#endif

#if defined(_POSIX_SOURCE) && !defined(sun) && !defined(__sgi)
#define	PORT_NONBLOCK	O_NONBLOCK
#define	PORT_WOULDBLK	EAGAIN
#else
#define	PORT_NONBLOCK	O_NDELAY
#define	PORT_WOULDBLK	EWOULDBLOCK
#endif

#if defined(_POSIX_SOURCE)
#define	USE_SETSID
#endif

#if defined(__osf__) || (defined(BSD) && (BSD > 43) && !defined(RISCOS_BSD))
typedef	int		WAIT_T;
#else
typedef	union wait	WAIT_T;
#endif

#if (!defined(WEXITSTATUS)) || (!defined(WTERMSIG))
#define WEXITSTATUS(x) (x).w_retcode
#define WTERMSIG(x) (x).w_termsig
#endif
