/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/tc.sig.h,v 3.5 1991/11/26 04:41:23 christos Exp $ */
/*
 * tc.sig.h: Signal handling
 *
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
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
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
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
 */
#ifndef _h_tc_sig
#define _h_tc_sig

#if (SVID > 0) || defined(BSD4_4)
# include <signal.h>
# ifndef SIGCHLD
#  define SIGCHLD SIGCLD
# endif				/* SIGCHLD */
#else				/* SVID == 0 */
# include <sys/signal.h>
#endif				/* SVID > 0 */

#ifdef sun
# define SAVESIGVEC
#endif

#if SVID > 0 && SVID < 3 && !defined(BSDSIGS)
/*
 * If we have unreliable signals...
 */
# define UNRELSIG
#endif /* SVID > 0 && SVID < 3 && !BSDSIGS */

#ifdef BSDSIGS
/*
 * sigvec is not the same everywhere
 */
# if defined(_SEQUENT_) || (defined(_POSIX_SOURCE) && !defined(hpux))
#  define HAVE_SIGVEC
#  define mysigvec(a, b, c)	sigaction(a, b, c)
typedef struct sigaction sigvec_t;
#  define sv_handler sa_handler
#  define sv_flags sa_flags
# endif	/* _SEQUENT || (_POSIX_SOURCE && !hpux) */

# ifdef hpux
#  define HAVE_SIGVEC
#  define mysigvec(a, b, c)	sigvector(a, b, c)
typedef struct sigvec sigvec_t;
#  define NEEDsignal
# endif	/* hpux */

# ifndef HAVE_SIGVEC
#  define mysigvec(a, b, c)	sigvec(a, b, c)
typedef struct sigvec sigvec_t;
# endif	/* HAVE_SIGVEC */

# undef HAVE_SIGVEC
#endif /* BSDSIGS */

#if SVID > 0
# ifdef BSDJOBS
/* here I assume that systems that have bsdjobs implement the
 * the setpgrp call correctly. Otherwise defining this would
 * work, but it would kill the world, because all the setpgrp
 * code is the the part defined when BSDJOBS are defined
 * NOTE: we don't want killpg(a, b) == kill(-getpgrp(a), b)
 * cause process a might be already dead and getpgrp would fail
 */
#  define killpg(a, b) kill(-(a), (b))
# else
/* this is the poor man's version of killpg()! Just kill the
 * current process and don't worry about the rest. Someday
 * I hope I get to fix that.
 */
#  define killpg(a, b) kill((a), (b))
# endif	/* BSDJOBS */
#endif /* SVID > 0 */

#ifdef _MINIX
#include <signal.h>
#  define killpg(a, b) kill((a), (b))
#endif /* _MINIX */

#ifdef BSDSIGS
/*
 * For 4.2bsd signals.
 */
# ifdef sigmask
#  undef sigmask
# endif				/* sigmask */
# define	sigmask(s)	(1 << ((s)-1))
# ifdef _SEQUENT_
#  define 	sigpause(a)	bsd_sigpause(a)
#  define 	signal(a, b)	sigset(a, b)
# else /* _SEQUENT_ */
#  define	sighold(s)	sigblock(sigmask(s))
#  define	sigignore(s)	signal(s, SIG_IGN)
#  define 	sigset(s, a)	signal(s, a)
# endif	/* _SEQUENT_ */
# ifdef aiws
#  define 	sigrelse(a)	sigsetmask(sigblock(0) & ~sigmask(a))
#  undef	killpg
#  define 	killpg(a, b)	kill(-getpgrp(a), b)
#  define	NEEDsignal
# endif	/* aiws */
#endif /* BSDSIGS */


/*
 * We choose a define for the window signal if it exists..
 */
#ifdef SIGWINCH
# define SIG_WINDOW SIGWINCH
#else
# ifdef SIGWINDOW
#  define SIG_WINDOW SIGWINDOW
# endif	/* SIGWINDOW */
#endif /* SIGWINCH */

#if defined(convex) || defined(__convex__)
# ifdef notdef
/* Does not seem to work right... Christos */
#  define SIGSYNCH       0 
# endif
# ifdef SIGSYNCH
#  define SYNCHMASK 	(sigmask(SIGCHLD)|sigmask(SIGSYNCH))
# else
#  define SYNCHMASK 	(sigmask(SIGCHLD))
# endif
extern sigret_t synch_handler();
#endif /* convex || __convex__ */

#ifdef SAVESIGVEC
# define NSIGSAVED 7
 /*
  * These are not inline for speed. gcc -traditional -O on the sparc ignores
  * the fact that vfork() corrupts the registers. Calling a routine is not
  * nice, since it can make the compiler put some things that we want saved
  * into registers 				- christos
  */
# define savesigvec(sv)						\
   (mysigvec(SIGINT,  (sigvec_t *) 0, &(sv)[0]),		\
    mysigvec(SIGQUIT, (sigvec_t *) 0, &(sv)[1]),		\
    mysigvec(SIGTSTP, (sigvec_t *) 0, &(sv)[2]),		\
    mysigvec(SIGTTIN, (sigvec_t *) 0, &(sv)[3]),		\
    mysigvec(SIGTTOU, (sigvec_t *) 0, &(sv)[4]),		\
    mysigvec(SIGTERM, (sigvec_t *) 0, &(sv)[5]),		\
    mysigvec(SIGHUP,  (sigvec_t *) 0, &(sv)[6]),		\
    sigblock(sigmask(SIGINT) | sigmask(SIGQUIT) | 		\
	    sigmask(SIGTSTP) | sigmask(SIGTTIN) | 		\
	    sigmask(SIGTTOU) | sigmask(SIGTERM) |		\
	    sigmask(SIGHUP)))

# define restoresigvec(sv, sm)					\
    (void) (mysigvec(SIGINT,  &(sv)[0], (sigvec_t *) 0),	\
	    mysigvec(SIGQUIT, &(sv)[1], (sigvec_t *) 0),	\
	    mysigvec(SIGTSTP, &(sv)[2], (sigvec_t *) 0),	\
	    mysigvec(SIGTTIN, &(sv)[3], (sigvec_t *) 0),	\
	    mysigvec(SIGTTOU, &(sv)[4], (sigvec_t *) 0),	\
	    mysigvec(SIGTERM, &(sv)[5], (sigvec_t *) 0),	\
	    mysigvec(SIGHUP,  &(sv)[6], (sigvec_t *) 0),	\
	    sigsetmask(sm))
# endif				/* SAVESIGVEC */

#endif				/* _h_tc_sig */
