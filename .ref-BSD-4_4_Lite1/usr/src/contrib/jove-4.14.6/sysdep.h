/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/


/* Determine if really ANSI C */
#ifdef	__STDC__
# if	__STDC__ >= 0
#  define REALSTDC 1
# endif
#endif

#ifdef	THINK_C
# define MAC 1
/* Think C does not have a "defined" preprocessor operator.
 * This kludge is intended to avoid the problem.
 * ??? Perhaps Think C has been fixed by now. -- DHR
 */
# define defined(x) (x)
  typedef int size_t;
#endif

/* The operating system (MSDOS or MAC) must be defined by this point.
   IBMPC is defined in the Makefile. */

#ifndef	MSDOS
# ifndef	MAC
#   define UNIX	1	/* default to UNIX */
# endif
#endif

#ifdef	MAC
# define byte_zero(s,n) setmem((s),(n),0)
# define NBUF 64
# define JBUFSIZ 1024
#endif

#ifdef	MSDOS
# ifdef	M_I86LM		/* large memory model */
#  define NBUF 		64
# else
#  define NBUF 		3
#  define SMALL		1
# endif
# define JBUFSIZ		512		/* or 1024 */
#endif

#ifdef	UNIX
# ifdef	pdp11
#  define SMALL	1
#  define JBUFSIZ	512	/* or 1024 */
#  define NBUF		3
# else
#  define VMUNIX	1	/* Virtual Memory UNIX */
#  define JBUFSIZ	1024
#  ifdef	iAPX286
#   define NBUF		48	/* NBUF*JBUFSIZ must be less than 64 kB */
#  else
#   define NBUF	64	/* number of disk buffers */
#  endif	/* iAPX286 */
# endif
#endif

#ifdef	SVR3
# define SYSVR3		1	/* system 5, rel. 3 */
#endif

#ifdef	SVR2
# define SYSVR2		1	/* system 5, rel. 2 */
#endif

#ifdef	BSD
# define BSD4_2		1	/* Berkeley 4.2 BSD or later */
#endif

#if	defined(sun) || defined(__sun__)
# define BSD4_2	1	/* True enough for Jove's purposes */
/* # define YP_PASSWD	1	/* if you are a sun running the yellow pages */
# ifdef	SUNOS4	/* gone to void */
#  define TERMIOS	1	/* uses termio struct for terminal modes */
#  define DIRENT	1	/* Posix style dirent.h */
#  define SIGRESULT	void
#  define SIGRETURN	{ return; }
#ifdef GCC
#  define POSIX_UNISTD	1	/* prototypes in unistd.h, don't use our own */
#endif
# endif
#endif

#if	defined(BSD386)
# define BSD4_2	1	/* True enough for Jove's purposes */
#  define TERMIOS	1	/* uses termio struct for terminal modes */
#  define DIRENT	1	/* Posix style dirent.h */
#  define SIGRESULT	void
#  define SIGRETURN	{ return; }
#  define POSIX_UNISTD	1	/* prototypes in unistd.h, don't use our own */
#endif

#define KILL0		1	/* kill(pid, 0) returns 0 if proc exists */

#if	defined(ultrix) || defined(__ultrix__)
# define ULTRIX	1
# define BSD4_2		1	/* True enough for Jove's purposes */
# define SIGRESULT  void
# define SIGRETURN  {return;}
#endif

/* M_XENIX is defined by the Compiler */
/* SYSV should be defined for (System III/System V) UNIX systems */

#ifdef SYSVR4
# define SYSVR3		1
# define DIRENT		1	/* Posix style dirent.h */
# define POSIX_UNISTD	1	/* prototypes in unistd.h, don't use our own */
#endif

#ifdef	SYSVR3
# ifndef	SYSVR2
#  define SYSVR2	1	/* SYSVR2 is a subset of SYSVR3 */
# endif
# define	SIGRESULT	void
# define	SIGRETURN	{ return; }
# ifndef	SYSVR4
#  define	SIGCHLD		SIGCLD
# endif
#endif

#ifdef	SYSVR2
# ifndef	SYSV
#  define SYSV	1	/* SYSV is a subset of SYSVR2 */
# endif
#endif

#ifdef	M_XENIX
# define iAPX286 1	/* we have segments */
# define BSD_DIR	1
#endif

#if defined(SYSTYPE_BSD43) || defined(__SYSTYPE_BSD43)
# define BSD4_2		1	/* RISCOS4.x on MIPS */
#endif

#ifdef	BSD4_2
# define byte_copy(from, to, len)	bcopy((UnivConstPtr)(from), (UnivPtr)(to), (size_t)(len))
# define byte_zero(s, n)	bzero((UnivPtr)(s), (size_t)(n))
# define strchr	index
# define strrchr	rindex
# define BSD_SIGS	1	/* Berkeley style signals */
# define BSD_WAIT	1	/* Berkeley style sys/wait.h */
# define WAIT3		1	/* Berkeley style wait3() */
# define BSD_DIR	1	/* Berkeley style dirent routines */
# define VFORK		1	/* if you have vfork(2) */
# define JOB_CONTROL	1	/* if you have job stopping */
# define MENLO_JCL	1
# define HAVE_GETWD	1	/* have the getwd() routine */
# ifndef TERMIOS
# define SGTTY		1	/* uses SGTTY for terminal modes */
#endif
#endif

#ifdef	A_UX		/* A/UX on a MacII (Do *not* define "MAC") */
/* It might be better to define POSIX compatibility and try that. Oh well! */
# define BSD_WAIT	1	/* Berkeley style sys/wait.h */
# define BSD_DIR	1	/* Berkeley style dirent routines */
# define WAIT3		1	/* Berkeley style wait3() */
# define BSD_SIGS	1	/* Berkeley style signals */
# define SYSV		1	/* System V everything else */
# define TERMIO	1	/* uses termio struct for terminal modes */
#endif

#ifdef AIX	/* from guttman@mashie.ece.jhu.edu via buchanan@cs.ubc.ca */
# define BSD_DIR
# define HAVE_GETWD
# define SYSV
# define TERMIO	1	/* uses termio struct for terminal modes */
#endif

#if	(defined(mips) || defined(__mips__)) && !defined(BSD4_2)
/*
 * Older MIPS (UMIPS-SYSV, anything other than their 4.3 port before
 * RISCOS4.x) and SGI 4D OSes (anything before Irix3.3) have BSD style wait,
 * and directory routines if you link -lbsd and define -I/usr/include/bsd on
 * the compile line. But they have SysV style signals.  Jove was ported to the
 * SGI 68K boxes once, but it the mods seem to have been lost.
 */
# ifndef	ULTRIX
   /* Not a DECstation 3100 or suchlike */
#  define BSD_WAIT	1	/* Berkeley style sys/wait.h */
#  define BSD_DIR	1	/* Berkeley style dirent routines */
# else
#  undef ULTRIX		1	/* Only needed it for this test */
# endif
# if	defined(sgi) || defined(__sgi__)
#  define WAIT3		1	/* Berkeley style wait3() */
#  define JOB_CONTROL	1	/* if you have job stopping */
#  define HAVE_GETWD	1
   /* All the following are for Irix 3.3 onwards */
#  define BSD_SIGS	1	/* Berkeley style signals */
#  define DIRENT	1	/* Posix style dirent.h */
#  define TERMIOS	1	/* new Posix terminal mode management */
#  ifndef REALSTDC
#   define REALSTDC	1	/* close enough for Jove's needs */
#  endif
#  define HAVE_STRERROR	1	/* have ANSI strerror() */
#  define POSIX_UNISTD	1	/* prototypes in unistd.h, don't use our own */
#  define SIGRESULT	void
#  define SIGRETURN	{ return; }
#  undef SIGCHLD		/* #define SIGCHLD SIGCLD in signal.h */
# endif
#endif

#ifndef	BSD4_2
# define KBDSIG		SIGEMT
#endif

#if	defined(SYSV) || defined(MSDOS) || defined(M_XENIX)
# include <memory.h>
# define byte_copy(from, to, count)	memcpy((UnivPtr)(to), (UnivConstPtr)(from), (size_t)(count))
# define byte_zero(s, n)		memset((UnivPtr)(s), 0, (size_t)(n))
#endif

#ifdef	UNIX
# define TERMCAP	1
# define ASCII7	1
#endif

#ifdef	ASCII7	/* seven bit characters */
# define NCHARS 0200
#else
# define NCHARS 0400
#endif

#define CHARMASK (NCHARS - 1)

#ifndef	MSDOS
# define FILESIZE	256
#else	/* MSDOS */
# define FILESIZE	64
#endif	/* MSDOS */

#ifndef	SIGRESULT	/* default to old-style */
# define	SIGRESULT	int
# define	SIGRETURN	{ return 0; }
#endif

#ifndef	BSD4_2
# ifdef	MENLO_JCL
#  define signal	sigset
# endif	/* MENLO_JCL */
#endif

#ifdef	BSD_SIGS
extern long	SigMask;

# define SigHold(s)	sigblock(SigMask |= sigmask((s)))
# define SigRelse(s)	sigsetmask(SigMask &= ~sigmask((s)))
#else
# define SigHold(s)	sighold(s)
# define SigRelse(s)	sigrelse(s)
# define killpg(pid, sig)	kill(-(pid), (sig))
#endif

/* On a system which limits JOVE to a very small data segment,
 * it may be worthwhile limiting daddr to a short.  This reduces
 * the size of a Line descriptor, but reduces the addressable size
 * of the temp file.  This is reasonable on a PDP-11 and perhaps
 * an iAPX*86.
 */

#ifdef	SMALL
  typedef unsigned short	daddr;
#else
  typedef unsigned long	daddr;
#endif	/* SMALL */

#define	NULL_DADDR		((daddr) 0)

#if !defined(TERMIOS) && !defined(SGTTY)
# define TERMIO	1	/* uses termio struct for terminal modes */
#endif

#ifdef SYSV
# define MAILSPOOL "/usr/mail"
#else
# define MAILSPOOL "/usr/spool/mail"
#endif
