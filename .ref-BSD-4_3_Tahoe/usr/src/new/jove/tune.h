/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define TUNED		/* don't touch this */

/*#define MAC 1		/* alas, there is no command line for this */

#ifdef MAC
#	define defined(x) (x)	/* take this out and you're in trouble... */
#endif


/* The operating system (MSDOS or MAC) must be defined by this point.
   IBMPC is defined in the Makefile. All MAC defines should be
   numerical (i.e. #define MAC 1) so that defined() will work. */

#if !(defined(MSDOS) || defined(MAC))
#	define UNIX
#endif


#ifdef UNIX		
			/* pick your version of Unix */
#   define BSD4_2	/* Berkeley 4.2 BSD */
#   define BSD4_3	/* Berkeley 4.3 BSD and 2.10 BSD */
/*# define SYSV		/* for (System III/System V) UNIX systems */
/*# define SYSVR2	/* system 5, rel. 2 */
			/* M_XENIX is defined by the Compiler */
#endif /* UNIX */

#ifdef SYSVR2
#   ifndef SYSV
#	define SYSV	/* SYSV is a subset of SYSVR2 */
#   endif
#endif

#ifdef BSD4_3
#   ifndef BSD4_2
#	define BSD4_2	/* 4.3 is 4.2 only different. */
#   endif
#endif

#ifdef M_XENIX
#   define iAPX286 1	/* we have segments. */
#endif

#ifdef MSDOS
#   ifdef M_I86LM		/* large memory model */
#		define NBUF 64
#	else
#		define NBUF 3
#   		define SMALL
#   endif
#   define BUFSIZ	512		/* or 1024 */
#endif

#ifdef UNIX
#   if vax || sel || sun || pyr || mc68000 || tahoe || iAPX286 || GOULD_NP1 || u3b2 || accel
#	define VMUNIX		/* Virtual Memory UNIX */
#	define BUFSIZ	1024
#	if iAPX286
#	    define NBUF	48	/* NBUF*BUFSIZ must be less than 64 kB */
#	else
#	    define NBUF	64	/* number of disk buffers */
#	endif /* iAPX286 */
#   else
#	define SMALL
#	define BUFSIZ	512	/* or 1024 */
#	define NBUF	3
#   endif
#
#   define LOAD_AV	/* Use the load average for various commands.
#			   Do not define this if you lack a load average
#			   system call and kmem is read protected. */
#
#   define JOB_CONTROL	/* if you have job stopping */
#
#   ifdef JOB_CONTROL
#       define MENLO_JCL
#       define IPROCS	/* Interactive processes only work with JOB_CONTROL. */
#   endif
#
#   define SUBPROCS	/* only on UNIX systems (NOT INCORPORATED YET) */
#endif /* UNIX */


#ifdef SMALL
    typedef	unsigned short	disk_line;
#else
#   if defined(iAPX286) || defined(MSDOS) || defined(MAC)
	typedef long	disk_line;
#   else
	typedef	int	disk_line;
#   endif /* iAPX286 */
#endif /* SMALL */

#define BACKUPFILES	/* enable the backup files code */
#define F_COMPLETION	/* filename completion */
#define ABBREV		/* word abbreviation mode */
#if !(defined(IBMPC) || defined(MAC))
#   define ANSICODES	/* extra commands that process ANSI codes */
#   define ID_CHAR	/* include code to IDchar */
#   define WIRED_TERMS	/* include code for wired terminals */
#endif
#define CHDIR		/* cd command and absolute pathnames */
#define LISP		/* include the code for Lisp Mode */
#define CMT_FMT		/* include the comment formatting routines */

#ifdef UNIX
#   define BIFF		/* if you have biff (or the equivalent) */
#   define KILL0	/* kill(pid, 0) returns 0 if proc exists */
#   define SPELL	/* spell words and buffer commands */
#if !sun && !iAPX286
#   define MY_MALLOC	/* use more memory efficient malloc (not on suns) */
#endif
#endif

#define DFLT_MODE	0666	/* file will be created with this mode */

#ifdef BSD4_3
#   define RESHAPING	/* enable windows to handle reshaping */
#endif

#ifdef BSD4_2			/* byte_copy(from, to, len) */
#   define	byte_copy bcopy	/* use fast assembler version */
#endif

#ifdef IPROCS
#   ifdef BSD4_2
#	define INPUT_SIG	SIGIO
#   else
#	define PIPEPROCS		/* do it with pipes */
#	define INPUT_SIG	SIGTINT
#   endif
#endif

#if defined(SYSV) || defined(MSDOS) || defined(M_XENIX)
#   define byte_copy(s2, s1, n)	memcpy(s1, s2, n)
#   define bzero(s, n)	memset(s, 0, n)
#   define index	strchr
#   define rindex	strrchr
#endif

#ifdef MAC
#	undef F_COMPLETION	/* can't do it with spaces in filenames */
#	undef CHDIR
#	define CHDIR 1
#	define rindex strrchr
#	define index strchr
#	define bzero(s,n) setmem(s,n,0)
#	define LINT_ARGS
#	define NBUF 64
#	define BUFSIZ 1024
#endif

/* These are here since they define things in tune.c.  If you add things to
   tune.c, add them here too, if necessary. */

#ifndef NOEXTERNS
extern char
	*d_tempfile,
	*p_tempfile,
	*Recover,
	*Joverc,

#ifdef PIPEPROCS
	*Portsrv,
#endif

#ifdef MSDOS
	CmdDb[],
#else
	*CmdDb,
#endif

	TmpFilePath[],
	Shell[],
	ShFlags[];
#endif /* NOEXTERNS */


