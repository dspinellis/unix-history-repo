/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#define TUNED		/* don't touch this */

/*#define LSRHS		/* if this is Lincoln-Sudbury Regional High School */

#define LOAD_AV		/* Use the load average for various commands.
			   Do not define this if you lack a load average
			   system call and kmem is read protected. */

/* #define SYSV		/* for (System III/System V) UNIX systems */

#define BACKUPFILES	/* enable the backup files code */

#define BIFF		/* if you have biff (or the equivalent) */

#define JOB_CONTROL	/* if you have job stopping */

#ifdef JOB_CONTROL
#	define MENLO_JCL
#	define IPROCS	/* Interactive processes.  PROCS only works
			   with JOB_CONTROL. */
#endif

#define F_COMPLETION	/* filename completion */

#define CHDIR		/* change directory command and absolute pathnames */

/*#define	KILL0	/* kill(pid, 0) returns 0 if proc exists
			   Used by recover to test if jove is
			   still running. */

#define SPELL		/* spell words and buffer commands */

#define ABBREV		/* word abbreviation mode */

#define LISP		/* include the code for Lisp Mode */

#define ID_CHAR		/* include code to IDchar */

#define WIRED_TERMS	/* include code for wired terminals */

#define ANSICODES	/* Include extra commands that process
			   ANSI codes sequences.  Includes simple
			   mouse support pointing. */

#define CMT_FMT		/* include the comment formatting routines */

#if !sun
#   define MY_MALLOC	/* use more memory efficient malloc */
#endif

#define BSD4_2		/* Berkeley 4.2 BSD */

#define BSD4_3		/* Berkeley 4.3 BSD */

#ifdef BSD4_3
#	ifndef BSD4_2
#		define BSD4_2	/* 4.3 is 4.2 only different. */
#	endif
#endif

#ifdef BSD4_3
#	define RESHAPING	/* enable windows to handle reshaping */
#endif

#ifdef BSD4_2			/* byte_copy(from, to, len) */
#	define	byte_copy bcopy	/* use fast assembler version */
#endif

#if vax || sel || sun || pyr || mc68000 || tahoe
#	define VMUNIX		/* Virtual Memory UNIX */
#endif

#ifndef VMUNIX
	typedef	short	disk_line;
#	define BUFSIZ	512		/* or 1024 */
#else
	typedef	int	disk_line;
#	define BUFSIZ	1024
#endif

#define DFLT_MODE	0666	/* file will be created with this mode */

#ifdef VMUNIX
#	define NBUF		64	/* number of disk buffers */
#else
#	define NBUF		3	/* only 3 because there is a bug */
#endif

#ifdef IPROCS
#	ifndef NOEXTERNS
		extern char *PORTSRV;
#	endif
#	ifdef BSD4_2
#		define INPUT_SIG	SIGIO
#	else
#		define PIPEPROCS		/* do it with pipes */
#		define INPUT_SIG	SIGTINT
#	endif
#endif

#ifdef SYSV
#	define index	strchr
#	define rindex	strrchr
#endif

/* These are here since they define things in tune.c.  If you add things to
   tune.c, add them here too, if necessary. */

#ifndef NOEXTERNS
extern char
	*TMP_DIR,
	*REC_DIR,
	*TMPFILE,
	*RECFILE,
	*REC_BASE,
	*RECOVER,
	*CMD_DB,
	*JOVERC,
	Shell[],
	ShFlags[];

#endif NOEXTERNS
