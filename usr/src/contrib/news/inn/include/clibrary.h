/*  $Revision: 1.17 $
**
**  Here be declarations of routines and variables in the C library.
**  You must #include <sys/types.h> and <stdio.h> before this file.
*/

#if	defined(DO_HAVE_UNISTD)
#include <unistd.h>
#endif	/* defined(DO_HAVE_UNISTD) */

#if	defined(DO_HAVE_VFORK)
#include <vfork.h>
#endif	/* defined(DO_HAVE_VFORK) */

    /* Generic pointer, used by memcpy, malloc, etc. */
    /* =()<typedef @<POINTER>@ *POINTER;>()= */
typedef void *POINTER;
    /* What is a file offset?  Will not work unless long! */
    /* =()<typedef @<OFFSET_T>@ OFFSET_T;>()= */
typedef long OFFSET_T;
    /* What is the type of an object size? */
    /* =()<typedef @<SIZE_T>@ SIZE_T;>()= */
typedef size_t SIZE_T;
    /* What is the type of a passwd uid and gid, for use in chown(2)? */
    /* =()<typedef @<UID_T>@ UID_T;>()= */
typedef uid_t UID_T;
    /* =()<typedef @<GID_T>@ GID_T;>()= */
typedef gid_t GID_T;
    /* =()<typedef @<PID_T>@ PID_T;>()= */
typedef pid_t PID_T;
    /* What should a signal handler return? */
    /* =()<#define SIGHANDLER	@<SIGHANDLER>@>()= */
#define SIGHANDLER	void

#if	defined(SIG_DFL)
    /* What types of variables can be modified in a signal handler? */
    /* =()<typedef @<SIGVAR>@ SIGVAR;>()= */
typedef sig_atomic_t SIGVAR;
#endif	/* defined(SIG_DFL) */

/* =()<#include @<STR_HEADER>@>()= */
#include <string.h>
/* =()<#include @<MEM_HEADER>@>()= */
#include <memory.h>


/*
**  It's a pity we have to go through these contortions, for broken
**  systems that have fd_set but not the FD_SET.
*/
#if	defined(FD_SETSIZE)
#define FDSET		fd_set
#else
#include <sys/param.h>
#if	!defined(NOFILE)
	error -- #define NOFILE to the number of files allowed on your machine!
#endif	/* !defined(NOFILE) */
#if	!defined(howmany)
#define howmany(x, y)	(((x) + ((y) - 1)) / (y))
#endif	/* !defined(howmany) */
#define FD_SETSIZE	NOFILE
#define NFDBITS		(sizeof (long) * 8)
typedef struct _FDSET {
    long	fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} FDSET;
#define FD_SET(n, p)	(p)->fds_bits[(n) / NFDBITS] |= (1 << ((n) % NFDBITS))
#define FD_CLR(n, p)	(p)->fds_bits[(n) / NFDBITS] &= ~(1 << ((n) % NFDBITS))
#define FD_ISSET(n, p)	((p)->fds_bits[(n) / NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p)	(void)memset((POINTER)(p), 0, sizeof *(p))
#endif	/* defined(FD_SETSIZE) */


#if	!defined(SEEK_SET)
#define SEEK_SET	0
#endif	/* !defined(SEEK_SET) */
#if	!defined(SEEK_END)
#define SEEK_END	2
#endif	/* !defined(SEEK_END) */

/*
**  We must use #define to set FREEVAL, since "typedef void FREEVAL;" doesn't
**  work on some broken compilers, sigh.
*/
/* =()<#define FREEVAL @<FREEVAL>@>()= */
#define FREEVAL int

extern int		optind;
extern char		*optarg;
#if	!defined(__STDC__)
extern int		errno;
#endif	/* !defined(__STDC__) */

extern char		*getenv();
extern char		*inet_ntoa();
extern char		*mktemp();
#if	!defined(strerror)
extern char		*strerror();
#endif	/* !defined(strerror) */
extern long		atol();
extern time_t		time();
extern unsigned long	inet_addr();
extern FREEVAL		free();
extern POINTER		malloc();
extern POINTER		realloc();
#if	defined(ACT_MMAP)
extern char		*mmap();
#endif	/* defined(ACT_MMAP) */

/* Some backward systems need this. */
extern FILE	*popen();

/* This is in <mystring.h>, but not in some system string headers,
 * so we put it here just in case. */
extern int	strncasecmp();

/* =()<extern @<ABORTVAL>@	abort();>()= */
extern int	abort();
/* =()<extern @<ALARMVAL>@	alarm();>()= */
extern unsigned int	alarm();
/* =()<extern @<EXITVAL>@	exit();>()= */
extern void	exit();
/* =()<extern @<GETPIDVAL>@	getpid();>()= */
extern pid_t	getpid();
/* =()<extern @<LSEEKVAL>@	lseek();>()= */
extern off_t	lseek();
/* =()<extern @<QSORTVAL>@	qsort();>()= */
extern int	qsort();
/* =()<extern @<SLEEPVAL>@	sleep();>()= */
extern unsigned int	sleep();
/* =()<extern @<_EXITVAL>@	_exit();>()= */
extern void	_exit();
