/*
 * config.h -- configure various defines for tcsh
 *
 * All source files should #include this FIRST.
 *
 * Edit this to match your system type.
 */

/****************** System dependant compilation flags ****************/
/*
 * POSIX	This system supports IEEE Std 1003.1-1988 (POSIX).
 */
#define POSIX

/*
 * POSIXJOBS	This system supports the optional IEEE Std 1003.1-1988 (POSIX)
 *		job control facilities.
 */
#define POSIXJOBS

/*
 * VFORK	This machine has a vfork().  
 *		It used to be that for job control to work, this define
 *		was mandatory. This is not the case any more.
 *		If you think you still need it, but you don't have vfork, 
 *		define this anyway and then do #define vfork fork.  
 *		I do this anyway on a Sun because of yellow pages brain damage,
 *		[should not be needed under 4.1]
 *		and on the iris4d cause	SGI's fork is sufficiently "virtual" 
 *		that vfork isn't necessary.  (Besides, SGI's vfork is weird).
 *		Note that some machines eg. rs6000 have a vfork, but not
 *		with the berkeley semantics, so we cannot use it there either.
 */
#define VFORK

/*
 * BSDJOBS	You have BSD-style job control (both process groups and
 *		a tty that deals correctly
 */
#define BSDJOBS

/*
 * BSDSIGS	You have 4.2-style signals, rather than USG style.
 *		Note: POSIX systems should not define this unless they
 *		have sigvec() and friends (ie: 4.3BSD-RENO, HP-UX).
 */
#define BSDSIGS

/*
 * BSDTIMES	You have BSD-style process time stuff (like rusage)
 *		This may or may not be true.  For example, Apple Unix
 *		(OREO) has BSDJOBS and BSDSIGS but not BSDTIMES.
 */
#define BSDTIMES

/*
 * BSDNICE	Your system uses setpriority() instead of nice, to
 *		change a processes scheduling priority
 */
#define BSDNICE

/*
 * TERMIO	You have struct termio instead of struct sgttyb.
 * 		This is usually the case for SVID systems, where
 *		BSD uses sgttyb. POSIX systems should define this
 *		anyway, even though they use struct termios.
 */
#define TERMIO

/*
 * SVID		Your machine is SVID complient (Sys V, HPUX, A/UX)
 *		NOTE: don't do this if you are on a Pyramid -- tcsh is
 *		built in a BSD universe.
 *		Set SVID to 1, 2, 3, or 4, depending the version of System V
 *		you are running. Or set it to 0 if you are not SVID based
 */
#define SVID	0

/*
 * YPBUGS	Work around Sun YP bugs that cause expansion of ~username
 *		to send command output to /dev/null
 */
#undef YPBUGS

/*
 * SIGVOID	Define this if your signal handlers return void.  On older
 *		systems, signal returns int, but on newer ones, it returns void.
 */
#define SIGVOID 

/*
 * HAVEDUP2	Define this if your system supports dup2().
 */
#define HAVEDUP2

/*
 * UTHOST	Does the utmp file have a host field?
 */
#define UTHOST

/*
 * DIRENT	Your system has <dirent.h> instead of <sys/dir.h>
 */
#define DIRENT
/****************** local defines *********************/
/****************** configurable hacks ****************/
/* have been moved to config_f.h */
#include "config_f.h"
