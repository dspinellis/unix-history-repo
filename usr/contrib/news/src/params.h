/*
 * params.h - parameters for everyone.
 */

/*	@(#)params.h	2.20	1/17/86	*/

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <grp.h>
#include <pwd.h>
#include <sys/stat.h>
#include <ctype.h>

#include "defs.h"

#if defined(BSD4_2) || defined(BSD4_1C)
#include <sys/time.h>
#else /* sane */
#include <time.h>
#endif /* sane */

#ifndef UNAME
/*
 * 9 bytes is for compatibility with USG, in case you forget to define UNAME.
 * 33 bytes in nodename because many sites have names longer than 8 chars.
 */

struct utsname {
	char	sysname[9];
	char	nodename[33];
	char	release[9];
	char	version[9];
};
#else
#include <sys/utsname.h>
#endif

#ifndef USG
#include <sys/timeb.h>
#else
struct timeb
{
	time_t	time;
	unsigned short millitm;
	short	timezone;
	short	dstflag;
};
#endif

#include "header.h"

/* line from SUBFILE */
struct	srec {
	char	s_name[2*BUFLEN];	/* system name		*/
	char	*s_nosend;		/* systems that inhibit sending */
	char	s_nbuf[LBUFLEN];	/* system subscriptions */
	char	s_flags[BUFLEN];	/* system flags		*/
	char	s_xmit[LBUFLEN];	/* system xmit routine	*/
};

extern	int	uid, gid, duid, dgid;
extern	int	savmask, SigTrap, mode, lockcount;
extern	struct	hbuf header;
extern	char	bfr[LBUFLEN], *username, *userhome;

extern	char	*SPOOL, *LIB, *BIN, *SUBFILE, *ACTIVE;
extern	char	*LOCKFILE, *SEQFILE, *ARTFILE;
extern	char	*news_version, *Progname;

#ifdef NOTIFY
extern	char	*TELLME;
#endif /* NOTIFY */

#ifdef HIDDENNET
extern char	*LOCALSYSNAME;
#endif /* HIDDENNET */

extern	char	*FULLSYSNAME;
#ifndef SHELL
extern char	*SHELL;
#endif /* !SHELL */

/* external function declarations */
extern	FILE	*xfopen(), *hread();
extern	char	*strcpy(), *strncpy(), *strcat(), *index(), *rindex();
extern	char	*ctime(), *mktemp(), *malloc(), *realloc(), *getenv();
extern	char	*arpadate(), *dirname(), *AllocCpy(), *strpbrk();
extern	char	*errmsg();
extern	struct	passwd *getpwnam(), *getpwuid(), *getpwent();
extern	struct	group *getgrnam();
extern	time_t	time(), getdate(), cgtdate();
extern	int	broadcast(), save(), newssave(), ushell(), onsig();
extern	long	atol();
extern	struct	tm *localtime();

#ifdef lint
/* This horrible gross kludge is the only way I know to
 * convince lint that signal(SIGINT,SIG_IGN) is legal. It hates SIG_IGN.
 */
#ifdef SIG_IGN
#undef SIG_IGN
#endif /* SIG_IGN */
#define SIG_IGN	main
extern int main();
#endif /* lint */

#ifdef VMS
#define LINK(a,b)	vmslink(a,b)
#define UNLINK(a)	vmsunlink(a)
#else	
#define LINK(a,b)	link(a,b)
#define UNLINK(a)	unlink(a)
#endif /* !VMS */
