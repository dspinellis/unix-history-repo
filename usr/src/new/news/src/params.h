/*
 * params.h - parameters for everyone.
 */

/*	@(#)params.h	2.25	10/15/87	*/

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

extern	char	*LOCALSYSNAME, *LOCALPATHSYSNAME, *FROMSYSNAME, *PATHSYSNAME;

#ifndef SHELL
extern	char	*SHELL;
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
#define UNLINK(a)	vmsdelete(a)
FILE *art_open(), *xart_open();
#else	
#define LINK(a,b)	link(a,b)
#define UNLINK(a)	unlink(a)
#define art_open fopen
#define xart_open xfopen
#endif /* !VMS */

/* Check for old naming scheme using HIDDENNET */
#ifdef HIDDENNET
#  ifndef GENERICFROM		/* Ugly fix, only for use in pathinit.c */
#    define GENERICFROM "%s%0.0s%s", HIDDENNET
#    define HIDDENNET_IN_LOCALSYSNAME
#  endif
#  ifndef GENERICPATH
#    define GENERICPATH HIDDENNET
#  endif
#endif

#ifdef M_XENIX
/* fake SVID adivsory locking with xenix routines */
#define lockf	locking
#define F_ULOCK	0
#define F_LOCK	3
#define F_TLOCK	4
#endif /* M_XENIX */

#ifdef IHCC
#define DOGETUSERS
#define LOGDIR
#endif /* IHCC */

#ifdef BSD4_2
#define MKDIR_SUB
#define READDIR
#endif /* BSD4_2 */

#ifdef READDIR
#include <sys/dir.h>
#else /* !READDIR */
#include "ndir.h"
#endif /* !READDIR */

#if defined(DBM) && !defined(M_XENIX)
typedef struct {
	char *dptr;
	int dsize;
} datum;
#endif /* DBM &! XENIX */

#define STRCMP(a,b)  ((*(a) != *(b)) ? (*(a)-*(b)) : strcmp((a)+1, (b)+1))
#define STRNCMP(a,b,n)  ((*(a) != *(b)) ? (*(a)-*(b)) : strncmp(a, b, n))
#define PREFIX(a,b)  ((*(a) != *(b)) ? FALSE : prefix((a)+1, (b)+1))

#ifdef SERVER
/* from clientlib.c */
extern	char	*getserverbyfile();
extern	int	server_init();
extern  void	put_server();
extern	int	get_server();
extern	void	close_server();
/* from nntp.c */
extern	FILE	*open_active();
extern	int	open_server();
extern	char	*set_group();
extern	char	*active_name();
extern	char	*group_name();
extern	FILE	*getarticle();
extern	FILE	*getartbyid();
extern	char	*article_name();
extern	void	sync_server();
extern	int	strindex();
#endif /* SERVER */
