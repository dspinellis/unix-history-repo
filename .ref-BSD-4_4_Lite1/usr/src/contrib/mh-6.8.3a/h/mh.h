/* mh.h - main header file for all of MH */
/* @(#)$Id: mh.h,v 2.19 1993/02/26 21:53:29 jromine Exp $ */


/* Well-used constants */

#define	NOTOK		(-1)	/* syscall()s return this on error */
#define	OK		0	/*  ditto on success */
#define	DONE		1	/* trinary logic */
#define ALL		""
#define NULLCP		((char *) 0)
#define NULLVP		((char **) 0)
#define	Nbby		8	/* number of bits/byte */

#define MAXARGS		1000	/* max arguments to exec */

#define NFOLDERS	 300	/* max folder arguments on command line */

#ifndef	UCI
#define	MAXFOLDER	1000	/* message increment */
#else
#define	MAXFOLDER	1500	/* message increment */
#endif
#define DMAXFOLDER	   4	/* typical number of digits */

#if (!defined(BSD42) && !defined(BSD41A) && !defined(VMUNIX) && !defined(hpux)) || defined(_AIX)
#define	vfork		fork
#endif	/* not BSD */		/* how sad... */

/*  */

/* profile structure */

struct node {
    char   *n_name;		/* key */
    char   *n_field;		/* value */

    char    n_context;		/* context, not profile */

    struct node *n_next;	/* next entry */
};


/* switches structure */

#define	AMBIGSW		(-2)	/* from smatch() on ambiguous switch */
#define	UNKWNSW		(-1)	/*  ditto on unknown switch */

struct swit {
    char   *sw;
    int     minchars;
};

extern struct swit  anoyes[];	/* standard yes/no switches */


/* messages structure */

struct msgs {
    int     hghmsg;		/* Highest msg in directory     */
    int     nummsg;		/* Actual Number of msgs        */
    int     lowmsg;		/* Lowest msg number            */
    int     curmsg;		/* Number of current msg if any */

    int     lowsel;		/* Lowest selected msg number   */
    int     hghsel;		/* Highest selected msg number  */
    int     numsel;		/* Number of msgs selected      */

    char   *foldpath;		/* Pathname of folder           */

    int     msgflags;		/* Folder status bits           */
#ifndef	MTR
    char    pad1[sizeof (int) - sizeof (char)];
#endif /* not MTR */
#define READONLY  0x01		/*     No write access to folder */
#define	SEQMOD	  0x02		/*     folder's sequences modifed */
#define	MHPATH	  0x04		/*     mhpath-style folder handling */
#define	OTHERS	  0x08		/*     folder has other files	*/
#define	MODIFIED  0x10		/*     msh in-core folder modified */
#define	FBITS	"\020\01READONLY\02SEQMOD\03MHPATH\04OTHERS\05MODIFIED"

/* Note well: msgstats[] is a int, so we have 16 or 32 bits to work
	with.  The first 5 are for standard MH message flags,
	this leaves us 11 (or 27) for user-defined attributes.  Of
	these, 1 is reserved for future internal use, so this leaves
	users 10 (or 26).					*/
#define	NATTRS	((sizeof(int)*Nbby)-6) 		/*  see above	*/
    char   *msgattrs[NATTRS + 1];/* folder attributes		*/
    int     attrstats;		/* public=0/private=1		*/

    int	    lowoff;		/* low element in msgstats[] */
    int	    hghoff;		/* hgh element in msgstats[] */

#ifndef	MTR
    int     msgstats[1];	/* msg status			*/
#else /* MTR */
    int    *msgbase;		/* msg base			*/
    int    *msgstats;		/* msg status			*/
#endif /* MTR */
#define EXISTS		0x0001	/*     exists			*/
#define DELETED		0x0002	/*     deleted			*/
#define SELECTED	0x0004	/*     selected for use		*/
#define SELECT_EMPTY	0x0008	/*     mhpath "new"		*/
#define	UNSEEN		0x0010	/*     inc/show "unseen"	*/
#define	FFATTRSLOT	5	/*     user-defined attributes	*/
				/*	first free slot is	*/
				/*	(1 << 5) or 0x20	*/
#define	MBITS	"\020\01EXISTS\02DELETED\03SELECTED\04NEW\05UNSEEN"

#ifndef	MTR
#define	MHSIZE(mp,lo,hi)	\
		((unsigned) (sizeof *mp + ((hi) + 2) * sizeof *mp -> msgstats))
#else /* MTR */
#define	MHSIZE(mp,lo,hi)	((unsigned) sizeof *mp)
#define	MHSIZEX(mp,lo,hi)	\
		((unsigned) (((hi) - (lo) + 1) * sizeof *mp -> msgstats))
#endif /* MTR */
};

#define	NULLMP	((struct msgs *) 0)

/*  */

/* m_getfld() message parsing */

#define NAMESZ  128		/* Limit on component name size         */

#define LENERR  (-2)		/* Name too long error from getfld      */
#define FMTERR  (-3)		/* Message Format error                 */
#define FLD      0		/* Field returned                       */
#define FLDPLUS  1		/* Field " with more to come            */
#define FLDEOF   2		/* Field " ending at eom                */
#define BODY     3		/* Body  " with more to come            */
#define BODYEOF  4		/* Body  " ending at eom                */
#define FILEEOF  5		/* Reached end of input file            */


/* Maildrop styles */

#define	MS_DEFAULT	0	/* default (one msg per file) */
#define	MS_UNKNOWN	1	/* type not known yet */
#define	MS_UUCP		2	/* Unix-style "from" lines */
#define	MS_MMDF		3	/* string mmdlm2 */
#define	MS_MSH		4	/* whacko msh */

extern int msg_count;		/* m_getfld() indicators */
extern int msg_style;		/*  .. */
extern char *msg_delim;		/*  .. */


#define	NOUSE	0		/* draft being re-used */

#define TFOLDER 0		/* path() given a +folder */
#define TFILE   1		/* path() given a file */
#define	TSUBCWF	2		/* path() given a @folder */

#ifndef	LINK
#define	LINK	"@"
#endif /* not LINK */

#ifndef	SBACKUP
#define	SBACKUP	","
#endif /* not SBACKUP */


#define OUTPUTLINELEN	72	/* default line length for headers */

/*  */

/*
 * These standard strings are defined in config.c.  They are the
 * only system-dependent parameters in MH, and thus by redefining
 * their values and reloading the various modules, MH will run
 * on any system.
 */

extern char *components;
extern char *context;
extern char *current;
extern char *defalt;
extern char *digestcomps;
extern char *distcomps;
extern char *draft;
extern char *faceproc;
extern char *fileproc;
extern char *foldprot;
extern char *forwcomps;
extern char *inbox;
extern char *incproc;
extern char *installproc;
extern char *lproc;
extern char *mailproc;
extern char *mh_defaults;
extern char *mh_profile;
extern char *mh_seq;
extern char *mhlformat;
extern char *mhlforward;
extern char *mhlproc;
extern char *moreproc;
extern char *msgprot;
extern char *mshproc;
extern char *nsequence;
extern char *packproc;
extern char *postproc;
extern char *pfolder;
extern char *psequence;
extern char *rcvdistcomps;
extern char *replcomps;
extern char *rmfproc;
extern char *rmmproc;
extern char *sendproc;
extern char *showproc;
extern char *slocalproc;
extern char *sysed;
extern char *usequence;
extern char *version;
extern char *vmhproc;
extern char *whatnowproc;
extern char *whomproc;

/*  */

/* global variables -sigh- */

extern char ctxflags;
#define CTXMOD	0x01		/* context information modified */
#define	DBITS	"\020\01CTXMOD"

#ifdef	OVERHEAD
extern int  fd_def;
extern int  fd_ctx;
#endif /* OVERHEAD */

extern char *invo_name;		/* pgm invocation name */
extern char *mypath;		/* user's $HOME */
extern char *defpath;		/* pathname of user's profile */
extern char *ctxpath;		/* pathname of user's context */

extern struct node *m_defs;

/*  */

/* from the MH subroutine library */

char   *add ();
void	adios ();
void	admonish ();
void	advise ();
void	advertise ();
void	ambigsw ();
int     atooi ();
char  **brkstring ();
void	closefds ();
char   *concat ();
char   *copy ();
char  **copyip ();
void	cpydata ();
void	cpydgst ();
void	discard ();
void	done ();
int     fdcompare ();
int     gans ();
char  **getans ();
int	getanswer ();
char   *getcpy ();
void	help ();
char   *libpath ();
int     m_atoi ();
char   *m_backup ();
int     m_convert ();
int     m_delete ();
char   *m_draft ();
void	m_eomsbr ();
int     m_file ();
char   *m_find ();
void	m_fmsg ();
void    m_foil ();
void	m_getdefs ();
int     m_getfld ();
char   *m_getfolder ();
int     m_gmprot ();
struct msgs *m_gmsg ();
char   *m_maildir ();
char   *m_mailpath ();
char   *m_name ();
int	m_putenv ();
void	m_readefs ();
struct msgs *m_remsg ();
void	m_replace ();
char   *m_scratch ();
char   *m_seq ();
int	m_seqadd ();
char   *m_seqbits ();
int	m_seqdel ();
int	m_seqflag ();
int	m_seqnew ();
void	m_setcur ();
void	m_setseq ();
void	m_setvis ();
void    m_sync ();
char   *m_tmpfil ();
void	m_unknown ();
void	m_update ();
int	m_whatnow ();
int     makedir ();
char   *path ();
int     peekc ();
int     pidwait ();
#define	pidXwait(id,cp)	pidstatus (pidwait (id, NOTOK), stdout, cp)
int     pidstatus ();
void	printsw ();
void    push ();
char   *pwd ();
char   *r1bindex ();
int	refile ();
int	remdir ();
int     showfile ();
int     smatch ();
char   *sprintb();
int	ssequal ();
int	stringdex ();
char   *trimcpy ();
int     type ();
int     uleq ();
int	unputenv ();
int     uprf ();
int	vfgets ();

/*  */

#include "../h/strings.h"

/* should be in <stdio.h> */

#if	!defined(SYS5) && !defined(ncr) && !defined(_AIX) && !defined(OSF1) && !defined(__convex__) && !defined(__386BSD__) && !defined(BSD44)
typedef struct _iobuf  *FP;
FP   popen ();
#else /* SYS5 */
#define	FP	FILE*
#endif /* SYS5 */


/* miscellaneous */

#if !defined(BSD42) && !defined(hpux) && !defined(ncr) && !defined(_AIX) && !defined(RENAME)
#define	rename(f1,f2)	(link (f1, f2) != NOTOK ? unlink (f1) : NOTOK)
#endif	/* not BSD42 */

#define	setsig(s,f)	if (signal (s, SIG_IGN) != SIG_IGN) \
			    (void) signal (s, f)
#define	setsigx(i,s,f)	if ((i = signal (s, SIG_IGN)) != SIG_IGN) \
			    (void) signal (s, f)

#if	defined(sun) && !defined(NFS)
#define	NFS
#endif

#ifdef	NFS
#define	ruserpass	_ruserpass
#endif

#if (defined(BSD44) || defined(SUN40) || defined(hpux) \
	|| defined(_AIX) || defined (sgi)) && !defined(UNISTD)
#define       UNISTD
#endif
