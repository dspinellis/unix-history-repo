/*  $Revision: 1.41 $
**
**  Many of the data types used here have abbreviations, such as CT
**  for CHANNELTYPE.  Here are a list of the conventions and meanings:
**	ART	A news article
**	CHAN	An I/O channel
**	CS	Channel state
**	CT	Channel type
**	FNL	Funnel, into which other feeds pour
**	FT	Feed type -- how a site gets told about new articles
**	ICD	In-core data (primarily the active and sys files)
**	LC	Local NNTP connection-receiving channel
**	CC	Control channel (used by ctlinnd)
**	NC	NNTP client channel
**	NG	Newsgroup
**	NGH	Newgroup hashtable
**	PROC	A process (used to feed a site)
**	PS	Process state
**	RC	Remote NNTP connection-receiving channel
**	RCHAN	A channel in "read" state
**	SITE	Something that gets told when we get an article
**	WCHAN	A channel in "write" state
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include "nntp.h"
#include "paths.h"
#include "logging.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


#define NOSITE		-1


/*
**  Some convenient shorthands.
*/
typedef struct in_addr	INADDR;


/*
**  Server's operating mode.
*/
typedef enum _OPERATINGMODE {
    OMrunning,
    OMpaused,
    OMthrottled
} OPERATINGMODE;


/*
**  An I/O buffer, including a size, an amount used, and a count of how
**  much space is left if reading or how much still needs to be written.
*/
typedef struct _BUFFER {
    int		Size;
    int		Used;
    int		Left;
    char	*Data;
} BUFFER;


/*
**  What program to handoff a connection to.
*/
typedef enum _HANDOFF {
    HOnnrpd,
    HOnnrqd,
    HOnntpd
} HANDOFF;


/*
**  Set of channel types.
*/
typedef enum _CHANNELTYPE {
    CTany,
    CTfree,
    CTremconn,
    CTnntp,
    CTlocalconn,
    CTcontrol,
    CTfile,
    CTexploder,
    CTprocess
} CHANNELTYPE;


/*
**  The state a channel is in.  Interpretation of this depends on the
**  channel's type.  Used mostly by CTnntp channels.
*/
typedef enum _CHANNELSTATE {
    CSerror,
    CSwaiting,
    CSgetcmd,
    CSgetauth,
    CSwritegoodbye,
    CSwriting,
    CSpaused,
    CSgetarticle,
    CSeatarticle,
    CSgetrep
} CHANNELSTATE;


/*
**  I/O channel, the heart of the program.  A channel has input and output
**  buffers, and functions to call when there is input to be read, or when
**  all the output was been written.
*/
typedef struct _CHANNEL {
    CHANNELTYPE		Type;
    CHANNELSTATE	State;
    int			fd;
    int			Reported;
    long		Received;
    long		Refused;
    long		Rejected;
    int			BadWrites;
    int			BadReads;
    int			BlockedWrites;
    int			BadCommands;
    time_t		LastActive;
    time_t		NextLog;
    INADDR		Address;
    FUNCPTR		Reader;
    FUNCPTR		WriteDone;
    time_t		Waketime;
    time_t		Started;
    FUNCPTR		Waker;
    POINTER		Argument;
    POINTER		Event;
    BUFFER		In;
    BUFFER		Out;
    BOOL		Tracing;
} CHANNEL;


/*
**  A newsgroup has a name in different formats, and a high-water count,
**  also kept in different formats.  It also has a list of sites that
**  get this group.
*/
typedef struct _NEWSGROUP {
    long		Start;		/* Offset into the active file	*/
    char		*Name;
    char		*Dir;		/* The name, as a directory	*/
    int			NameLength;
    ARTNUM		Last;
    ARTNUM		Filenum;	/* File name to use		*/
    int			Lastwidth;
    int			PostCount;	/* Have we already put it here?	*/
    char		*LastString;
    char		*Rest;
    int			nSites;
    int			*Sites;
    struct _NEWSGROUP	*Alias;
} NEWSGROUP;


/*
**  How a site is fed.
*/
typedef enum _FEEDTYPE {
    FTerror,
    FTfile,
    FTchannel,
    FTexploder,
    FTfunnel,
    FTlogonly,
    FTprogram
} FEEDTYPE;


/*
**  A site may reject something in its subscription list if it has
**  too many hops, or a bad distribution.
*/
typedef struct _SITE {
    char		*Name;
    char		*Entry;
    int			NameLength;
    char		**Exclusions;
    char		**Distributions;
    char		**Patterns;
    BOOL		Sendit;
    BOOL		Seenit;
    BOOL		DistRequired;
    BOOL		IgnorePath;
    int			Hops;
    int			Groupcount;
    FEEDTYPE		Type;
    NEWSGROUP		*ng;
    BOOL		Spooling;
    char		*SpoolName;
    BOOL		Working;
    long		StartWriting;
    long		StopWriting;
    long		StartSpooling;
    char		*Param;
    char		FileFlags[FEED_MAXFLAGS + 1];
    long		MaxSize;
    CHANNEL		*Channel;
    BOOL		IsMaster;
    int			Master;
    int			Funnel;
    BOOL		FNLwantsnames;
    BUFFER		FNLnames;
    int			Process;
    PID_T		pid;
    int			Flushpoint;
    BUFFER		Buffer;
    BOOL		Buffered;
    struct _SITE	*Next;
    struct _SITE	*Prev;
} SITE;


/*
**  A process is something we start up to send articles.
*/
typedef enum _PROCSTATE {
    PSfree,
    PSrunning,
    PSdead
} PROCSTATE;


/*
**  We track our children and collect them synchronously.
*/
typedef struct _PROCESS {
    PROCSTATE	State;
    PID_T	Pid;
    int		Status;
    time_t	Started;
    time_t	Collected;
    int		Site;
} PROCESS;


/*
**  Miscellaneous data we want to keep on an article.  All the fields
**  are not always valid.
*/
typedef struct _ARTDATA {
    STRING	Poster;
    STRING	Replyto;
    char	*Body;
    time_t	Posted;
    time_t	Arrived;
    time_t	Expires;
    int		Groupcount;
    int		LinesValue;
    char	Lines[SMBUF];
    long	SizeValue;
    char	Size[SMBUF];
    int		SizeLength;
    char	Name[SPOOLNAMEBUFF];
    int		NameLength;
    char	TimeReceived[33];
    int		TimeReceivedLength;
    STRING	MessageID;
    int		MessageIDLength;
    STRING	Newsgroups;
    int		NewsgroupsLength;
    STRING	Distribution;
    int		DistributionLength;
    STRING	Feedsite;
    int		FeedsiteLength;
    STRING	Replic;
    int		ReplicLength;
    BUFFER	*Headers;
    BUFFER	*Overview;
} ARTDATA;




/*
**  In-line macros for efficiency.
*/

#if	defined(lint) || defined(__CENTERLINE__)
extern int	KeepLintQuiet;
#define JUSTONCE	KeepLintQuiet
#else
#define JUSTONCE	0
#endif	/* defined(lint) || defined(__CENTERLINE__) */

/*
**  Set or append data to a channel's output buffer.
*/
#define WCHANset(cp, p, l)	BUFFset(&(cp)->Out, (p), (l))
#define WCHANappend(cp, p, l)	BUFFappend(&(cp)->Out, (p), (l))


/*
**  Append data to a buffer.
*/
#define BUFFappend(bp_parm, p_parm, l_parm) \
    do { \
	register int	l_; \
	register BUFFER	*bp_; \
	int		i_; \
    \
	if ((l_ = l_parm) != 0) { \
	    bp_ = bp_parm; \
	    /* Note end of buffer, grow it if we need more room. */ \
	    i_ = bp_->Used + bp_->Left; \
	    if (i_ + l_ > bp_->Size) { \
		/* Round size up to next 1K. */ \
		bp_->Size += (l_ + 0x3FF) & ~0x3FF; \
		RENEW(bp_->Data, char, bp_->Size); \
	    } \
	    bp_->Left += l_; \
	    if (l_ > MEMCPY_THRESHOLD) \
		(void)memcpy((POINTER)&bp_->Data[i_], (POINTER)p_parm, (SIZE_T)l_); \
	    else { \
		register STRING	p_; \
		register char	*dest_; \
    \
		for (p_ = p_parm, dest_ = &bp_->Data[i_], l_++; --l_ > 0; ) \
		    *dest_++ = *p_++; \
	    } \
	} \
    } while (JUSTONCE)


/*
**  Mark that an I/O error occurred, and block if we got too many.
*/
#define IOError(WHEN)	\
	if (--ErrorCount <= 0 || errno == ENOSPC) ThrottleIOError(WHEN); else


/*
**  Global data.
*/
#if	defined(DEFINE_DATA)
#define EXTERN		/* NULL */
#else
#define EXTERN		extern
#endif	/* defined(DEFINE_DATA) */
EXTERN BOOL		AlwaysCrosspost;
extern BOOL		AmRoot;
extern BOOL		BufferedLogs;
EXTERN BOOL		AmSlave;
EXTERN BOOL		AnyIncoming;
extern BOOL		Debug;
EXTERN BOOL		ICDneedsetup;
EXTERN BOOL		NeedHeaders;
EXTERN BOOL		NeedOverview;
EXTERN BOOL		NNRPFollows;
extern BOOL		NNRPTracing;
extern BOOL		Tracing;
EXTERN BUFFER		Path;
EXTERN BUFFER		Xref;
EXTERN char		*ModeReason;	/* NNTP reject message		*/
EXTERN char		*NNRPReason;	/* NNRP reject message		*/
EXTERN char		*Reservation;	/* Reserved lock message	*/
EXTERN char		*RejectReason;	/* NNTP reject message		*/
extern char		SPOOL[];
EXTERN char		*Version;
EXTERN FILE		*Errlog;
EXTERN FILE		*Log;
extern char		LogName[];
EXTERN INADDR		MyAddress;
extern int		ErrorCount;
EXTERN int		ICDactivedirty;
extern int		MaxIncoming;
EXTERN int		MaxOutgoing;
EXTERN int		nGroups;
EXTERN int		nSites;
EXTERN int		PROCneedscan;
extern int		SPOOLlen;
EXTERN int		Xrefbase;
extern long		LargestArticle;
EXTERN NEWSGROUP	**GroupPointers;
EXTERN NEWSGROUP	*Groups;
extern OPERATINGMODE	Mode;
EXTERN SIGVAR		GotTerminate;
EXTERN SITE		*Sites;
EXTERN SITE		ME;
EXTERN struct timeval	TimeOut;
EXTERN TIMEINFO		Now;		/* Reasonably accurate time	*/
extern time_t		Cutoff;


/*
**  Function declarations.
*/
extern BOOL		FormatLong();
extern BOOL		MakeSpoolDirectory();
extern BOOL		NeedShell();
extern char		**CommaSplit();
extern char		*MaxLength();
extern int		Spawn();
extern NORETURN		CleanupAndExit();
extern void		FileGlue();
extern void		JustCleanup();
extern void		ThrottleIOError();
extern void		ReopenLog();
extern void		xchown();

extern BOOL		ARTidok();
extern BOOL		ARTreadschema();
extern char		*ARTreadarticle();
extern char		*ARTreadheader();
extern STRING		ARTpost();
extern void		ARTcancel();
extern void		ARTclose();
extern void		ARTsetup();

extern void		BUFFset();

extern BOOL		CHANsleeping();
extern CHANNEL		*CHANcreate();
extern CHANNEL		*CHANiter();
extern CHANNEL		*CHANfromdescriptor();
extern char		*CHANname();
extern int		CHANreadtext();
extern void		CHANclose();
extern void		CHANreadloop();
extern void		CHANsetup();
extern void		CHANtracing();

extern void		RCHANadd();
extern void		RCHANremove();

extern void		SCHANadd();
extern void		SCHANremove();
extern void		SCHANwakeup();

extern BOOL		WCHANflush();
extern void		WCHANadd();
extern void		WCHANfappend();
extern void		WCHANremove();
extern void		WCHANsetfrombuffer();

extern void		CCcopyargv();
extern STRING		CCblock();
extern STRING		CCcancel();
extern STRING		CCcheckfile();

extern BOOL		HIShavearticle();
extern BOOL		HISwrite();
extern char		*HISfilesfor();
extern void		HISclose();
extern void		HISsetup();
extern void		HISsync();

extern BOOL		ICDnewgroup();
extern char		*ICDreadactive();
extern BOOL		ICDchangegroup();
extern void		ICDclose();
extern void		ICDrenumberactive();
extern BOOL		ICDrmgroup();
extern void		ICDsetup();
extern void		ICDwrite();
extern void		ICDwriteactive();

extern void		CCclose();
extern void		CCsetup();

extern void		LCclose();
extern void		LCsetup();

extern char		**NGsplit();
extern NEWSGROUP	*NGfind();
extern CHANNEL		*NCcreate();
extern void		NGparsefile();
extern BOOL		NGrenumber();

extern void		NCclose();
extern void		NCsetup();

extern int		PROCwatch();
extern void		PROCunwatch();
/* extern void		PROCclose(); */
extern void		PROCscan();
extern void		PROCsetup();

extern BOOL		RCnolimit();
extern BOOL		RCauthorized();
extern BOOL		RCcanpost();
extern char		*RChostname();
extern int		RCismaster();
extern void		RCclose();
extern void		RChandoff();
extern void		RCreadlist();
extern void		RCsetup();

extern BOOL		SITEfunnelpatch();
extern BOOL		SITEsetup();
extern BOOL		SITEwantsgroup();
extern char		**SITEreadfile();
extern SITE		*SITEfind();
extern SITE		*SITEfindnext();
extern STRING		SITEparseone();
extern void		SITEchanclose();
extern void		SITEdrop();
extern void		SITEflush();
extern void		SITEflushall();
extern void		SITEforward();
extern void		SITEfree();
extern void		SITElinkall();
extern void		SITEparsefile();
extern void		SITEprocdied();
extern void		SITEsend();
extern void		SITEwrite();
