#include <stdio.h>

#if	defined(RCSIDENT) && defined(MAINLINE)
static char zzstructs[] = "$Header: structs.h,v 1.7.0.1 85/03/17 20:57:42 notes Rel $";
#endif	defined(RCSIDENT) && defined(MAINLINE)

/*
 *	structure definitions for the notesfile program.
 *
 *	Constants/definitions likely to change with different
 *	kernels are included in the file "parms.h".
 */


#define	TRUE	1					/* pretty euphemisms */
#define	FALSE	0
#define	NEVER	(-1)					/* expiration stuff */
#define	EDIT	TRUE					/* whether to edit or */
#define	NOEDIT	FALSE					/* not to edit */

#ifdef	UID8						/* 8 bit uids */
#define		UIDMASK 0377				/* mask 8 bit uids */
#define		GIDMASK 0377				/* mask 8 bit gids */
#else	! defined(UID8)					/* 16 bit uids */
#define		UIDMASK	0177777				/* mask out high UID bits */
#define		GIDMASK 0177777				/* mask out high GID bits */
#endif	! defined(UID8)

#define		NETLOG	"net.log"			/* network logfile */
#define		GRIPES	"nfgripes"			/* gripe notesfile */
#define		UTILITY	".utilities"			/* utility directory */
#define		LOCKS	".locks"			/* lock directory */
#define		SEQUENCER ".sequencer"			/* sequencer files */
#define		SEQ	".SEQ"				/* next d_nfnum */
#define		INDXHLP	"index.help"			/* index page help */
#define		RDMHLP	"read.help"			/* readem page help file */
#define		LIMHLP	"lim.help"			/* help for limited index */
#define		ACCHLP	"access.help"			/* for access editor */
#define		DIRHLP	"dir.help"			/* for director options */
#define		AVAILHLP "avail.notes"			/* list of public notefiles */
#define		NFCOMMENT "nfcomment"			/* nfcomment routine */
#define		ARCHALIAS "Archive-into"		/* archive mapping */

#define		TEXT	"text"				/* name of text file */
#define		INDEXN	"note.indx"			/* master index */
#define		INDEXR	"resp.indx"			/* response chains */
#define		ACCESS	"access"			/* access lists here */
#define		ARCHIVE	"archive"			/* archive sub-dir */
#define		COMPRESS "comp."			/* compress names */
#define		SEQLOCK		's'			/* .SEQ lock */
#define		LOGLOCK		'l'			/* log file locking */
#define		ARCHLOCK	'a'			/* archiving */
#define		TXTLOCK		't'			/* text file */
#define		DSCRLOCK 	'n'			/* index file */

#define		RESPSZ	5				/* number responses/response record */
#define		NAMESZ	17				/* longest user name */
#define		HOMESYSSZ	33			/* user home system */
#define		SYSSZ	33				/* system name length */
							/* above hold null byte also */
#define		BUFSIZE 1024				/* chars in core */

/*
 *	pick an appropriate default for the maximum message length
 *	(MAXMSG). Also pick an appropriate hard limit. (HARDMAX)
 *	HARDMAX must leave room for a one line message of up to 50
 *	bytes of the form "ignoring %ld excess bytes"
 */
#ifdef	BIGTEXT
#define		MAXMSG	100000				/* can be larger */
#define		HARDMAX	3000000				/* 3 Mbytes */
#else	NOT BIGTEXT
#define		MAXMSG	65000				/* not much larger */
#define		HARDMAX	65000				/* room for mesage */
#endif	NOT BIGTEXT
#define		PAGESAV	50				/* display stack */
							/* using unsigned short */
#define		UNIQPLEX 100000				/* multiplex nfid into noteid */
							/* see putnote and putresp */

#define		TITLEN	36				/* note title length */
#define		NNLEN	40				/* Notesfile Name length */
#define		DMLEN	40				/* director message length */
#define		NOT	~				/* tilde, prints wrong on hazeltines */
#define		WDLEN	128				/* longest file name */
#define		CMDLEN	512				/* build-a-command */
#define		PASSWDLEN 128				/* longest line in /etc/passwd */
#define		DATELEN	24				/* length of formatted date */
#define		NPERMS	35				/* max access list */
/*
 *	NPERMS can be increased as desired. However, a static array is
 *	allocated based on this constant. Watch it if you are running in
 *	a memory starved environment (like an 11/60).
 */
#define		PAGELEN	56				/* pagelength for 'S' */

#define		ANONOK	   01				/* permit anon notes */
#define		FRMNEWS    01				/* non-nf article */
#define		OPEN	   02				/* open for public */
#define		DIRMES	   04				/* director msg on */
#define		DELETED	   010				/* is deleted */
#define		NFINVALID  010				/* got bad copy */
#define		NETWRKD	   020				/* networking OK */
#define		CONTINUED  040				/* was auto-split */
#define		WRITONLY   0100				/* writeonly access when written */
#define		ORPHND	   0200				/* foster parent */
#define		ISARCH	   0400				/* is an archive */

/*	change these only after modifying the table in access.c		*/
#define		READOK	01				/* allow user to read */
#define		WRITOK	02				/* allow user to write */
#define		DRCTOK	04				/* allow user to be director */
#define		RESPOK  010				/* ok to respond */
/*
 *	archive writing keyed on director status for now...
 */
#define		ARCHWRITOK  020				/* archive write */
#define		ARCHRESPOK 040				/* archive response */
#define		DFLTPERMS (READOK+WRITOK+RESPOK)	/* default permissions */

#define		PERMUSER	00			/* perm_f entry type */
#define		PERMGROUP	01			/* ORDER IS IMPORTANT */
#define		PERMSYSTEM	02


/*
 *	Sequencer modes.
 *	modes < NOREADSEQ cause the time to be taken from "Basetime"
 *	modes < NOWRITESEQ cause no update when leaving the notesfile.
 *	There is currently no way to read from the sequencer file
 *		and not update -- it's a wierd case anyway.
 */

#define		NOSEQ		0			/* no sequencer */
#define		USERSEQ		1			/* usertime noupdate */

#define		NOWRITESEQ	10			/* < this no write */
#define		BASESEQ		11			/* usertime & update */

#define		NOREADSEQ	100			/* use Basetime if < */
#define		NORMSEQ		101			/* normal sequencer */
#define		EXTSEQ		102			/* enter anyway */
#define		INDXSEQ		103			/* index sequencer */
							/*  from harpo!mmp */

#define		QUITSEQ		-2			/* quit, update sequencer */
#define		QUITNOSEQ	-3			/* quit, no seq update */
#define		QUITFAST	-4			/* quit almost abort */
#define		QUITUPD		-5			/* like quitfast */
#define		QUITNEX		-6			/* no notesfile */
#define		QUITBAD		-7			/* bad notesfile */

#define		GOOD		0			/* good exit status */
#define		BAD		1			/* bad exit status */
#define		NONF		2			/* no notefile */

#define		POLICY		1			/* mnemonics */
#define		NOPOLICY	0			/* used in calls */
#define		LOCKIT		1			/* Do not change */
#define		NOLOCKIT	0
#define		COPYID		1
#define		NOCOPYID	0
#define		ADDID		1
#define		NOADDID		0
#define		ADDTIME		1
#define		NOADDTIME	0
#define		DETAIL		1			/* dump extra info */
#define		NODETAIL	0			/* used for generic form */

/*
 *	These defines are for the archiver. They are used for determining
 *	eligibility for archival along with the age of the note.
 *	DFLT means to use whatever value was supplied on the archive
 *	command line.  The others mean the notesfile is configured
 *	for a specific archival setup.
 */

#define		DIRDFLT		0			/* use cmd line */
#define		DIRNOCARE	1			/* don't check dir */
#define		DIRON		2			/* only if dir on */
#define		DIROFF		3			/* only if dir off */

#define		KEEPDFLT	0			/* use cmd line */
#define		KEEPNO		1			/* delete 'em */
#define		KEEPYES		2			/* archive 'em */

struct auth_f						/* how we id author */
{
    char    aname[NAMESZ];				/* his name */
    char    asystem[HOMESYSSZ];				/* his system */
    int     aid;					/* uid (if local) */
};

struct when_f						/* standard date structure */
{
    short   w_year;
    short   w_month;
    short   w_day;
    short   w_hours;
    short   w_mins;
    long    w_gmttime;					/* stock unix time */
};

struct id_f						/* unique id for notes */
{
    char    sys[SYSSZ];
    long    uniqid;
};

struct perm_f						/* permission tables */
{
    short   ptype;					/* user, group, system */
    char    name[NAMESZ];				/* name of such */
    short   perms;					/* what he is allowed */
};

struct daddr_f						/* save a disk address */
{
    long    addr;					/* for lseeks */
#ifdef	BIGTEXT
    unsigned long   textlen;				/* how long is text */
#else	NOT BIGTEXT
    unsigned short  textlen;				/* how long the text is */
#endif	NOT BIGTEXT
};

struct txtbuf_f
{
    char    txtbuf[BUFSIZE];				/* hold a bunch of characters */
};

struct dsply_f
{
    struct daddr_f  d_head;				/* text start */
    struct txtbuf_f d_buf;
    int     optr,
            olim;					/* output index and end of buffer */
    long    outcount;					/* number of characters dumped */
};

struct resp_f						/* for each response: */
{
    short   r_first,					/* bounds of this */
            r_last;					/* resp_f block */
    struct id_f r_id[RESPSZ];				/* system/id for each response */
    struct daddr_f  r_addr[RESPSZ];			/* where the response is */
    struct when_f   r_when[RESPSZ];			/* date/time of response */
    char    r_from[RESPSZ][SYSSZ];			/* system that sent this to us */
    struct when_f   r_rcvd[RESPSZ];			/* date/time for sequencer */
    struct auth_f   r_auth[RESPSZ];
    char    r_stat[RESPSZ];				/* director/status flag */
    int     r_next;					/* index of next response_ind */
    int     r_previous;					/* backlinks */
							/* [currently unused */
};

struct note_f						/* standard note structure: */
{
    struct id_f n_id;					/* unique id for this note */
    short   n_nresp;					/* number of responses */
    char    ntitle[TITLEN];				/* title of note */
    struct auth_f   n_auth;				/* note's author */
    struct when_f   n_date;				/* note's date */
    struct when_f   n_rcvd;				/* date we got it */
    struct when_f   n_lmod;				/* date of last mod */
    char    n_from[SYSSZ];				/* system that handed us the note */
    int     n_rindx;					/* where the first set of responses lies */
    struct daddr_f  n_addr;				/* address of note's text on disk */
    char    n_stat;					/* director/status flag */
};

struct descr_f						/* for the notesfile: */
{
    long    d_format;					/* nf's format */
    char    d_title[NNLEN];				/* nf's name */
    char    d_drmes[DMLEN];				/* director message */
    short   d_plcy;					/* ==0 if no message */
    struct when_f   d_lastm;				/* last modified time */
    short   d_stat;					/* open/closed/etc */
    short   d_nnote;					/* how many notes in file */
    struct id_f d_id;					/* sys name & unique id counter */
    struct when_f   d_lstxmit;				/* last network transmit */
    struct when_f   d_created;				/* creation date */
    struct when_f   d_lastuse;				/* last day used */
    long    d_daysused;					/* count those days */
    long    d_rspwrit;					/* number of responses ever written */
    long    d_notwrit;					/* number of notes ever written */
    long    entries;					/* number of entries into the notefile */
    long    walltime;					/* man-seconds (?) spent in notefile */
    long    d_rspread;					/* number of responses read */
    long    d_notread;					/* and number of notes */
    long    d_rsprcvd;					/* network in stats */
    long    d_notrcvd;
    long    d_rspxmit;					/* network out stats */
    long    d_notxmit;
    long    d_notdrop;					/* duplicate notes recieved */
    long    d_rspdrop;					/* and dropped on ground */
    long    d_orphans;					/* orphaned responses */
    long    netwrkouts;					/* number of times networked out */
    long    netwrkins;					/* and number of networked in */
    short   d_nfnum;					/* unique to this notesfile */
    long    d_archtime;					/* archive after X days */
    long    d_workset;					/* working set size */
    long    d_delnote;					/* count deletes */
    long    d_delresp;					/* count resp dels */
    long    d_dmesgstat;				/* use dirmsg for archive */
    long    d_archkeep;					/* keep/delete */
    long    d_adopted;					/* orphans adopted */
    long    d_longnote;					/* max per article */
    char    d_filler[20];				/* future use ... */
};

struct io_f						/* master i/o form */
{
    int     fidtxt;					/* text */
    int     fidndx;					/* note.indx */
    int     fidrdx;					/* resp.indx */
    struct descr_f  descr;				/* current descr */
							/* updated by critical sections */
    char    nf[NNLEN];					/* last part of name */
    char    basedir[WDLEN];				/* its directory */
    char    fullname[WDLEN];				/* full pathname */
    char    xstring[TITLEN + 1];			/* search string */
    char    xauthor[NAMESZ + SYSSZ + 2];		/* author search */
							/* site!user\0 */
    struct when_f   stime;				/* read notes/responses more recent than this */
    short   access;					/* what sort of access user has */
    int     nrspwrit;					/* number of responses written this entry */
    int     nnotwrit;					/* num of notes written */
    long    entered;					/* when started so can figure time in */
    int     nrspread;					/* how many responses he read */
    int     nnotread;					/* how many notes he read */
							/* num read may be tough */
    int     nnotxmit;					/* network out stats */
    int     nrspxmit;
    int     nnotrcvd;					/* network in stats */
    int     nrsprcvd;
    int     nnotdrop;					/* duplicates rom the network */
    int     nrspdrop;
    int     norphans;					/* orphans rcvd */
    int     adopted;					/* adoptions handled */
};

struct seq_f						/* sequencer entry list form */
{
    char    nfname[NNLEN];				/* name of notefile */
    struct when_f   lastin;				/* last entry time */
};

struct nflist_f						/* nf's to scan */
{
    char   *nf_name;
    short   nf_active;					/* !'ed or not */
    short   nf_seqmode;					/* sequencer mode */
};

/*
 *	Declare global variables. The actual instantiation of these 
 *	variables is in the file startup.c
 *
 */

extern char *hised;					/* preferred editor */
extern char *hisshell;					/* preferred shell */
extern char *hispager;					/* paging program */
extern char *hismailer;					/* mail program */
extern int  nrows;					/* rows on screen */
extern int  ncols;					/* screen width */
extern char *histty;					/* tty on command */
extern int  intflag;					/* DEL hit recently */
extern int  globuid;					/* his true user id */
extern int  Notesuid;					/* who's god */
extern int  Anonuid;					/* who's not allowed */
extern int  Nindex;					/* index page rows */
extern int  ignoresigs;					/* critical section */
extern char *Mstdir;					/* default nf place */
extern char *System;					/* point to it */
extern char *Authsystem;				/* author's system */
extern char *Invokedas;					/* argv[0] */
extern char Seqname[];					/* sequencing name */
extern struct when_f    Basetime;			/* zero time */

/*
 *	Various definitions that help keep things portable.
 *	Types that various functions return, etc.
 */

#if	defined(USG)
extern char *strchr ();					/* UNIX4.0 index() */
extern char *strrchr ();				/* UNIX4.0 rindex() */
#else
extern char *index ();					/* for lint */
extern char *rindex ();
#endif	defined(USG)

/*
 *	Standard library routines that return other than "int".
 */

extern int  errno;					/* syscall errors */
extern char *sys_errlist[];				/* errno messages */
extern int  sys_nerr;					/* and how many */
char   *sprintf ();					/* satisfy lint */
char   *getenv ();
long    lseek ();					/* for lint */

/*
 *	routines in the notesfile system that return other than
 *	"int".
 */

extern char *strsave ();				/* in misc.c */
extern struct nflist_f *nextgroup ();			/* in expand.c */
extern long pagein ();					/* in pagein.c */
extern long gettext ();					/* in gtext.c */
extern long puttrec ();					/* in recsio.c */
