/*	structure definitions for the notefile program */

#include <stdio.h>
#include <setjmp.h>

#ifdef BSD4.1c
#include <sys/file.h>
#define	setuid(x)	setreuid(-1,x)
#endif BSD4.1c

#ifdef	VFORK
#define	fork	vfork
#endif

#define		NETLOG	"net.log"	/* log file for network */
#define		GRIPES	"nfgripes"	/* name of gripe notefile */
#define		UTILITY	".utilities"	/* subdirectory containing utilities */
#define		LOCKS	".locks"	/* lock directory */
#define		SEQUENCER ".sequencer"	/* where sequencer files live */
#define		SEQ	".SEQ"		/* next d_nfnum available */
#define		INDXHLP	"index.help"	/* name of index page help */
#define		RDMHLP	"read.help"	/* name of readem page help file */
#define		LIMHLP	"lim.help"	/* help for limited index */
#define		ACCHLP	"access.help"	/* for access editor */
#define		DIRHLP	"dir.help"	/* for director options */
#define		AVAILHLP "avail.notes"	/* list of public notefiles */
#define		NFCOMMENT "nfcomment"	/* nf comment routine */

#define		SEQLOCK 's'			/* lock flags */
#define		LOGLOCK 'l'

#define		TEXT	"text"		/* name of text file */
#define		INDEXN	"note.indx"	/* master index */
#define		INDEXR	"resp.indx"	/* response chains */
#define		ACCESS	"access"	/* access lists in here */
#define		RESPSZ	5		/* number responses/response record */
#define		NAMESZ	25		/* max number of characters in name */
#define		SYSSZ	10		/* max system name length */
					/* must hold null terminator also */
#define		BUFSIZE 512		/* chars in core */
#ifdef BSD4.1c
#define		MAXMSG	4000000000	/* longest allowed note */
#else
#define		MAXMSG	65535		/* longest allowed note */
#endif BSD4.1c
#define		PAGESAV	10		/* display stack */
					/* using unsigned short */
#define		UNIQPLEX 100000		/* multiplex nfid into noteid */
					/* see putnote and putresp */

#define		TITLEN	36		/* note title length */
#ifdef BSD4.1c
#define		NNLEN	255		/* Notesfile Name length */
#else
#define		NNLEN	40		/* Notesfile Name length */
#endif BSD4.1c
#define		DMLEN	40		/* director message length */
#define		NINDEX	10		/* how many notes on directory page */
#define		NOT	~		/* tilde, prints wrong on hazeltines */
#define		WDLEN	255		/* longest file name length */
#ifdef BSD4.1c
#define		CMDLEN  1024		/* command length */
#else
#define		CMDLEN  512		/* command length */
#endif BSD4.1c
#define		MAXGROUPS 300		/* maximum "globbed" notesfiles */
#define		PASSWDLEN 128		/* longest line in /etc/passwd */
#define		DATELEN	24		/* length of formatted date */
#define		NPERMS	35		/* max access list size */
/*
 *	NPERMS can be increased as desired. However, a static array is
 *	allocated based on this constant. Watch it if you are running in
 *	a memory starved environment (like an 11/60).
 */
#define		PAGELEN	56		/* pagelength for 'S' */

#define		ANONOK	   01		/* permit anon notes */
#define		FRMNEWS    01		/* non-nf article */
#define		OPEN	   02		/* open for public */
#define		DIRMES	   04		/* director msg on */
#define		DELETED	   010		/* is deleted */
#define		NFINVALID  010		/* got bad copy */
#define		NETWRKD	   020		/* networking OK */
#define		CONTINUED  040		/* was auto-split */
#define		WRITONLY   0100		/* writeonly access when written */
#define		ORPHND	   0200		/* foster parent */

/*	change these only after modifying the table in access.c		*/
#define		READOK	01		/* allow user to read */
#define		WRITOK	02		/* allow user to write */
#define		DRCTOK	04		/* allow user to be director */
#define		RESPOK  010		/* ok to respond */
#define		DFLTPERMS (READOK+WRITOK+RESPOK) /* default permissions */

#define		PUSER	00		/* for perm_f - entry type */
#define		PGROUP	01		/* ORDER IS IMPORTANT */
#define		PSYSTEM	02

#define		NOSEQ		0	/* no sequencer */
#define		NORMSEQ		1	/* normal sequencer */
#define		EXTSEQ		2	/* enter anyway sequencer */
#define		INDXSEQ		3	/* index sequencer */
					/*  from harpo!mmp */

#define		QUITSEQ		-2	/* quit, update sequencer */
#define		QUITNOSEQ	-3	/* quit, no seq update */
#define		QUITFAST	-4	/* quit almost abort */
#define		QUITUPD  	-5      /* update seq and exit, RLS */
#define		QUITNEX		-6	/* no such notesfile */
#define		QUITBAD		-7	/* error opening notesfile */

#define		GOOD		0	/* good exit status */
#define		BAD		1	/* bad exit status */
#define		NONF		2	/* no notefile */

#define		POLICY		1	/* mnemonics */
#define		NOPOLICY	0	/* used in calls */
#define		LOCKIT		1	/* Do not change */
#define		NOLOCKIT	0
#define		COPYID		1
#define		NOCOPYID	0
#define		ADDID		1
#define		NOADDID		0
#define		ADDTIME		1
#define		NOADDTIME	0
#define		DETAIL		1	/* dump extra info */
#define		NODETAIL	0	/* used for generic form */

/*
 *	These defines are for the archiver. They are used for determining
 *	eligibility for archival along with the age of the note.
 */
#define		DIRNOCARE	0	/* don't care about dir msgs */
#define		DIRON		1	/* archive only if on */
#define		DIROFF		2	/* only if off */

struct auth_f				/* how we id author */
{
    char    aname[NAMESZ];		/* author name */
    int     aid;			/* author's uid */
};

struct when_f				/* standard date structure */
{
    short w_year;
    short w_month;
    short w_day;
    short w_hours;
    short w_mins;
};

struct id_f				/* unique id for notes */
{
    char    sys[SYSSZ];
            long uniqid;
};

struct perm_f {				/* permission tables */
    short ptype;			/* user, group, system */
    char    name[NAMESZ];		/* name of such */
            short perms;		/* what he is allowed */
};

struct txthead_f			/* way note text stored */
{
    short note_no;	/* to which note this text belongs (0=policy) */
    short resp_no;	/* to which response this text belongs (0=main note) */
					/* both for debugging */
#ifdef BSD4.1c
    unsigned long textlen;		/* how long the text is */
#else
    unsigned short textlen;		/* how long the text is */
#endif BSD4.1c
};

struct txtbuf_f
{
    char    txtbuf[BUFSIZE];		/* hold a bunch of characters */
};

struct dsply_f
{
    struct txthead_f    d_head;			/* text start */
    struct txtbuf_f d_buf;
    int     optr,
            olim;			/* output index and end of buffer */
            long outcount;		/* number of characters dumped */
};

struct daddr_f					/* save a disk address */
{
    long addr;					/* for lseeks */
};

struct resp_f					/* for each response: */
{
    struct id_f r_id[RESPSZ];		/* system/id for each response */
    struct daddr_f  r_addr[RESPSZ];	/* where the response is */
    struct when_f   r_when[RESPSZ];	/* date/time of response */
    char    r_from[RESPSZ][SYSSZ];	/* system that sent this to us */
    struct when_f   r_rcvd[RESPSZ];	/* date/time for sequencer */
    struct auth_f   r_auth[RESPSZ];
    char    r_stat[RESPSZ];		/* director/status flag */
    int     r_next;			/* index of next response_ind */
};

struct note_f				/* standard note structure: */
{
    struct id_f n_id;			/* unique id for this note */
                short n_nresp;		/* number of responses */
    char    ntitle[TITLEN];		/* title of note */
    struct auth_f   n_auth;		/* note's author */
    struct when_f   n_date;		/* note's date */
    struct when_f   n_rcvd;		/* date we got it */
    struct when_f   n_lmod;		/* date of last mod */
    char    n_from[SYSSZ];		/* system that handed us the note */
    int     n_rindx;		/* where the first set of responses lies */
    struct daddr_f  n_addr;		/* address of note's text on disk */
    char    n_stat;			/* director/status flag */
};

struct descr_f				/* for the notesfile: */
{
    char    d_title[NNLEN];		/* name of the notesfile */
    char    d_drmes[DMLEN];		/* director message */
            short d_plcy;		/* ==0 if no message */
    struct when_f   d_lastm;		/* last modified time */
                    short d_stat;	/* open/closed/etc */
                    short d_nnote;	/* how many notes in file */
    struct id_f d_id;			/* sys name & unique id counter */
    struct when_f   d_lstxmit;		/* last network transmit */
    struct when_f   d_created;		/* creation date */
    struct when_f   d_lastuse;		/* last day used */
                    long d_daysused;	/* count those days */
                    long d_rspwrit;	/* number of responses ever written */
                    long d_notwrit;	/* number of notes ever written */
                    long entries;  /* number of entries into the notefile */
                    long walltime;	/* man-seconds (?) spent in notefile */
                    long d_rspread;	/* number of responses read */
                    long d_notread;	/* and number of notes */
                    long d_rsprcvd;	/* network in stats */
                    long d_notrcvd;
                    long d_rspxmit;	/* network out stats */
                    long d_notxmit;
                    long d_notdrop;	/* duplicate notes recieved */
                    long d_rspdrop;	/* and dropped on ground */
                    long d_orphans;	/* orphaned responses */
                    long netwrkouts;	/* number of times networked out */
                    long netwrkins;	/* and number of networked in */
                    short d_nfnum;	/* unique to this notesfile */
    char    d_filler[38];		/* bytes for future use ... */
};

struct io_f				/* master i/o form */
{
    int     fidtxt;			/* text */
    int     fidndx;			/* note.indx */
    int     fidrdx;			/* resp.indx */
    /* this notefile's descriptor, updated by critical sections */
    struct  descr_f  descr;
    char    nf[NNLEN];			/* which notefile he is in */
    char    xstring[TITLEN + 1];	/* search string */
    char    xasys[SYSSZ];		/* author search sys */
    char    xaname[NAMESZ];		/* author search name */
    struct  when_f   stime;   /* read notes/responses more recent than this */
    short   access;		/* what sort of access user has */
    int     nrspwrit;		/* number of responses written this entry */
    int     nnotwrit;		/* num of notes written */
    long    entered;		/* when started so can figure time in */
    int     nrspread;		/* how many responses he read */
    int     nnotread;		/* how many notes he read */
				/* num read may be tough */
    int     nnotxmit;		/* network out stats */
    int     nrspxmit;
    int     nnotrcvd;		/* network in stats */
    int     nrsprcvd;
    int     nnotdrop;		/* duplicates rom the network */
    int     nrspdrop;
    int     norphans;		/* orphaned responses */
};

struct seq_f			/* sequencer entry list form */
{
    char    nfname[NNLEN];	/* name of notefile */
    struct when_f   lastin;	/* last entry time */
};

struct fbuf_f			/* buffered character input */
{
    int     fid,		/* file desriptor */
            iptr,		/* current pointer */
            ilim;		/* end of buffer */
    char    buff[512];		/* the buffer */
};

struct grp {
    char *name;
    char lookat;
    char seqtyp;
};

extern char *SYSTEM;		/* points at system name */
extern int  intflag;		/* flag DEL's */
char   *sprintf();		/* satisfy lint */
char   *getenv();		/* remove ugly warnings */
char   *strsave();
long lseek();			/* clean up lint and other stuff */
extern int  globuid;		/* point at the true uid */
extern int  nrows, ncols;	/* screen size */

extern char *notesrc;		/* notesrc file name, RLS */
extern int  nindex;		/* number of index lines, RLS */
extern struct grp group[];
extern int last_group;

extern int ignsigs;
extern int replot;

struct notesenv {
	jmp_buf	n_env;
};

extern struct notesenv curenv;
#define	jenv	curenv.n_env

extern int msk;
