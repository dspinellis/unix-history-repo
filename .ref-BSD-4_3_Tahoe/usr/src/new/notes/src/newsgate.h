#if	defined(RCSIDENT) && defined(MAINLINE)
static char *zznewsgate = "$Header: newsgate.h,v 1.7.0.2 85/07/20 13:44:37 notes Rel $";
#endif	defined(RCSIDENT) && defined(MAINLINE)

/*
 *	newsgate.h
 *
 *	This file contains the definitions needed for the notesfile/news
 *	gateway features.
 *
 *	The new (April 4) implementation allows gateing from news to 
 *	notesfiles at ANY site. Gateing from notes to news is allowed 
 *	by a SINGLE site (for any given newsgroup) per notesfile 'subnet'.
 *
 *	An example:
 *	4 sites running notesfiles: A,B,C and D.
 *
 *	Both A and B run news alongside the notesfile program.
 *	A and B both gateway news into notesfiles; this is safe
 *	and does not generate duplicate articles.
 *	Neither C nor D run news; they do want the articles they
 *	have written to go to the news system beyond A and B.
 *	Sites A and B should at least gateway their locally generated
 *	articles to news.  Sites C and D should each make their own
 *	arrangements to have their articles passed to the news
 *	networks.  Good criteria for choosing the site include
 *	the load of the machine to gateway, how well it is connected
 *	to the rest of the network and who runs the machine.
 *	In this case, suppose C goes with A and D goes with B.
 *	The gateway scripts will look something like:
 *
 *	On A:
 *		newsoutput list			(local articles (A))
 *		newsoutput -sC list		(C's articles)
 *
 *	On B:
 *		newsoutput list			(local articles (B))
 *		newsoutput -sD list		(D's articles)
 *
 *
 *	Original Coding:	Ray Essick	April 4, 1982
 *
 */

/*
 *	Definitions that are likely to change between sites:
 */

#define		DFLTRNEWS		"rnews"		/* if getnet fails */
#undef		EXPANDPATH				/* do indirect links */
#define		DFLTDOMAIN	"UUCP"			/* if undomained */
#define		PATHMAP		"/usr/local/lib/usemap"	/* path to the paths */
							/* pathmap unused. */

/*
 *	definitions that aren't likely to be changed.
 */

#define	UGLYPATH					/* want Path: ....!author */

#define RNEWSNAME	"Usenet"			/* getnet pseudohost */
#define	NEWSSYS		"Anysystem"			/* news' sequencer */
#define	NEWSSEQ		"News"				/* prefix for above */
#define	NEWSALIAS	"newsgroups"			/* mapping file */
#define	NGRPLEN		30				/* longest newsgroup */
#define	NEWSNF		1				/* alias news->nf */
#define	NFBASENEWS	2				/* alias nf->news */
#define	NFRESPNEWS	3				/* alias nf->news */
#define	OLDSUFFIX	"- from a notesfile"		/* catch loops */
#define	NFSUFFIX	"- (nf)"			/* catch nf-news-nf */
#define	CTL		".ctl"				/* control msgs */
#define	SLEEPTIME	10				/* between rnews' */
#define	GATEMAX		100				/* for newsoutput -c */
#define	AUTHORPLACEHOLDER	"seefromline"		/* for Path: line */

/*
 *	classes to send back
 */

#define		NEWS_ALLSEND	001			/* all non-local */
#define		NEWS_LOCALSEND	002			/* local articles */


/*
 *	stuff to be compatible with Salkind/Spickelmier work
 *	Most of these are carried over from the news system.
 */

#define	NGDELIM		','
#define	NEGCHAR		'!'				/* negation */
#define	PROTO		'A'				/* A-news protocol */
#define	NUMGROUPS	5

/*
 *	Definitions for the New and Improved version thanks to
 *	Tw Cook of Hewlett-Packard.
 */

#define		NFLINE1	"Nf-ID"				/* parent, etc */
#define		NFLINE2	"Nf-From"			/* author,dates */


extern char *tzone ();					/* in newsdump.c */
extern char *getpath ();				/* in newspath.c */
extern char *Version;					/* in newsdump.c */
extern char rnewscmd[];					/* in newsoutput.c */
extern int  sendclass;					/* classes to send */

/*
 *	The following lines are from the B-news program.
 *
 * header.h - Article header format
 *
 *	@(#)header.h	2.11	4/24/83";
 */

#define		NUNREC 		50			/* bnews:header.h */
#define		BUFLEN		256			/* bnews:defs.h */
#define		PATHLEN		512			/* bnews:defs.h */
#define		NAMELEN		64			/* bnews:defs.h */
#define		NETCHRS		"!:.@^%"		/* bnews:defs.h */
#define		LBUFLEN		1024			/* bnews:defs.h */
#define		BDATE		64			/* bnews:defs.h */
							/* was DATELEN */

struct hbuf						/* article header */
{							/* bnews:header.h */
    char    from[BUFLEN];				/* From: */
    char    path[PATHLEN];				/* Path: */
    char    nbuf[BUFLEN];				/* Newsgroups: */
    char    snbuf[BUFLEN];				/* Squashed nbuf. */
    char    title[BUFLEN];				/* Subject: */
    char    oident[BUFLEN];				/* Article-I.D.: */
    char    ident[BUFLEN];				/* Message-ID: */
    char    replyto[BUFLEN];				/* Reply-To: */
    char    followid[BUFLEN];				/* References: */
    char    subdate[BDATE];				/* (submitted) Date: */
    long    subtime;					/* subdate in secs */
    char    recdate[BDATE];				/* Date-Received: */
    long    rectime;					/* recdate in secs */
    char    expdate[BDATE];				/* Expires: */
    long    exptime;					/* expdate in secs */
    char    ctlmsg[PATHLEN];				/* Control: */
    char    sender[BUFLEN];				/* Sender: */
    char    followto[BUFLEN];				/* Followup-to: */
    char    postversion[BUFLEN];			/* Post-Version: */
    char    relayversion[BUFLEN];			/* Relay-Version: */
    char    distribution[BUFLEN];			/* Distribution: */
    char    organization[BUFLEN];			/* Organization: */
    char    numlines[8];				/* Lines: */
    int     intnumlines;				/* Integer version */
    char    keywords[BUFLEN];				/* Keywords: */
    char    approved[BUFLEN];				/* Approved: */
    char    nline1[BUFLEN];				/* NF-Id: */
    char    nline2[BUFLEN];				/* NF-From: */
    char   *unrec[NUNREC];				/* unrecognized lines */
};
