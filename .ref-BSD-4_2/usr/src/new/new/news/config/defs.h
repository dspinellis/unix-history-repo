/*
 * defs.h - defines for news-related programs.
 *
 * If you remove any lines here or in your Makefile, make the change
 * to localize.sh so you won't have to redo it for each news release.
 *
 * If TMAIL is undefined, the -M option will be disabled.
 *
 * By convention, the version of the software you are running is taken
 * to be news_version below.
 */

static char *news_version = "B 2.10 beta	3/9/83";
/* SCCS ID @(#)defs.dist	2.14	3/9/83 */

#define DAYS	(60L*60L*24L)
#define WEEKS	(7*DAYS)
/* Things that very well may require local configuration */
#ifndef HOME
#define	ROOTID	2163	/* uid of person allowed to cancel anything	*/
#endif
#define	N_UMASK 022	/* mask for umask call, 022 for secure system	*/
#define DFLTEXP	3*WEEKS	/* default no. of seconds to expire in		*/
#define DFLTSUB "general,all.general"	/* default subscription list	*/
#define TMAIL	"/b3/smb/bin/mail"	/* Mail program that understands -T	*/
#define	ADMSUB	"general"	/* Mandatory subscription list		*/
#define PAGE	"/usr/ucb/more"	/* Default pager			*/
#define INEWS	"/usr/lbin/inews" /* inews for recnews to fork	*/
#define FOLLOWUP INEWS	/* What followup commmand should use */
#define RNEWS	"/usr/lbin/rnews" /* rnews for uurec to fork	*/
				/* If you change rnews, see Makefile too*/
#define NOTIFY	"usenet"	/* Tell him about certain ctl messages	*/
				/* Default xmit command - remove -z if	*/
#define DFTXMIT	"uux - -r -n %s!rnews < %s" /* your uux can't do it	*/
#define UXMIT	"uux -r -z -c %s!rnews '<' %s" /* If uux -c is ok	*/
#define DFTEDITOR "/usr/ucb/vi"	/* Default editor, see also postnews.	*/
/* #define UUNAME @@	/* omit for uuname, put in LIBDIR	*/
#define MANUALLY		/* Don't execute rmgroups, just notify.	*/
/* #define BATCH @@	/* Allow batch input: name of unbatcher */
				/* BATCH: SEE SECURITY WARNING IN	*/
				/* ifuncs.c BEFORE YOU TURN THIS ON.	*/
/* #define BERKNAME @@	/* name of local host on Berknet	*/
/* #define LOCALNAME 		/* There is no full name database. 	*/
/* #define INTERNET		/* Internet mail works locally		*/
#define MYDOMAIN ".UUCP"	/* Local domain				*/
#define AUTONEWNG		/* Believe new unrecognized newsgroups	*/
/* #define CHEAP		/* don't chown files to news		*/
#define OLD			/* Add extra headers for old neighbors	*/
#define UNAME		/* If uname call is available.		*/
/* #define GHNAME		/* If gethostname call is available.	*/
#define V7MAIL			/* Local mail format is V7 ("From ")	*/
#define MYORG "Bell Labs, Murray Hill"	/* My organization.  Please	*/
				/* include your city (and state, and	*/
				/* country, if not obvious) in MYORG,	*/
				/* and please keep it short.		*/

/* Things you might want to change */
#define NEWSRC  ".newsrc"	/* name of .newsrc file (in home dir)	*/
#define LINES	512	/* maximum no. of lines in .newsrc		*/
#define NEGCHAR	'!'	/* newsgroup negation character			*/
#define DEADTIME 90	/* no. of seconds to wait on deadlock		*/
#define FMETA	'%'	/* file meta-character for c option		*/
#ifdef pdp11
# define BUFLEN	128	/* standard buffer size				*/
#else
# define BUFLEN	256	/* standard buffer size				*/
#endif
#define LBUFLEN 1024	/* big buffer size				*/ 
#define SYSPATH	"/usr/fbin:/usr/lbin:/bin:/usr/bin"	/* default, secure, vanilla path */
#define LNCNT	16	/* Articles with > LNCNT lines go through pager */

/* Things you probably won't want to change */
#define PATHLEN 512	/* length of longest source string		*/
#define	DATELEN	64	/* length of longest allowed date string	*/
#define	NAMELEN	64	/* length of longest possible message ID	*/
#define	SNLN	8	/* max significant characters in sysname	*/
#define	PROTO	'A'	/* old protocol name				*/
#define NETCHRS	"!:.@^%"	/* Punct. chars used for various networks	*/
#define	TRUE	1	/* boolean true					*/
#define	FALSE	0	/* boolean false				*/
#define NGFSIZ  5000	/* legal newsgroup file size			*/
#define	NGDELIM	','	/* delimit character in news group line		*/
