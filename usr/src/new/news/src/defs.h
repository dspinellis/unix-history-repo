/*
 *	This software is Copyright (c) 1986 by Rick Adams.
 *
 *	Permission is hereby granted to copy, reproduce, redistribute or
 *	otherwise use this software as long as: there is no monetary
 *	profit gained specifically from the use or reproduction or this
 *	software, it is not sold, rented, traded or otherwise marketed, and
 *	this copyright notice is included prominently in any copy
 *	made.
 *
 *	The author make no claims as to the fitness or correctness of
 *	this software for any use whatsoever, and it is provided as is. 
 *	Any use of this software is at the user's own risk.
 *
 */

/*	@(#)defs.dist	2.59	10/15/87	*/

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

#define DAYS	(60L*60L*24L)
#define WEEKS	(7*DAYS)
/* Things that very well may require local configuration */
#ifndef HOME
#define ROOTID	0	/* uid of person allowed to cancel anything	*/
#endif
#define N_UMASK 002	/* mask for umask call, 022 for secure system	*/
#define DFLTEXP	2*WEEKS	/* default no. of seconds to expire in		*/
#define HISTEXP	4*WEEKS	/* default no. of seconds to forget in		*/
#define DFLTSUB "general,all.announce"	/* default subscription list	*/
#define TMAIL	"/usr/ucb/Mail"	/* Mail program that understands -T	*/
#define ADMSUB	"general,all.announce"	/* Mandatory subscription list	*/
#define PAGE	"/usr/ucb/more"	/* Default pager			*/
#define NOTIFY	"usenet"	/* Tell him about certain ctl messages	*/
				/* Default xmit command - remove -z if	*/
#define DFTXMIT	"uux - -r -z -gd %s!rnews < %s" /* your uux can't do it	*/
#define UXMIT	"uux -r -z -gd -c %s!rnews '<' %s" /* If uux -c is ok	*/
#define DFTEDITOR "vi"		/* Default editor, see also postnews.	*/
/* #define UUPROG "euuname"	/* omit for uuname, put in LIBDIR	*/
#define MANUALLY		/* Don't execute rmgroups, just notify.	*/
/* #define NONEWGROUPS		/* Don't create new groups, just notify.*/
/* #define SPOOLNEWS		/* Spool incoming rnews, don't process	*/
/* #define SPOOLINEWS		/* Spool local inews, don't process	*/
/* #define LOCALNAME 		/* There is no full name database. 	*/
#define INTERNET		/* Internet mail works locally		*/
#define MYDOMAIN ".Berkeley.EDU"	/* Local domain				*/
/* #define CHEAP		/* don't chown files to news		*/
/* #define OLD			/* Add extra headers for old neighbors	*/
/* #define UNAME		/* If uname call returns your nodename  */
#define GHNAME		/* If gethostname call is available.	*/
/* #define UUNAME "/etc/uucpname" /* If your nodename is stored in a file */
#define V7MAIL			/* Local mail format is V7 ("From ")	*/
#define SORTACTIVE		/* if you want news presented in the order of the .newsrc */
#define ZAPNOTES		/* if you want old style notes headers moved into the headers */
#define DIGPAGE			/* allow digestifying in vnews */
#define DOXREFS		/* Generate xref line for rn to use */
/* #define MULTICAST		/* If you want to be able to multicast news */
#define BSD4_2		/* If you are running 4.2  or 4.3 BSD	*/
/* #define BSD2_10		/* If you are running 2.10 BSD */
/* #define LOCKF		/* If you have the lockf() sys call */
/* #define DOGETUSER		/* Always do 'getuser' so can't fake name */
/* #define LOGDIR		/* use the 'logdir' call on path lookups */
#define MKDIRSUB		/* your system has mkdir as a syscall */
/* #define READDIR		/* your system has readdir() in libc */
/* #define ALWAYSALIAS		/* temporary kludge for conversion */
#define SENDMAIL "/usr/lib/sendmail" /* command line to run "sendmail" if you have it	*/
/* #define MMDF	"/usr/mmdf/submit"	/* command line to run mmdf if you have it */
#define MYORG "CSRG, UC Berkeley"	/* My organization.  Please	*/
				/* include your city (and state, and	*/
				/* country, if not obvious) in MYORG,	*/
				/* and please keep it short.		*/
/* #define HIDDENNET "frooz"	/* if you have a local network and want */
				/* The mail address to look like it came */
				/* from one machine */
/* NOTE: The following two macros replace the use of HIDDENNET */
/* #define GENERICPATH "frooz"	/* If you are using a shared USENET/UUCP node */
/* #define GENERICFROM "Frobozz.COM"	/* If you want generic From:-addresses */
/* #define NICENESS	4	/* does a nice(NICENESS) in rnews */
/* #define FASCIST	"all,!all.all"	/* only permit posting to certain groups */
				/* see installation guide for details */
/* #define SMALL_ADDRESS_SPACE	/* If your machine can't address > 32767 */
/* #define ORGDISTRIB	"froozum"	/* For organization wide control message handling */

/* Things you might want to change */
#define NEWSRC  ".newsrc"	/* name of .newsrc file (in home dir)	*/
#define LINES	512	/* maximum no. of lines in .newsrc		*/
#define NEGCHAR	'!'	/* newsgroup negation character			*/
#define DEADTIME 45	/* no. of seconds to wait on deadlock		*/
#define FMETA	'%'	/* file meta-character for c option		*/
#if defined(pdp11) || defined(SMALL_ADDRESS_SPACE)
# define BUFLEN	128	/* standard buffer size				*/
#else
# define BUFLEN	256	/* standard buffer size				*/
#endif
#define LBUFLEN 1024	/* big buffer size				*/
#define SBUFLEN 32	/* small buffer size (for system names, etc)	*/
#define LNCNT	14	/* Articles with > LNCNT lines go through pager */

/* Things you probably won't want to change */
#define PATHLEN 512	/* length of longest source string		*/
#define DATELEN	64	/* length of longest allowed date string	*/
#define NAMELEN	128	/* length of longest possible message ID	*/
#define SNLN	8	/* max significant characters in sysname	*/
#define PROTO	'A'	/* old protocol name				*/
#define NETCHRS	"!:@^%,"/* Punct. chars used for various networks	*/
#define TRUE	1	/* boolean true					*/
#define FALSE	0	/* boolean false				*/
#define PERHAPS	2	/* indeterminate boolean value			*/
#define NGDELIM	','	/* delimit character in news group line		*/

/* for NNTP */
#ifdef SERVER
# include "/usr/src/new/nntp/common/response_codes.h"
# define SERVER_FILE "/usr/new/lib/news/server"
#endif /* SERVER */

#ifdef M_XENIX
#define index	strchr
#define rindex	strrchr
#define	vfork	fork
#endif /* M_XENIX */
