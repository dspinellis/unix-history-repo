#
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
 *	4 sites at U. of I. running notesfiles: A,B,C and D.
 *
 *	A and B also interface to news and the rest of the network.
 *	C and D run notesfiles only, A and B run news in parallel.
 *	A is the specified gateway from notesfiles to news. 
 *	Both A and B can gateway from news to notesfiles, without
 *	fear of duplication.
 *
 *	So, for any given 'subnet' of notesfiles, count the N sites that
 *	also run news. One of these N 'parallel' sites should gate from
 *	notesfiles to news. It should (and the other N-1 if they desire)
 *	run gateways from news to notesfiles.
 *
 *	Original Coding:	Ray Essick	April 4, 1982
 */


#define	NEWSSYS		"News:system"		/* news' sequencer name */
#define	NEWSALIAS	"newsgroups"		/* maps nf's to newsgroups */
#ifdef BSD4.1c
#define	NGRPLEN		255			/* max newsgroup length */
#else
#define	NGRPLEN		30			/* max newsgroup length */
#endif BSD4.1c
#define	NEWSNF		1			/* alias newsgroup to nf name */
#define	NFNEWS		2			/* alias nf to newsgroup */
#define	OLDSUFFIX	"- from a notesfile"	/* to catch nf-news-nf */
#define	NFSUFFIX	"- (nf)"		/* to catch nf-news-nf */
#define NGDELIM         ','
#define NEGCHAR		'!'
#define TRUE		1
#define FALSE		0
#define NETCHRS	"!:.@^"	/* Punct. chars used for various networks	*/
#define PROTO		'A'			/* old protocol */
#define NUMGROUPS	5
