/*
 * iextern - external definitions for inews.
 */
/*LINTLIBRARY*/

#ifdef SCCSID
static char	*SccsId = "@(#)iextern.c	2.18	9/24/87";
#endif /* SCCSID */

#include "iparams.h"

int	uid, gid;			/* real user/group I.D. */
int	duid, dgid;			/* effective user/group I.D. */
int	SigTrap;			/* set if signal trapped */
int	savmask;			/* old umask */
int	mode;				/* mode of news program */
struct hbuf header;			/* general-use header structure */
char	bfr[LBUFLEN];			/* general-use scratch area */
char	nbuf[LBUFLEN];			/* local newsgroup buffer */
char	filename[BUFLEN];		/* general-use file name */
char	not_here[SBUFLEN];		/* name of system not to xmit to */

#ifndef ROOTID
int	ROOTID;				/* special users id # */
#endif

char	*DFLTNG = "general";		/* default newsgroup */
FILE	*infp;				/* input file-pointer */
FILE	*actfp;				/* active newsgroups file pointer */
int	tty;				/* set if infp is a tty */
char	*PARTIAL = "dead.article";	/* place to save partial news */
char	*SHELL = "/bin/sh";		/* shell for inews to use	*/
int	is_ctl;				/* true for a control message */
char	is_mod[NAMELEN];		/* contains newsgroup if moderated */
