/*
 * iextern - external definitions for inews.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)iextern.c	6.2	5/17/86";
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

#ifndef ROOTID
int	ROOTID;				/* special users id # */
#endif

char	*DFLTNG = "general";		/* default newsgroup */
FILE	*infp;				/* input file-pointer */
FILE	*actfp;				/* active newsgroups file pointer */
int	tty;				/* set if infp is a tty */
char	*PARTIAL = "dead.article";	/* place to save partial news */
#ifndef	SHELL
char	*SHELL = "/bin/sh";		/* shell for inews to use	*/
#endif
int	is_ctl;				/* true for a control message */
