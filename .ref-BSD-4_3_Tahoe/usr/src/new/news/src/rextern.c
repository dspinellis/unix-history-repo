/*
 * rextern - external definitions for readnews
 */

#ifdef SCCSID
static char	*SccsId = "@(#)rextern.c	2.17	11/21/86";
#endif /* SCCSID */

/*LINTLIBRARY*/

#include "rparams.h"

int	uid, gid;			/* real user/group I.D.		*/
int	duid, dgid;			/* effective user/group I.D.	*/
int	SigTrap;			/* set if signal trapped	*/
int	savmask;			/* old umask			*/
int	mode;				/* mode of news program		*/
struct hbuf header;			/* general-use header structure	*/
char	bfr[LBUFLEN];			/* general-use scratch area	*/

#ifndef ROOTID
int	ROOTID;				/* special users id #		*/
#endif

char	*outfile = "/tmp/M1XXXXXX";	/* output file for -M and -c	*/
char	*infile = "/tmp/M2XXXXXX";	/* -T output from Mail		*/
int	ngrp, line = -1;

char	filename[BUFLEN];
char	afline[BUFLEN];
FILE	*rcfp, *actfp;
time_t	atime;
char	newsrc[BUFLEN], groupdir[BUFLEN], *rcline[LINES], rcbuf[LBUFLEN];
char	*bitmap, *argvrc[LINES];
long	bit, obit, last;
int	readmode = NEXT;
int	news = 0;		/* Was there any news to read */
int	actdirect = FORWARD;	/* read direction in ACTIVE file */
int	rcreadok = FALSE;	/* NEWSRC has been read OK */
int	zapng = FALSE;		/* ! out this newsgroup on next updaterc */
long	ngsize;			/* max article # in this newsgroup */
long	minartno;		/* min article # in this newsgroup */

#ifndef SHELL
char	*SHELL;
#endif

#ifndef MAILER
char	*MAILER;
#endif

char	*PAGER = "";
