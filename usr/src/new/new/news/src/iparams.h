/*
 * iparams - parameters for inews.
 */

static char *Iparams = "@(#)iparams.h	2.3	3/27/83";

#include "params.h"

/* external declarations specific to inews */
extern	char	nbuf[], ARTICLE[], INFILE[], *PARTIAL;
#ifndef ROOTID
extern	int	ROOTID;
#endif

#ifdef NOTIFY
extern	char	TELLFILE[], TELLME[];
#endif NOTIFY

extern	char	logfname[], STASH[];

extern	FILE	*infp,*actfp;
extern	int	tty, flag, is_ctl;
extern	char	filename[],*DFLTNG,whatever[];
