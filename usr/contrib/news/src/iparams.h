/*
 * iparams - parameters for inews.
 */

/*	@(#)iparams.h	2.12	4/16/85	*/

#include "params.h"

/* external declarations specific to inews */
extern	char	nbuf[], *ARTICLE, *INFILE, *ALIASES, *PARTIAL;
#ifndef ROOTID
extern	int	ROOTID;
#endif

#ifdef NOTIFY
extern	char	*TELLME;
#endif /* NOTIFY */

extern	FILE	*infp,*actfp;
extern	int	tty, is_ctl;
extern	char	filename[], *DFLTNG;
