/* $Header: artstate.h,v 4.3.1.2 85/05/13 09:30:30 lwall Exp $
 *
 * $Log:	artstate.h,v $
 * Revision 4.3.1.2  85/05/13  09:30:30  lwall
 * Added CUSTOMLINES option.
 * 
 * Revision 4.3.1.1  85/05/10  11:31:32  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:35:59  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT bool reread INIT(FALSE);		/* consider current art temporarily */
				    /* unread? */
EXT bool do_fseek INIT(FALSE);	/* should we back up in article file? */

EXT bool oldsubject INIT(FALSE);	/* not 1st art in subject thread */
EXT ART_LINE topline INIT(-1);		/* top line of current screen */
EXT bool do_hiding INIT(TRUE);		/* hide header lines with -h? */
#ifdef ROTATION
EXT bool rotate INIT(FALSE);		/* has rotation been requested? */
#endif
EXT char *prompt;			/* pointer to current prompt */

EXT char *firstline INIT(Nullch);			/* special first line? */
#ifdef CUSTOMLINES
EXT char *hideline INIT(Nullch);		/* custom line hiding? */
EXT char *pagestop INIT(Nullch);		/* custom page terminator? */
EXT COMPEX hide_compex;
EXT COMPEX page_compex;
#endif
