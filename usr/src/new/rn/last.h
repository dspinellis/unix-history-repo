/* $Header: last.h,v 4.3 85/05/01 11:42:22 lwall Exp $
 *
 * $Log:	last.h,v $
 * Revision 4.3  85/05/01  11:42:22  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *lastngname INIT(Nullch);	/* last newsgroup read, from .rnlast file */
EXT long lasttime INIT(0);	/* time last rn was started up */
EXT long lastactsiz INIT(0);	/* size of active file when rn last started up */

void	last_init();
void    writelast();
