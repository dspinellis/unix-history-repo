/* $Header: init.h,v 4.3 85/05/01 11:40:46 lwall Exp $
 *
 * $Log:	init.h,v $
 * Revision 4.3  85/05/01  11:40:46  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *lockname INIT(nullstr);

bool	initialize();
void	lock_check();
void	newsnews_check();
void	version_check();
