/* $Header: addng.h,v 4.3 85/05/01 11:34:48 lwall Exp $
 *
 * $Log:	addng.h,v $
 * Revision 4.3  85/05/01  11:34:48  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

void	addng_init();
#ifdef FINDNEWNG
    bool	newlist();
    long	birthof();
    bool	scanactive();
#endif
