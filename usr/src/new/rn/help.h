/* $Header: help.h,v 4.3 85/05/01 11:39:19 lwall Exp $
 *
 * $Log:	help.h,v $
 * Revision 4.3  85/05/01  11:39:19  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

void	help_init();
int	help_ng();
int	help_art();
int	help_page();
#ifdef ESCSUBS
    int	help_subs();
#endif
