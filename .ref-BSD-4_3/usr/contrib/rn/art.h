/* $Header: art.h,v 4.3 85/05/01 11:35:29 lwall Exp $
 *
 * $Log:	art.h,v $
 * Revision 4.3  85/05/01  11:35:29  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

/* do_article() return values */

#define DA_NORM 0
#define DA_RAISE 1
#define DA_CLEAN 2
#define DA_TOEND 3

void	art_init();
int	do_article();
int	page_switch();
bool	innermore();
