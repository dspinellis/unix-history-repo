/* $Header: ngstuff.h,v 4.3 85/05/01 11:45:12 lwall Exp $
 *
 * $Log:	ngstuff.h,v $
 * Revision 4.3  85/05/01  11:45:12  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#define NN_NORM 0
#define NN_INP 1
#define NN_REREAD 2
#define NN_ASK 3

void	ngstuff_init();
int	escapade();
int	switcheroo();
int	numnum();
int	perform();
