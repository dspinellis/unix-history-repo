/* $Header: respond.h,v 4.3 85/05/01 11:47:50 lwall Exp $
 *
 * $Log:	respond.h,v $
 * Revision 4.3  85/05/01  11:47:50  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *savedest INIT(Nullch);	/* value of %b */
EXT ART_POS savefrom INIT(0);		/* value of %B */
EXT char *headname INIT(Nullch);

#define SAVE_ABORT 0
#define SAVE_DONE 1

void	respond_init();
int	save_article();
int	cancel_article();
void	reply();
void	followup();
void	invoke();
