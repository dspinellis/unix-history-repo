/* $Header: sw.h,v 4.3 85/05/01 11:51:07 lwall Exp $
 *
 * $Log:	sw.h,v $
 * Revision 4.3  85/05/01  11:51:07  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifdef INNERSEARCH
EXT int gline INIT(0);
#endif

void    sw_init();
void    sw_file();
void    sw_list();
void	decode_switch();
void	pr_switches();
void	cwd_check();
