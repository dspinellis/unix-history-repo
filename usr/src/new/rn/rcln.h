/* $Header: rcln.h,v 4.3 85/05/01 11:45:52 lwall Exp $
 *
 * $Log:	rcln.h,v $
 * Revision 4.3  85/05/01  11:45:52  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifdef DEBUGGING
EXT ART_NUM ngmax[MAXRCLINE];
#endif

void    rcln_init();
#ifdef CATCHUP
    void	catch_up();
#endif
int	addartnum();
#ifdef MCHASE
    void	subartnum();
#endif
void	prange();
void	set_toread();
void	checkexpired();
