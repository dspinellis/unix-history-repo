/* $Header: cheat.h,v 4.3 85/05/01 11:36:58 lwall Exp $
 *
 * $Log:	cheat.h,v $
 * Revision 4.3  85/05/01  11:36:58  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifdef ARTSEARCH
EXT ART_NUM srchahead INIT(0); 	/* are we in subject scan mode? */
				/* (if so, contains art # found or -1) */
#endif

#ifdef PENDING
#   ifdef CACHESUBJ
	EXT ART_NUM subj_to_get;
#   endif
#endif

void	cheat_init();
void	look_ahead();
void	collect_subjects();
