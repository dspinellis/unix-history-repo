/* $Header: bits.h,v 4.3.1.2 86/11/03 09:49:58 lwall Exp $
 *
 * $Log:	bits.h,v $
 * Revision 4.3.1.2  86/11/03  09:49:58  lwall
 * Added firstbit variable.
 * 
 * Revision 4.3.1.1  85/05/10  11:31:52  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:36:39  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *ctlarea INIT(Nullch);	/* one bit for each article in current newsgroup */
			/* with the following interpretation: */
			/*	0 => unread  */
			/*	1 => read    */

/* if subscripting is faster than shifting on your machine, define this */
#undef USESUBSCRIPT
#ifdef USESUBSCRIPT
EXT char powerof2[] INIT({1,2,4,8,16,32,64,128});
#define pow2(x) powerof2[x]
#else
#define pow2(x) (1 << (x))
#endif

#ifdef lint
EXT bool nonesuch INIT(FALSE);
#define ctl_set(a)
#define ctl_clear(a)
#define ctl_read(a) nonesuch
#define was_read(a) nonesuch
#else
#define ctl_set(a) (ctlarea[(OFFSET(a)) / BITSPERBYTE] |= pow2((OFFSET(a)) % BITSPERBYTE))
#define ctl_clear(a) (ctlarea[(OFFSET(a)) / BITSPERBYTE] &= ~pow2((OFFSET(a)) % BITSPERBYTE))
#define ctl_read(a) ((ctlarea[(OFFSET(a)) / BITSPERBYTE] & pow2((OFFSET(a)) % BITSPERBYTE)) != 0)

#define was_read(a) ((a)<firstbit || ctl_read(a))
#endif lint

EXT ART_NUM absfirst INIT(0);	/* 1st real article in current newsgroup */
EXT ART_NUM firstart INIT(0);	/* minimum unread article number in newsgroup */
EXT ART_NUM firstbit INIT(0);	/* minimum valid bit, usually == firstart */
EXT ART_NUM lastart INIT(0);	/* maximum article number in newsgroup */

#ifdef DELAYMARK
EXT FILE *dmfp INIT(Nullfp);
EXT char *dmname INIT(Nullch);
EXT int dmcount INIT(0);
#endif

void	bits_init();
void	checkpoint_rc();
void	restore_ng();
void	onemore();
void	oneless();
void	unmark_as_read();
void	delay_unmark();
void	mark_as_read();
void	check_first();
#ifdef DELAYMARK
    void	yankback();
#endif
int	chase_xrefs();
int	initctl();
void	grow_ctl();
