/* $Header: rcstuff.h,v 4.3 85/05/01 11:46:49 lwall Exp $
 *
 * $Log:	rcstuff.h,v $
 * Revision 4.3  85/05/01  11:46:49  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *rcline[MAXRCLINE];/* pointers to lines of .newsrc */
EXT ART_UNREAD toread[MAXRCLINE];
			/* number of articles to be read in newsgroup */
			/* <0 => invalid or unsubscribed newsgroup */
#define TR_ONE ((ART_UNREAD) 1)
#define TR_NONE ((ART_UNREAD) 0)
#define TR_UNSUB ((ART_UNREAD) -1)
			/* keep this one as -1, some tests use >= TR_UNSUB */
#define TR_BOGUS ((ART_UNREAD) -2)
#define TR_JUNK ((ART_UNREAD) -3)

EXT char rcchar[MAXRCLINE]; /* holds the character : or ! while spot is \0 */
EXT char rcnums[MAXRCLINE]; /* offset from rcline to numbers on line */
EXT ACT_POS softptr[MAXRCLINE];
			/* likely ptr to active file entry for newsgroup */
EXT bool paranoid INIT(FALSE);	/* did we detect some inconsistency in .newsrc? */

bool	rcstuff_init();
bool	get_ng();	/* return TRUE if newsgroup can be found or added */
NG_NUM	add_newsgroup();
#ifdef RELOCATE
    NG_NUM	relocate_newsgroup();	/* move newsgroup around */
#endif
void	list_newsgroups();
NG_NUM	find_ng();	/* return index of newsgroup */
void	cleanup_rc();
void	sethash();
int	hash();
void	newsrc_check();
void	write_rc();
void	get_old_rc();
