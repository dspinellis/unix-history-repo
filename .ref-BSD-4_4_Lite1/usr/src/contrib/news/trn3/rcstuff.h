/* $Id: rcstuff.h,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

EXT char **rcline INIT(NULL);/* pointers to lines of .newsrc */
EXT ART_UNREAD *toread INIT(NULL);
			/* number of articles to be read in newsgroup */
			/* <0 => invalid or unsubscribed newsgroup */
#define TR_ONE ((ART_UNREAD) 1)
#define TR_NONE ((ART_UNREAD) 0)
#define TR_UNSUB ((ART_UNREAD) -1)
			/* keep this one as -1, some tests use >= TR_UNSUB */
#define TR_BOGUS ((ART_UNREAD) -2)
#define TR_JUNK ((ART_UNREAD) -3)

#define RCCHAR(ch) ((ch) == '0' ? ':' : (ch))

#define ADDNEW_SUB ':'
#define ADDNEW_UNSUB '!'

#define GNG_RELOC	0x0001
#define GNG_FUZZY	0x0002

EXT char *rcchar INIT(NULL); /* holds the character : or ! while spot is \0 */
EXT char *rcnums INIT(NULL); /* offset from rcline to numbers on line */
EXT ACT_POS *softptr INIT(NULL);
			/* likely ptr to active file entry for newsgroup */
EXT bool paranoid INIT(FALSE);	/* did we detect some inconsistency in .newsrc? */
EXT int maxrcline INIT(0);	/* current maximum # of lines in .newsrc */
EXT int addnewbydefault INIT(0);

bool	rcstuff_init _((void));
void	abandon_ng _((NG_NUM));
bool	get_ng _((char*,int)); /* return TRUE if newsgroup is found or added */
NG_NUM	add_newsgroup _((char*,char_int));
#ifdef RELOCATE
NG_NUM	relocate_newsgroup _((NG_NUM,NG_NUM)); /* move newsgroup around */
#endif
void	list_newsgroups _((void));
NG_NUM	find_ng _((char*));	/* return index of newsgroup */
void	cleanup_rc _((void));
void	sethash _((NG_NUM));
int	hash _((char*));
void	newsrc_check _((void));
void	checkpoint_rc _((void));
void	write_rc _((void));
void	get_old_rc _((void));
