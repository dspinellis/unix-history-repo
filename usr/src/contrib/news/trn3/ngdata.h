/* $Id: ngdata.h,v 3.0 1992/02/01 03:09:32 davison Trn $
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

EXT FILE *actfp INIT(Nullfp);	/* the active file */
EXT bool writesoft INIT(FALSE);	/* rewrite the soft pointer file? */
EXT int softtries INIT(0), softmisses INIT(0);

EXT ART_NUM absfirst INIT(0);	/* 1st real article in current newsgroup */
EXT ART_NUM firstart INIT(0);	/* minimum unread article number in newsgroup */
EXT ART_NUM lastart INIT(0);	/* maximum article number in newsgroup */

#ifdef USE_NNTP
EXT char active_name[MAXFILENAME];
EXT time_t lastactfetch INIT(0);
#define MINFETCHTIME (2 * 60 * 60)
#endif

EXT ART_NUM *abs1st INIT(NULL);	/* 1st real article in newsgroup */

EXT char *moderated;
EXT bool ThreadedGroup;
EXT long activeitems;			/* number of enties in active file */

void	ngdata_init _((void));
bool	access_ng _((void));
void	grow_ng _((ART_NUM));
void	ng_skip _((void));
ART_NUM	getngsize _((NG_NUM));
void	getngmissing _((void));

ACT_POS findact _((char*,char*,int,long));
void ngdatansrv_init _(());
