/* $Id: trn.h,v 4.4 1991/09/09 20:27:37 davison Trn $
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

EXT char *ngname INIT(Nullch);		/* name of current newsgroup */
EXT int ngnlen INIT(0);			/* current malloced size of ngname */
EXT char *ngdir INIT(Nullch);		/* same thing in directory name form */
EXT int ngdlen INIT(0);			/* current malloced size of ngdir */

EXT NG_NUM ng INIT(0);		/* current newsgroup index into rcline and toread */
EXT NG_NUM current_ng INIT(0);	/* stable current newsgroup so we can ditz with ng */
EXT NG_NUM starthere INIT(0);   /* set to the first newsgroup with unread news on startup */
EXT char *spool INIT(Nullch);		/* public news spool directory */
EXT char *threaddir INIT(Nullch);	/* directory for thread data */
EXT char *overviewdir INIT(Nullch);	/* directory for overview data */

void	rn_init _((void));
void	main _((int,char**));
void	set_ngname _((char*));
char	*getngdir _((char*));
