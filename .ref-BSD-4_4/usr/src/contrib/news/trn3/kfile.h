/* $Id: kfile.h,v 3.0 1991/09/09 20:18:23 davison Trn $
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

#define KF_GLOBAL 0
#define KF_LOCAL 1

#ifdef KILLFILES
EXT FILE *globkfp INIT(Nullfp);		/* global article killer file */
EXT FILE *localkfp INIT(Nullfp);	/* local (for this newsgroup) file */
#endif
EXT bool has_normal_kills;		/* flag when KILLs needs rereading */
EXT bool save_ids;			/* flag when we need to write ids */
EXT ART_NUM killfirst;			/* used as firstart when killing */

void	kfile_init _((void));
int	do_kfile _((FILE*,int));
void	kill_unwanted _((ART_NUM,char*,int));
int	edit_kfile _((void));
void	open_kfile _((int));
void    kf_append _((char*));
void	setthru _((ART_NUM));
