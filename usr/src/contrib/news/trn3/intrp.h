/* $Id: intrp.h,v 3.0 1992/02/01 03:09:32 davison Trn $
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

EXT char *lib INIT(Nullch);		/* news library */
EXT char *rnlib INIT(Nullch);		/* private news program library */
EXT char *origdir INIT(Nullch);		/* cwd when rn invoked */
EXT char *homedir INIT(Nullch);		/* login directory */
EXT char *dotdir INIT(Nullch);		/* where . files go */
EXT char *logname INIT(Nullch);		/* login id of user */
EXT char *realname INIT(Nullch);	/* real name of user */
EXT char *phostname INIT(Nullch);	/* host name in a posting */
EXT int perform_cnt;

#ifdef NEWS_ADMIN
    EXT char newsadmin[] INIT(NEWS_ADMIN);/* news administrator */
    EXT int newsuid INIT(0);
#endif

void    intrp_init _((char*));
char	*filexp _((char*));
char	*dointerp _((char*,int,char*,char*));
void	interp _((char*,int,char*));
void	refscpy _((char*,int,char*));
char	*getrealname _((long));
