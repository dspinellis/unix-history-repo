/* $Id: respond.h,v 3.0 1992/02/01 03:09:32 davison Trn $
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

EXT char *savedest INIT(Nullch);	/* value of %b */
EXT char *extractdest INIT(Nullch);	/* value of %E */
EXT char *extractprog INIT(Nullch);	/* value of %e */
EXT ART_POS savefrom INIT(0);		/* value of %B */
EXT char *headname INIT(Nullch);

#define SAVE_ABORT 0
#define SAVE_DONE 1

void	respond_init _((void));
int	save_article _((void));
int	cancel_article _((void));
int	supersede_article _((void));
void	reply _((void));
void	followup _((void));
void	invoke _((char*,char*));
