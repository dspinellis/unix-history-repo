/* $Id: final.h,v 3.0 1992/02/01 03:09:32 davison Trn $
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

/* cleanup status for fast exits */

EXT bool panic INIT(FALSE);		/* we got hung up or something-- */
					/*  so leave tty alone */
EXT bool rc_changed INIT(FALSE);	/* need we rewrite .newsrc? */
EXT bool doing_ng INIT(FALSE);		/* do we need to reconstitute */
					/* current rc line? */

EXT char int_count INIT(0);		/* how many interrupts we've had */

EXT bool clear_on_stop INIT(FALSE);	/* set when handling the stop signal */
					/* would leave the screen a mess */

/* signal catching routines */

Signal_t int_catcher _((int));
Signal_t sig_catcher _((int));
#ifdef SIGTSTP
Signal_t stop_catcher _((int));
#endif

void	final_init _((void));
void	finalize _((int));
