/* $Id: addng.h,v 3.0 1992/02/01 03:09:32 davison Trn $
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

void	addng_init _((void));
#ifdef FINDNEWNG
bool	newlist _((bool_int,bool_int));
# ifdef ACTIVE_TIMES
bool	find_new_groups _((void));
# else
long	birthof _((char*,ART_NUM));
# endif
bool	scanactive _((void));
#endif
