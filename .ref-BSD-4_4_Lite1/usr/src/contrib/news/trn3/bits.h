/* $Id: bits.h,v 3.0 1991/09/09 20:18:23 davison Trn $
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

EXT char *found_bits INIT(Nullch);
EXT ART_NUM found_min;

/* if subscripting is faster than shifting on your machine, define this */
#undef USESUBSCRIPT
#ifdef USESUBSCRIPT
EXT char powerof2[] INIT({1,2,4,8,16,32,64,128});
#define pow2(x) powerof2[x]
#else
#define pow2(x) (1 << (x))
#endif

#define foundart(a) (found_bits[((a)-found_min) / BITSPERBYTE] \
	|= pow2(((a)-found_min) % BITSPERBYTE))
#define artismissing(a) (!(found_bits[((a)-found_min) / BITSPERBYTE] \
	& pow2(((a)-found_min) % BITSPERBYTE)))

EXT int dmcount INIT(0);

void	bits_init _((void));
void	rc_to_bits _((void));
void	bits_to_rc _((void));
void	setfoundbits _((void));
void	setmissingbits _((void));
void	onemore _((ARTICLE*));
void	oneless _((ARTICLE*));
void	onemissing _((ARTICLE*));
void	unmark_as_read _((void));
void	set_read _((ARTICLE*));
void	set_unread _((ARTICLE*));
void	delay_unmark _((ARTICLE*));
void	mark_as_read _((void));
void	check_first _((ART_NUM));
void	yankback _((void));
int	chase_xrefs _((ART_NUM,int));
