/* $Id: sw.h,v 3.0 1992/02/01 03:09:32 davison Trn $
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

#ifdef INNERSEARCH
EXT int gline INIT(0);
#endif

void    sw_init _((int,char**,char**));
void    sw_file _((char**,bool_int));
void    sw_list _((char*));
void	decode_switch _((char*));
void	pr_switches _((void));
void	cwd_check _((void));
