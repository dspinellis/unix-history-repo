/* $Id: artstate.h,v 3.0 1991/09/09 20:18:23 davison Trn $
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

EXT bool reread INIT(FALSE);		/* consider current art temporarily */
					/* unread? */

EXT bool do_fseek INIT(FALSE);	/* should we back up in article file? */

EXT bool oldsubject INIT(FALSE);	/* not 1st art in subject thread */
EXT ART_LINE topline INIT(-1);		/* top line of current screen */
EXT bool do_hiding INIT(TRUE);		/* hide header lines with -h? */
#ifdef ROTATION
EXT bool rotate INIT(FALSE);		/* has rotation been requested? */
#endif
EXT char *prompt;			/* pointer to current prompt */

EXT char *firstline INIT(Nullch);			/* special first line? */
#ifdef CUSTOMLINES
EXT char *hideline INIT(Nullch);		/* custom line hiding? */
EXT char *pagestop INIT(Nullch);		/* custom page terminator? */
EXT COMPEX hide_compex;
EXT COMPEX page_compex;
#endif
