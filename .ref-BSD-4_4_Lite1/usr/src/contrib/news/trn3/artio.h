/* $Id: artio.h,v 3.0 1991/09/09 20:18:23 davison Trn $
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

EXT ART_POS artpos INIT(0);	/* byte position in article file */

EXT ART_LINE artline INIT(0);	/* current line number in article file */
EXT FILE *artfp INIT(Nullfp);	/* current article file pointer */
EXT ART_NUM openart INIT(0);	/* the article number we have open */

void artio_init _((void));
FILE *artopen _((ART_NUM));	/* open an article unless already opened */
#ifdef LINKART
EXT char *linkartname INIT(nullstr);/* real name of article for Eunice */
#endif
