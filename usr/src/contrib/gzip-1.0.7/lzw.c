/* lzw.c -- compress files in LZW format.
 * This is a dummy version avoiding patent problems.
 */

#ifndef lint
static char rcsid[] = "$Id: lzw.c,v 0.7 1993/02/10 16:07:22 jloup Exp $";
#endif

#include "tailor.h"
#include "gzip.h"
#include "lzw.h"

#include <stdio.h>

static int msg_done = 0;

/* Compress in to out with lzw method. */
void lzw(in, out) 
    int in, out;
{
    if (msg_done) return;
    msg_done = 1;
    fprintf(stderr,"output in compress .Z format not supported\n");
    in++, out++; /* avoid warnings on unused variables */
    exit_code = ERROR;
}
