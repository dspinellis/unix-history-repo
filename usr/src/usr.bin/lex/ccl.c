/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Vern Paxson of Lawrence Berkeley Laboratory.
 * 
 * The United States Government has rights in this work pursuant
 * to contract no. DE-AC03-76SF00098 between the United States
 * Department of Energy and the University of California.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)ccl.c	5.2 (Berkeley) 6/18/90";
#endif /* not lint */

/* ccl - routines for character classes */

#include "flexdef.h"

/* ccladd - add a single character to a ccl
 *
 * synopsis
 *    int cclp;
 *    int ch;
 *    ccladd( cclp, ch );
 */

void ccladd( cclp, ch )
int cclp;
int ch;

    {
    int ind, len, newpos, i;

    len = ccllen[cclp];
    ind = cclmap[cclp];

    /* check to see if the character is already in the ccl */

    for ( i = 0; i < len; ++i )
	if ( ccltbl[ind + i] == ch )
	    return;

    newpos = ind + len;

    if ( newpos >= current_max_ccl_tbl_size )
	{
	current_max_ccl_tbl_size += MAX_CCL_TBL_SIZE_INCREMENT;

	++num_reallocs;

	ccltbl = reallocate_character_array( ccltbl, current_max_ccl_tbl_size );
	}

    ccllen[cclp] = len + 1;
    ccltbl[newpos] = ch;
    }


/* cclinit - make an empty ccl
 *
 * synopsis
 *    int cclinit();
 *    new_ccl = cclinit();
 */

int cclinit()

    {
    if ( ++lastccl >= current_maxccls )
	{
	current_maxccls += MAX_CCLS_INCREMENT;

	++num_reallocs;

	cclmap = reallocate_integer_array( cclmap, current_maxccls );
	ccllen = reallocate_integer_array( ccllen, current_maxccls );
	cclng = reallocate_integer_array( cclng, current_maxccls );
	}

    if ( lastccl == 1 )
	/* we're making the first ccl */
	cclmap[lastccl] = 0;

    else
	/* the new pointer is just past the end of the last ccl.  Since
	 * the cclmap points to the \first/ character of a ccl, adding the
	 * length of the ccl to the cclmap pointer will produce a cursor
	 * to the first free space
	 */
	cclmap[lastccl] = cclmap[lastccl - 1] + ccllen[lastccl - 1];

    ccllen[lastccl] = 0;
    cclng[lastccl] = 0;	/* ccl's start out life un-negated */

    return ( lastccl );
    }


/* cclnegate - negate a ccl
 *
 * synopsis
 *    int cclp;
 *    cclnegate( ccl );
 */

void cclnegate( cclp )
int cclp;

    {
    cclng[cclp] = 1;
    }


/* list_character_set - list the members of a set of characters in CCL form
 *
 * synopsis
 *     int cset[CSIZE];
 *     FILE *file;
 *     list_character_set( cset );
 *
 * writes to the given file a character-class representation of those
 * characters present in the given set.  A character is present if it
 * has a non-zero value in the set array.
 */

void list_character_set( file, cset )
FILE *file;
int cset[];

    {
    register int i;
    char *readable_form();

    putc( '[', file );

    for ( i = 0; i < csize; ++i )
	{
	if ( cset[i] )
	    {
	    register int start_char = i;

	    putc( ' ', file );

	    fputs( readable_form( i ), file );

	    while ( ++i < csize && cset[i] )
		;

	    if ( i - 1 > start_char )
		/* this was a run */
		fprintf( file, "-%s", readable_form( i - 1 ) );

	    putc( ' ', file );
	    }
	}

    putc( ']', file );
    }
