/* sym - symbol table routines */

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
static char sccsid[] = "@(#)sym.c	5.2 (Berkeley) 6/18/90";
#endif /* not lint */

#include "flexdef.h"

/* declare functions that have forward references */

int hashfunct PROTO((register char[], int));


struct hash_entry *ndtbl[NAME_TABLE_HASH_SIZE];
struct hash_entry *sctbl[START_COND_HASH_SIZE];
struct hash_entry *ccltab[CCL_HASH_SIZE];

struct hash_entry *findsym();


/* addsym - add symbol and definitions to symbol table
 *
 * synopsis
 *    char sym[], *str_def;
 *    int int_def;
 *    hash_table table;
 *    int table_size;
 *    0 / -1 = addsym( sym, def, int_def, table, table_size );
 *
 * -1 is returned if the symbol already exists, and the change not made.
 */

int addsym( sym, str_def, int_def, table, table_size )
register char sym[];
char *str_def;
int int_def;
hash_table table;
int table_size;

    {
    int hash_val = hashfunct( sym, table_size );
    register struct hash_entry *sym_entry = table[hash_val];
    register struct hash_entry *new_entry;
    register struct hash_entry *successor;

    while ( sym_entry )
	{
	if ( ! strcmp( sym, sym_entry->name ) )
	    { /* entry already exists */
	    return ( -1 );
	    }
	
	sym_entry = sym_entry->next;
	}

    /* create new entry */
    new_entry = (struct hash_entry *) malloc( sizeof( struct hash_entry ) );

    if ( new_entry == NULL )
	flexfatal( "symbol table memory allocation failed" );

    if ( (successor = table[hash_val]) )
	{
	new_entry->next = successor;
	successor->prev = new_entry;
	}
    else
	new_entry->next = NULL;

    new_entry->prev = NULL;
    new_entry->name = sym;
    new_entry->str_val = str_def;
    new_entry->int_val = int_def;

    table[hash_val] = new_entry;

    return ( 0 );
    }


/* cclinstal - save the text of a character class
 *
 * synopsis
 *    Char ccltxt[];
 *    int cclnum;
 *    cclinstal( ccltxt, cclnum );
 */

void cclinstal( ccltxt, cclnum )
Char ccltxt[];
int cclnum;

    {
    /* we don't bother checking the return status because we are not called
     * unless the symbol is new
     */
    Char *copy_unsigned_string();

    (void) addsym( (char *) copy_unsigned_string( ccltxt ), (char *) 0, cclnum,
		   ccltab, CCL_HASH_SIZE );
    }


/* ccllookup - lookup the number associated with character class text
 *
 * synopsis
 *    Char ccltxt[];
 *    int ccllookup, cclval;
 *    cclval/0 = ccllookup( ccltxt );
 */

int ccllookup( ccltxt )
Char ccltxt[];

    {
    return ( findsym( (char *) ccltxt, ccltab, CCL_HASH_SIZE )->int_val );
    }


/* findsym - find symbol in symbol table
 *
 * synopsis
 *    char sym[];
 *    hash_table table;
 *    int table_size;
 *    struct hash_entry *sym_entry, *findsym();
 *    sym_entry = findsym( sym, table, table_size );
 */

struct hash_entry *findsym( sym, table, table_size )
register char sym[];
hash_table table;
int table_size;

    {
    register struct hash_entry *sym_entry = table[hashfunct( sym, table_size )];
    static struct hash_entry empty_entry =
	{
	(struct hash_entry *) 0, (struct hash_entry *) 0, NULL, NULL, 0,
	} ;

    while ( sym_entry )
	{
	if ( ! strcmp( sym, sym_entry->name ) )
	    return ( sym_entry );
	sym_entry = sym_entry->next;
	}

    return ( &empty_entry );
    }

    
/* hashfunct - compute the hash value for "str" and hash size "hash_size"
 *
 * synopsis
 *    char str[];
 *    int hash_size, hash_val;
 *    hash_val = hashfunct( str, hash_size );
 */

int hashfunct( str, hash_size )
register char str[];
int hash_size;

    {
    register int hashval;
    register int locstr;

    hashval = 0;
    locstr = 0;

    while ( str[locstr] )
	hashval = ((hashval << 1) + str[locstr++]) % hash_size;

    return ( hashval );
    }


/* ndinstal - install a name definition
 *
 * synopsis
 *    char nd[];
 *    Char def[];
 *    ndinstal( nd, def );
 */

void ndinstal( nd, def )
char nd[];
Char def[];

    {
    char *copy_string();
    Char *copy_unsigned_string();

    if ( addsym( copy_string( nd ), (char *) copy_unsigned_string( def ), 0,
		 ndtbl, NAME_TABLE_HASH_SIZE ) )
	synerr( "name defined twice" );
    }


/* ndlookup - lookup a name definition
 *
 * synopsis
 *    char nd[], *def;
 *    char *ndlookup();
 *    def/NULL = ndlookup( nd );
 */

Char *ndlookup( nd )
char nd[];

    {
    return ( (Char *) findsym( nd, ndtbl, NAME_TABLE_HASH_SIZE )->str_val );
    }


/* scinstal - make a start condition
 *
 * synopsis
 *    char str[];
 *    int xcluflg;
 *    scinstal( str, xcluflg );
 *
 * NOTE
 *    the start condition is Exclusive if xcluflg is true
 */

void scinstal( str, xcluflg )
char str[];
int xcluflg;

    {
    char *copy_string();

    /* bit of a hack.  We know how the default start-condition is
     * declared, and don't put out a define for it, because it
     * would come out as "#define 0 1"
     */
    /* actually, this is no longer the case.  The default start-condition
     * is now called "INITIAL".  But we keep the following for the sake
     * of future robustness.
     */

    if ( strcmp( str, "0" ) )
	printf( "#define %s %d\n", str, lastsc );

    if ( ++lastsc >= current_max_scs )
	{
	current_max_scs += MAX_SCS_INCREMENT;

	++num_reallocs;

	scset = reallocate_integer_array( scset, current_max_scs );
	scbol = reallocate_integer_array( scbol, current_max_scs );
	scxclu = reallocate_integer_array( scxclu, current_max_scs );
	sceof = reallocate_integer_array( sceof, current_max_scs );
	scname = reallocate_char_ptr_array( scname, current_max_scs );
	actvsc = reallocate_integer_array( actvsc, current_max_scs );
	}

    scname[lastsc] = copy_string( str );

    if ( addsym( scname[lastsc], (char *) 0, lastsc,
		 sctbl, START_COND_HASH_SIZE ) )
	format_pinpoint_message( "start condition %s declared twice", str );

    scset[lastsc] = mkstate( SYM_EPSILON );
    scbol[lastsc] = mkstate( SYM_EPSILON );
    scxclu[lastsc] = xcluflg;
    sceof[lastsc] = false;
    }


/* sclookup - lookup the number associated with a start condition
 *
 * synopsis
 *    char str[], scnum;
 *    int sclookup;
 *    scnum/0 = sclookup( str );
 */

int sclookup( str )
char str[];

    {
    return ( findsym( str, sctbl, START_COND_HASH_SIZE )->int_val );
    }
