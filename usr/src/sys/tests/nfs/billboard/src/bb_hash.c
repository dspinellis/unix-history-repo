/*
 ******************************************************************************
 *
 * Module: bb_hash.c
 *
 * Functions: 
 *	    bb_put_hash()	- Place an entry in the hash table.
 *	    bb_get_hash()	- Return an index from the hash table.
 *
 *
 ******************************************************************************
 */

/*
 ******************************************************************************
 * Include Files
 ******************************************************************************
 */
#include <stdio.h>
#include <rpc/rpc.h>
#include "common.h"
#include "protocol.h"
#include "server.h"

static BB_hash		bb_hash;	/* The hash table of id, index.	*/
static BB_co_data	bb_co_d[BB_MAX_IMP]; /* The company data table.	*/

int
bb_put_hash( id, index)
  BB_id		id;		/* The identifier to hash.		*/
  int		index;		/* Index of this record in co. data tbl.*/
{
    int		i;		/* Nice loop variable name.		*/
    int		hash_idx = 0;	/* Index into hash table.		*/
    int		term_idx;	/* Termination index of circular search.*/

    /*
    **  Sum up all of the characters in the id and mod it by the
    **  hash list size.  This is the initial hash index.
    */
    for( i = 0; (id[i] != NUL) && (i < BB_ID_NAME_LEN); i++)
    {
	hash_idx += id[i];
    }
    hash_idx = term_idx = (hash_idx % BB_MAX_HASH);

    /*
    **  Search the table for the first open hash bucket.  If the hash
    **  table does not contain one break at the termination index.
    */
    while( bb_hash[hash_idx].id_ptr != NULL )
    {
	hash_idx = ((hash_idx + 1) % BB_MAX_HASH);
	if ( hash_idx == term_idx )
	    break;
    }
    
    /*
    **  If the hash bucket is empty then install the info here.
    **  Otherwise the table is full.
    */
    if ( bb_hash[hash_idx].id_ptr == NULL )
    {
	bb_hash[hash_idx].index  = index;
	bb_hash[hash_idx].id_ptr = id;
	return BB_SUCCESS;
    }
    else
    {
	return BB_HASH_TABLE_FULL;
    }
}

int
bb_get_hash( id)
  BB_id		id;		/* The identifier to hash.		*/
{
    int		i;		/* Nice loop variable name.		*/
    int		hash_idx = 0;	/* Index into hash table.		*/
    int		term_idx;	/* Termination index of circular search.*/

    /*
    **  Sum up all of the characters in the id and mod it by the
    **  hash list size.  This is the initial hash index.
    */
    for( i = 0; (id[i] != NUL) && (i < BB_ID_NAME_LEN); i++)
    {
	hash_idx += id[i];
    }
    hash_idx = term_idx = (hash_idx % BB_MAX_HASH);

    /*
    **  Search the hash table based upon the initial index.  The search
    **  ends when the index is found or one complete cycle of the hash
    **  table has been done.  This is linear resolution.
    */
    while( ( bb_hash[hash_idx].id_ptr != NULL ) && 
	   ( strncmp( bb_hash[hash_idx].id_ptr, id, BB_ID_NAME_LEN) != 0 ) )
    {
	hash_idx = ((hash_idx + 1) % BB_MAX_HASH);
	if ( hash_idx == term_idx )
	    break;
    }
    
    /*
    **  If the id is equal to that of the hash bucket then a match is 
    **  found.  Otherwise the table does not contain this id.
    */
    if ( ( bb_hash[hash_idx].id_ptr != NUL ) &&
	 ( strncmp( bb_hash[hash_idx].id_ptr, id, BB_ID_NAME_LEN) == 0 ) )
    {
	return bb_hash[hash_idx].index;
    }
    else
    {
	return BB_HASH_ID_NOT_FOUND;
    }
}
