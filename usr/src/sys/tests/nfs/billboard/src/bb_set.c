/*
 ******************************************************************************
 *
 * Module: bb_set.c
 *
 * Functions: 
 *	    bb_set_1()		- Set an entry in the billboard.
 *	    bb_unset_1()	- Unset an entry in the billboard.
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

void bb_board_tag();


/*************************************************************************
**									**
**  bb_set_1() - Set a location in the board.  This means that the 	**
**  client has successfully tested his client against the specified	**
**  server.  A password is required.					**
**									**
*************************************************************************/
BB_set_out *
bb_set_1( p_set)
  BB_set_in	*p_set;		/* The set_in structure.		*/
{
    static BB_set_out	result;	/* The result of the set operation.	*/

    result.status = BB_SUCCESS;

    /*
    **  If the machine being used to send this request is not one
    **  of the ones owned by this client group then a password must
    **  be specified.
    */
    if ( bb_check_ip( p_set->client) != BB_SUCCESS )
    {
	if ( (result.status=bb_check_passwd( p_set->client, p_set->passwd))
	     != BB_SUCCESS )
	{
	    result.status = BB_BAD_PASSWD;
	    return &result;
	}
    }

    /*
    **  If the server being verified against is the universal server,
    **  then change the phase of the client.
    */
    if ( strncmp( BB_SUN_SERVER, p_set->server, BB_ID_NAME_LEN) == 0 )
    {
	if ( bb_set_phase( p_set->client, BB_SUN_PHASE) != BB_SUCCESS )
	{
	    result.status = BB_BAD_PHASE;
	    return &result;
	}
    }

    /*
    **  If the phase is not correct don't let them change the board.
    */
    if ( bb_phase_ok( p_set->client) != BB_SUCCESS )
    {
	result.status = BB_BAD_PHASE;
	return &result;
    }

    /*
    **  They are allowed to place a mark in the bill board, so lets do it.
    */
    bb_board_tag( p_set, &result, BB_SET);

    return &result;
}



/*************************************************************************
**									**
**  bb_unset_1() - Unset a previously set spot in the bill board.	**
**									**
*************************************************************************/
BB_set_out *
bb_unset_1( p_set)
  BB_set_in	*p_set;		/* The set_in structure.		*/
{
    static BB_set_out	result;	/* The result of the set operation.	*/

    /*
    **  If the machine being used to send this request is not one
    **  of the ones owned by this client group then a password must
    **  be specified.
    */
    if ( bb_check_ip( p_set->client) != BB_SUCCESS )
    {
	if ( bb_check_passwd( p_set->client, p_set->passwd) != SUCCESS )
	{
	    result.status = BB_BAD_PASSWD;
	    return &result;
	}
    }

    /*
    **  If the server being unverified is the universal server,
    **  then change the phase of the client.
    */
    if ( strncmp( BB_SUN_SERVER, p_set->server, BB_ID_NAME_LEN) == 0 )
    {
	if ( bb_unset_phase( p_set->client, BB_SUN_PHASE) != BB_SUCCESS )
	{
	    result.status = BB_BAD_PHASE;
	    return &result;
	}
    }

    /*
    **  They are allowed to place a mark in the bill board, so lets do it.
    */
    bb_board_tag( p_set, &result, BB_UNSET);

    return &result;
}
