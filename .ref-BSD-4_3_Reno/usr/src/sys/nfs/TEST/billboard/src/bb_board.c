/*
 ******************************************************************************
 *
 * Module: bb_board.c
 *
 * Description: 
 *
 * Functions: 
 *	    bb_read_board()	- Read or create the board data file.
 *	    bb_board_tag()	- Change an entry in the board.
 *	    bb_get_clients()	- Return a list of clients.
 *	    bb_get_servers()	- Return a list of servers.
 *	    bb_board_update()	- Update the board file on disk.
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



/*************************************************************************
**									**
**  bb_read_board() - If the board data file exists then read it and	**
**  store it in the board array.  Otherwise create the file with zeroed	**
**  entries.								**
**									**
*************************************************************************/
BB_board	board;			/* The board of tested imps.	*/

int
bb_read_board()
{
    FILE	*board_file;		/* Pointer to the board file.	*/
    int		i;			/* Index into board array.	*/
    int		j;			/* Index into board array.	*/
    int		num_imps;		/* The number of implementations*/

    num_imps = bb_get_imp_cnt();

    /*
    **  If the file exists read it into the board array.
    */
    if ( (board_file = fopen( BB_BOARD_FILE, "r")) != NULL )
    {
	for ( i = 0; i < num_imps; i++ )
	{
	    for ( j = 0; j < num_imps; j++ )
	    {
		if ( (board[i][j] = getw( board_file)) == (u_char)EOF )
		{
		    fprintf( stderr, "ERROR: Board data file incomplete.\n");
		    return BB_FAILURE;
		}
	    }
	}

	fclose( board_file);
	return BB_SUCCESS;
    }

    /*
    **  The file did not exist so create it.
    */
    if ( (board_file = fopen( BB_BOARD_FILE, "w")) == NULL )
    {
	fprintf( stderr, "FAILED opening the board data file '%s'.\n",
		 BB_BOARD_FILE);
	fclose( board_file);
	return BB_FAILURE;
    }

    /*
    **  Fill the file with zeroed entries.
    */
    for ( i = 0; i < num_imps; i++ )
    {
	for ( j = 0; j < num_imps; j++ )
	{
	    if ( putw( (int)BB_BOARD_UNSET, board_file) == 1 )
	    {
		fprintf( stderr, "FAILED writting new board file.\n");
		fclose( board_file);
		return BB_FAILURE;
	    }
	}
    }

    fclose( board_file);
    return BB_SUCCESS;
}




/*************************************************************************
**									**
** bb_board_tag() - Change the value of the board slot to either set it	**
** or unset it.  Struct p_in contains a BB_id the client and the server.**
** The p_out struct contains most of the company data.			**
**									**
*************************************************************************/
void
bb_board_tag( p_in, p_out, set)
  BB_set_in	*p_in;		/* Set input, client id, server id.	*/
  BB_set_out	*p_out;		/* Set output, most company data.	*/
  int		set;
{
    int		client_id;		/* The client identifier.	*/
    int		server_id;		/* The server identifier.	*/
    BB_co_data	codata;			/* The company data.		*/
    
    /*
    **  Get the client and server id's.
    */
    if ( (client_id = bb_get_hash( p_in->client)) == BB_HASH_ID_NOT_FOUND )
    {
	p_out->status = BB_BAD_CLIENT;
	return;
    }

    if ( (server_id = bb_get_hash( p_in->server)) == BB_HASH_ID_NOT_FOUND )
    {
	p_out->status = BB_BAD_SERVER;
	return;
    }

    if ( set == BB_SET )
    {
	if ( board[server_id][client_id] == BB_BOARD_SET )
	{
	    p_out->status = BB_ALREADY_SET;
	}
	board[server_id][client_id] = BB_BOARD_SET;
    }
    else if ( set == BB_UNSET )
    {
	if ( board[server_id][client_id] == BB_BOARD_UNSET )
	{
	    p_out->status = BB_ALREADY_UNSET;
	}
	board[server_id][client_id] = BB_BOARD_UNSET;
    }
    else
    {
	fprintf( stderr, "Unknown set operation.\n");
	p_out->status = BB_FAILURE;
	return;
    }

    /*
    **  Fill in the company data parts of the output structure.
    */
    if ( bb_get_codata( client_id, &codata) != BB_SUCCESS )
    {
	/* OOOPS...this wasn't supposed to happen! */
	p_out->status = BB_FAILURE;
	return;
    }
    p_out->client.booth = codata.booth;
    strncpy( p_out->client.company, codata.company, BB_COMPANY_NAME_LEN);
    strncpy( p_out->client.imp, codata.imp, BB_IMP_NAME_LEN);
    strncpy( p_out->client.id, codata.id, BB_ID_NAME_LEN);
    if ( bb_get_codata( server_id, &codata) != BB_SUCCESS )
    {
	/* OOOPS...this wasn't supposed to happen! */
	p_out->status = BB_FAILURE;
	return;
    }
    p_out->server.booth = codata.booth;
    strncpy( p_out->server.company, codata.company, BB_COMPANY_NAME_LEN);
    strncpy( p_out->server.imp, codata.imp, BB_IMP_NAME_LEN);
    strncpy( p_out->server.id, codata.id, BB_ID_NAME_LEN);

    /*
    **  Set the status to indicate whether or not the write to disk
    **  succeeded otherwise use the previously set status.
    */
    if ( bb_board_update( client_id, server_id) != BB_SUCCESS )
    {
	p_out->status = BB_FAILURE;
    }
}



/*************************************************************************
**									**
**  bb_get_clients() - Get the list of all the clients and their test	**
**  status for a particular server.					**
**									**
*************************************************************************/
int
bb_get_clients( id, list)
  BB_id		id;		/* Id of the client.		*/
  BB_row	list;		/* List of all the servers.	*/
{
    int		index;		/* Index of the client.		*/

    if ( (index = bb_get_hash( id)) == BB_HASH_ID_NOT_FOUND )
    {
        return BB_BAD_CLIENT;
    }

    memcpy( list, board[index], sizeof( list));
    return BB_SUCCESS;
}



/*************************************************************************
**									**
**  bb_get_servers() - Get the list of all the servers and their test	**
**  status for a particular client.					**
**									**
*************************************************************************/
int
bb_get_servers( id, list)
  BB_id		id;		/* Id of the client.		*/
  BB_row	list;		/* List of all the servers.	*/
{
    int		index;		/* Index of the client.		*/
    int		i;		/* Nice loop variable name.	*/

    if ( (index = bb_get_hash( id)) == BB_HASH_ID_NOT_FOUND )
    {
        return BB_BAD_CLIENT;
    }

    for( i = 0; i < BB_MAX_IMP; i++ )
    {
	list[i] = board[i][index];
    }

    return BB_SUCCESS;
}


/*************************************************************************
**									**
**  bb_board_update() - Update the billboard on the disk.  Since these	**
**  only happen one at a time, update is for client,server pair.	**
**									**
*************************************************************************/
int
bb_board_update( client, server)
  int		client;		/* The index of the client to update.	*/
  int		server;		/* The index of the server to update.	*/
{
    int		num_imps;	/* The number of implementations.	*/
    FILE	*board_file;	/* Pointer to the board file.		*/
    long	seek_to;	/* The position to seek to in the file.	*/

    num_imps = bb_get_imp_cnt();

    if ( (client >= num_imps) || (server >= num_imps) )
    {
	return BB_FAILURE;
    }

    /*
    **  The word that we want to write is at a location server rows
    **  down and client rows across.  Since the output is written in
    **  integers, multiply this number by the sizeof an integer and
    **  that is where in the file to write the number.
    */
    seek_to = ((server * num_imps) + client) * sizeof(int);

    /*
    **  Open the board file.
    */
    if ( (board_file = fopen( BB_BOARD_FILE, "r+")) == NULL )
    {
	fprintf( stderr, "ERROR: Could not open board file for update.\n");
	return BB_FAILURE;
    }

    if ( fseek( board_file, seek_to, 0) != 0 )
    {
	fprintf( stderr, "ERROR: Could not seek in board file.\n");
	fclose( board_file);
	return BB_FAILURE;
    }

    if ( putw( (int)board[server][client], board_file) == 1 )
    {
	fprintf( stderr, "ERROR: Could not update board file.\n");
	fclose( board_file);
	return BB_FAILURE;
    }

    fclose( board_file);
    return BB_SUCCESS;
}
