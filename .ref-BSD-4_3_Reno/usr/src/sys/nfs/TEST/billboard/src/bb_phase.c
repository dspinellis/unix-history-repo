/*
 ******************************************************************************
 *
 * Module: bb_phase.c
 *
 * Functions: 
 *	    bb_read_phases()	- Read or create the phases file.
 *	    bb_phase_update()	- Update the phases file on disk.
 *	    bb_set_phase()	- Add the specified flags to a phase.
 *	    bb_unset_phase()	- Remove the specified flags from a phase.
 *	    bb_phase_ok()	- Check if the phase is ok to make changes.
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
**  bb_read_phases - If the phases file exists read the entries into 	**
**  the phases array.  Otherwise create the file with 0'ed entries.	**
**									**
*************************************************************************/
BB_phase	phases[BB_MAX_IMP];	/* The phases array.		*/

int
bb_read_phases()
{
    FILE	*phase_file;		/* Pointer to the phase file.	*/
    int		i = 0;			/* Index into phase array.	*/
    int		num_imps;		/* The number of implementations*/
    char	line[BB_MAX_LINE_LEN];	/* Space for the input line.	*/

    num_imps = bb_get_imp_cnt();

    /*
    **  If the file exists read it into the phase array.
    */
    if ( (phase_file = fopen( BB_PHASE_FILE, "r")) != NULL )
    {
	for ( i = 0; i < num_imps; i++ )
	{
	    if ( (phases[i] = getw( phase_file)) == (u_char)EOF )
	    {
		fprintf( stderr, "ERROR: Phase data file incomplete.\n");
		return BB_FAILURE;
	    }
	}

	fclose( phase_file);
	return BB_SUCCESS;
    }

    /*
    **  The file did not exist so create it.
    */
    if ( (phase_file = fopen( BB_PHASE_FILE, "w")) == NULL )
    {
	fprintf( stderr, "FAILED opening the phase data file '%s'.\n",
		 BB_PHASE_FILE);
	fclose( phase_file);
	return BB_FAILURE;
    }

    /*
    **  Fill the file with zeroed phases.
    */
    for( i = 0; i < num_imps; i++ )
    {
	if ( putw( (int)BB_BOARD_UNSET, phase_file) == 1 )
	{
	    fprintf( stderr, "FAILED writting new phase file.\n");
	    fclose( phase_file);
	    return BB_FAILURE;
	}
    }

    fclose( phase_file);
    return BB_SUCCESS;
}



/*************************************************************************
**									**
**  bb_phase_update() - The phase of the client has changed and should	**
**  be written to disk.  						**
**									**
*************************************************************************/
int
bb_phase_update( client)
  int		client;		/* The client with the change of phase.	*/
{
    int		num_imps;	/* The number of implementations.	*/
    FILE	*phase_file;	/* Pointer to the phase file.		*/
    long	seek_to;	/* The position to seek to in the file.	*/

    num_imps = bb_get_imp_cnt();

    if ( client >= num_imps ) 
    {
	return BB_FAILURE;
    }

    /*
    **  The word that we want to write is at the location client
    **  integers into the file.  Take the client index and multiply
    **  that by the size of the integer and that is the offset.
    */
    seek_to = client * sizeof(int);

    /*
    **  Open the phase file.
    */
    if ( (phase_file = fopen( BB_PHASE_FILE, "r+")) == NULL )
    {
	fprintf( stderr, "ERROR: Could not open phase file for update.\n");
	return BB_FAILURE;
    }

    if ( fseek( phase_file, seek_to, 0) != 0 )
    {
	fprintf( stderr, "ERROR: Could not seek in phase file.\n");
	fclose( phase_file);
	return BB_FAILURE;
    }

    if ( putw( (int)phases[client], phase_file) == 1 )
    {
	fprintf( stderr, "ERROR: Could not update phase file.\n");
	fclose( phase_file);
	return BB_FAILURE;
    }

    fclose( phase_file);
    return BB_SUCCESS;
}


/*************************************************************************
**									**
**  bb_set_phase() - Change the phase of a client.  This adds the	**
**  flags specified to the phase in the phase list.			**
**									**
*************************************************************************/
int
bb_set_phase( client, phase)
  BB_id		client;		/* The name of the client to change.	*/
  int		phase;		/* The phase to add to the client.	*/
{
    int		client_id;	/* The index of the client.		*/

    /*
    **  Get the client's id.
    */
    if ( (client_id = bb_get_hash( client)) == BB_HASH_ID_NOT_FOUND )
    {
	return BB_BAD_CLIENT;
    }

    /*
    **  Or the bits together to turn them on.  The bits are saying OOO WEEE.
    */
    phases[client_id] |= phase;
    
    /*
    **  If the phase cannot be written to disk, return an error.
    */
    return bb_phase_update( client_id);
}


/*************************************************************************
**									**
**  bb_unset_phase() - Change the phase of a client.  This deletes the	**
**  flags specified from the phase in the phase list.			**
**									**
*************************************************************************/
int
bb_unset_phase( client, phase)
  BB_id		client;		/* The name of the client to change.	*/
  int		phase;		/* The phase to add to the client.	*/
{
    int		client_id;	/* The index of the client.		*/

    /*
    **  Get the client's id.
    */
    if ( (client_id = bb_get_hash( client)) == BB_HASH_ID_NOT_FOUND )
    {
	return BB_BAD_CLIENT;
    }

    /*
    **  Or the bits together to turn them on.  The bits are saying OOO WEEE.
    */
    phases[client_id] &= ~phase;
    
    /*
    **  If the phase cannot be written to disk, return an error.
    */
    return bb_phase_update( client_id);
}



/*************************************************************************
**									**
**  bb_phase_ok() - Check to see if the phase is ok for the change to	**
**  the board that will be made.					**
**									**
*************************************************************************/
int
bb_phase_ok( client)
  BB_id		client;		/* The client identifier.		*/
{
    int		client_id;	/* The index of the client.		*/

    /*
    **  Get the client's id.
    */
    if ( (client_id = bb_get_hash( client)) == BB_HASH_ID_NOT_FOUND )
    {
	return BB_BAD_CLIENT;
    }

    if ( phases[client_id] & BB_SUN_PHASE )
	return BB_SUCCESS;
    else
	return BB_FAILURE;
}
