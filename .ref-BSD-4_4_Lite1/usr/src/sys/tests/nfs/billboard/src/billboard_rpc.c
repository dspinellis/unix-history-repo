/*
 ******************************************************************************
 *
 * Module: billboard_rpc.c
 *
 * Description: billboard_rpc.c
 *		Conatins routines that handle RPC calls.
 *
 * Functions:
 *	bb_call_rpc(func_p, arg_p)
 *	_get_handle()
 *
 * Notes:
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


/*
 ******************************************************************************
 * Manifest Constants
 ******************************************************************************
 */
#define	_SERVER_NAME		"BB_SERVER"	/* environment variable to
						   set the billboard server 
						   name */
#define	_SERVER_NAME_LEN	64

/*
 ******************************************************************************
 * Global Declarations
 ******************************************************************************
 */
CLIENT	*client_handle_p;		/* request client handle */
char	server_name[_SERVER_NAME_LEN]; 	/* name of the billboard server */

/*
 ******************************************************************************
 * Function Declarations
 ******************************************************************************
 */
static CLIENT 	*_get_handle();
char 		*bb_call_rpc();

/*
 ******************************************************************************
 *
 * Function: bb_call_rpc
 *
 * Description:
 *	Gets client handle and does the remote procedure call.
 *
 * Input:
 *	func_p -- address of remote procedure 
 *	arg_p -- input arguments to the remote procedure
 *
 * Output:
 *
 * Returns:
 *	result of the RPC call
 *
 * Side Effects:
 *
 * Notes:
 *
 ******************************************************************************
 */
char *
bb_call_rpc(func_p, arg_p)
char	*(*func_p)();
char	*arg_p;
{

	/*
	 * gets client handle
	 */
	if (client_handle_p == NULL)
		client_handle_p= _get_handle();

	/*
	 * do the RPC call
	 */
	return((char *)(*func_p)(arg_p, client_handle_p));
}


/*
 ******************************************************************************
 *
 * Function: _get_handle
 *
 * Description:
 *	Gets client handle for the RPC call.
 *	The server name may be specified in the environment      
 *	variable BB_SERVER, otherwise, the user will be prompted 
 *	for one.						
 *
 * Input:
 *
 * Output:
 *
 * Returns:
 *	client handle
 *
 * Side Effects:
 *
 * Notes:
 *
 ******************************************************************************
 */
static
CLIENT *
_get_handle()
{
char	*server_p;
char	buffer[BB_MAX_LINE_LEN];
char	*getenv();
CLIENT 	*cl_handle_p;

	/*
	 * gets the server name from the environment variable, if not specified
	 * the user will be prompted
	 */
	if ((server_p= getenv(_SERVER_NAME)) == NULL) {
		printf("Server name: ");
		gets(buffer);
		sscanf(buffer, "%s", server_name);		
	} else
		strcpy(server_name, server_p);

	/*
	 * gets a client handle
	 */
	if ((cl_handle_p= clnt_create(server_name, BILLBOARD_PROG, 
					BILLBOARD_VERS, "tcp")) == NULL) {
		clnt_pcreateerror(server_name);
		exit(1);
	}

	return(cl_handle_p);
}

