/*
 ******************************************************************************
 *
 * Module: billboard_util.c
 *
 * Description: billboard_util.c
 *		Routines that handle the works for each feature.
 *
 * Functions:
 *	bb_call_set_unset(flag, bb_set_in_p)
 *	bb_set_unset_print_result(flag, bb_set_in_p, bb_set_out_p)
 *	bb_list(flag, bb_list_in_p)
 *	bb_list_print_result(flag, bb_list_in_p, bb_list_out_p)
 *	bb_change_passwd(bb_passwd_in_p)
 *	bb_print_vendor_info(bb_vendor_p)
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
 * Module Local Definitions
 ******************************************************************************
 */

/* 
 * local copy of password, saved for interactive use of the tool
 * i.e. the password entered when in interactive mode will be used for the
 *      rest of the interactive session
 */
static BB_passwd	_pwbuff;

/*
 ******************************************************************************
 * External Declarations
 ******************************************************************************
 */
extern CLIENT	*client_handle_p;
extern char	server_name[];
extern char	*_bb_salt;


/*
 ******************************************************************************
 * Function Declarations
 ******************************************************************************
 */
void bb_set_unset();
void bb_lists();
void bb_list_print_result();
void bb_set_unset_print_result();
void bb_change_passwd();
void bb_print_vendor_info();


/*
 ******************************************************************************
 *
 * Function: bb_call_set_unset
 *
 * Description: handles the operation to set test failed/passed
 *		The user request will be sent to the server via RPC,
 *		if password is needed, the user will be prompted for one and
 *		the request will be re-sent. 
 *
 * Input: 
 *	flag -- BB_SET or BB_UNSET
 *	bb_set_in_p -- input bb_set_in structure to RPC call
 *
 * Output:
 *
 * Returns:
 *
 * Side Effects:
 *
 * Notes:
 *
 ******************************************************************************
 */
void
bb_call_set_unset(flag, bb_set_in_p)
int		flag;
BB_set_in	*bb_set_in_p;
{
BB_set_out	*bb_set_out_p;

	/* 
	 * initialise the passowrd buffer, and copy exisiting password if 
	 * exists
	 */
	memset(bb_set_in_p->passwd, NUL, BB_PASSWD_LEN);
	strncpy(bb_set_in_p->passwd, _pwbuff, BB_PASSWD_LEN-1);

	/*
	 * issue the RPC call
	 */
	if (flag == BB_SET)
		bb_set_out_p= (BB_set_out *) bb_call_rpc(bb_set_1, bb_set_in_p);
	else if (flag == BB_UNSET)
		bb_set_out_p= (BB_set_out *) bb_call_rpc(bb_unset_1, 
							bb_set_in_p);
	else
		return;

	/*
	 * if NULL is returned, RPC call failed.
	 */
	if (bb_set_out_p == NULL) {
		clnt_perror(client_handle_p, server_name);
		return;
	}

	/*
	 * If password is requested, the user is prompted for one and
	 * the request is re-sent
	 */
	if (bb_set_out_p->status == BB_BAD_PASSWD) {
		/*
		 * gets password
		 */
		memset(bb_set_in_p->passwd, NUL, BB_PASSWD_LEN);
		strncpy(bb_set_in_p->passwd, 
			crypt(getpass("Password:"), _bb_salt), BB_PASSWD_LEN-1);
		/*
		 * save the password to be re-used if the tool is in
		 * interactive mode
		 */
		strncpy(_pwbuff, bb_set_in_p->passwd, BB_PASSWD_LEN);
		if (flag == BB_SET)
			bb_set_out_p= (BB_set_out *) bb_call_rpc(bb_set_1, 
					bb_set_in_p);
		else
			bb_set_out_p= (BB_set_out *) bb_call_rpc(bb_unset_1, 
					bb_set_in_p);

		if (bb_set_out_p == NULL) {
			clnt_perror(client_handle_p, server_name);
			return;
		}

		bb_set_unset_print_result(flag, bb_set_in_p, bb_set_out_p);
	} else 
		bb_set_unset_print_result(flag, bb_set_in_p, bb_set_out_p);

	return;
}


/*
 ******************************************************************************
 *
 * Function: bb_set_unset_print_result
 *
 * Description: print the result of RPC call for set unset of tests
 *
 * Input:
 *	flag: BB_SET or BB_UNSET
 *	bb_set_in_p: input bb_set_in structure to RPC call
 *	bb_set_out_p: RPC returned bb_set_out structure
 *
 * Output:
 *	Output goes to stdout, error goes to stderr.
 *
 * Returns:
 *
 * Side Effects:
 *
 * Notes:
 *
 ******************************************************************************
 */
void
bb_set_unset_print_result(flag, bb_set_in_p, bb_set_out_p)
int		flag;
BB_set_in	*bb_set_in_p;
BB_set_out	*bb_set_out_p;
{

	printf("\n");
	switch (bb_set_out_p->status) {
	case	BB_SUCCESS:
				if (flag == BB_SET)
					printf("The following client and server implementation is tested succesfully:\n");
				else
					printf("The following client and server implementation is set as being untested:\n");
				printf("Client:\n");
				bb_print_vendor_info(&bb_set_out_p->client);	
				printf("Server:\n");
				bb_print_vendor_info(&bb_set_out_p->server);	
				break;
	case	BB_FAILURE:
				fprintf(stderr, "Operation failed\.n");
				break;
	case	BB_BAD_CLIENT:
				fprintf(stderr, "Bad client identifier: %s.\n",
					bb_set_in_p->client);
				break;
	case	BB_BAD_SERVER:
				fprintf(stderr, "Bad server identifier: %s.\n",
					bb_set_in_p->server);
				break;
	case	BB_ALREADY_SET:
				fprintf(stderr, 
		"Client %s against server %s has already been set as tested.\n",
				bb_set_in_p->client, bb_set_in_p->server);
				break;
	case	BB_ALREADY_UNSET:
				fprintf(stderr, 
		"Client %s against server %s has already been set as untested.\n",
				bb_set_in_p->client, bb_set_in_p->server);
				break;

	case	BB_BAD_PASSWD:
				fprintf(stderr, "Bad password.\n");
				break;

	case	BB_BAD_PHASE:
				fprintf(stderr, "You have to complete testing against a Sun server before proceeding.\n");
				break;

	default:
				fprintf(stderr, "Unexpected error code %d.\n",
					bb_set_out_p->status);
				break;
	}

	return;
}

/*
 ******************************************************************************
 *
 * Function: bb_list
 *
 * Description: calls the RPC procedure, and prompted for password if needed.
 *
 * Input: 
 *	bb_list_in_p -- input bb_list_in structure to RPC call
 *
 * Output:
 *
 * Returns:
 *
 * Side Effects:
 *
 * Notes:
 *
 ******************************************************************************
 */
void
bb_list(flag, bb_list_in_p)
int	flag;
BB_list_in	*bb_list_in_p;
{
BB_list_out	*(*func)();
BB_list_out	*bb_list_out_p;

	/*
	 * set the RPC procedure
	 */
	switch  (flag) {
	case	BB_ALIST:
				func= bb_alist_1;
				break;
	case	BB_BLIST:
				func= bb_blist_1;
				break;
	case	BB_CLIST:
				func= bb_clist_1;
				break;
	case	BB_DLIST:
				func= bb_dlist_1;
				break;
	default:
				return;
	}

	/*
	 * initialise the password buffer, and copy existing one if exists
	 */
	memset(bb_list_in_p->passwd, NUL, BB_PASSWD_LEN);
	strncpy(bb_list_in_p->passwd, _pwbuff, BB_PASSWD_LEN-1);

	bb_list_out_p= (BB_list_out *) bb_call_rpc(func, bb_list_in_p);
	if (bb_list_out_p == NULL) {
		clnt_perror(client_handle_p, server_name);
		return;
	}

	if (bb_list_out_p->status == BB_BAD_PASSWD) {
		/*
		 * password is needed, so get one
		 */
		memset(bb_list_in_p->passwd, NUL, BB_PASSWD_LEN);
		strncpy(bb_list_in_p->passwd, 
			crypt(getpass("Password:"), _bb_salt), BB_PASSWD_LEN-1);
		/*
		 * save the password to be re-used if the tool is in
		 * interactive mode
		 */
		strncpy(_pwbuff, bb_list_in_p->passwd, BB_PASSWD_LEN);

		/* 
		 * issue the RPC call 
		 */
		bb_list_out_p= (BB_list_out *) bb_call_rpc(func, bb_list_in_p);
		if (bb_list_out_p == NULL) {
			clnt_perror(client_handle_p, server_name);
			return;
		}
		bb_list_print_result(flag, bb_list_in_p, bb_list_out_p);
	} else 
		bb_list_print_result(flag, bb_list_in_p, bb_list_out_p);

	return;
}

/*
 ******************************************************************************
 *
 * Function: bb_list_print_result
 *
 * Description: print result of RPC call to get list of clients or servers.
 *
 * Input:
 *	flag -- BB_ALIST, BB_BLIST, BB_CLIST or BB_DLIST
 *	bb_list_in_p -- input bb_list_in structure to RPC call
 *	bb_list_out_p -- RPC returned bb_list_out structure
 *
 * Output:
 *	Output goes to stdout, error goes to stderr.
 *
 * Returns:
 *
 * Side Effects:
 *
 * Notes:
 *
 ******************************************************************************
 */
void
bb_list_print_result(flag, bb_list_in_p, bb_list_out_p)
int		flag;
BB_list_in	*bb_list_in_p;
BB_list_out	*bb_list_out_p;
{
int	i;
int	data_len;
register BB_vendor	*vendor_p;

	printf("\n");
	switch (bb_list_out_p->status) {
	case	BB_SUCCESS:

				if ((data_len= bb_list_out_p->data.data_len) 
					> 0) {
					switch (flag) {
					case BB_ALIST:
	printf("List of servers successfully tested against client %s:\n", 
		bb_list_in_p->id);
							break;
					case BB_BLIST:
	printf("List of servers not tested against client %s:\n", 
		bb_list_in_p->id);
							break;
					case BB_CLIST:
	printf("List of clients successfully tested against server %s:\n", 
		bb_list_in_p->id);
							break;
					case BB_DLIST:
	printf("List of clients not tested against server %s:\n", 
		bb_list_in_p->id);
							break;
					}
					vendor_p= bb_list_out_p->data.data_val;

					for (i= 0; i < data_len; i++) {
						printf("%d)\n", i+1);
						bb_print_vendor_info(&vendor_p[i]);
					}
				} else {
					switch (flag) {
					case BB_ALIST:
	printf("No servers successfully tested against client %s.\n", 
		bb_list_in_p->id);
							break;
					case BB_BLIST:
	printf("All servers are successfully tested against client %s.\n", 
		bb_list_in_p->id);
							break;
					case BB_CLIST:
	printf("No clients successfully tested against server %s.\n", 
		bb_list_in_p->id);
							break;
					case BB_DLIST:
	printf("All clients are successfully tested against server %s.\n", 
		bb_list_in_p->id);
							break;
					}
				}
				break;
	case	BB_FAILURE:
				fprintf(stderr, "Operation failed.\n");
				break;
	case	BB_BAD_CLIENT:
				fprintf(stderr, "Bad client identifier: %s.\n",
					bb_list_in_p->id);
				break;
	case	BB_BAD_SERVER:
				fprintf(stderr, "Bad server identifier: %s.\n",
					bb_list_in_p->id);
				break;
	case	BB_BAD_PASSWD:
				fprintf(stderr, "Bad password.\n");
				break;

	default:
				fprintf(stderr, "Unknown error code %d.\n",
					bb_list_out_p->status);
				break;
	}

	return;
}


/*
 ******************************************************************************
 *
 * Function: bb_change_passwd
 *
 * Description: handles RPC call to change the password.  Make the RPC call
 *		and prints the status.
 *
 * Input: 
 *	bb_passwd_in_p -- input bb_passwd_in structure to RPC call
 *
 * Output:
 *	Output goes to stdout, error goes to stderr.
 *
 * Returns:
 *
 * Side Effects:
 *
 * Notes:
 *
 ******************************************************************************
 */
void
bb_change_passwd(bb_passwd_in_p)
BB_passwd_in	*bb_passwd_in_p;
{
int	*status_p;

	/*
	 * issue the RPC call
	 */
	if ((status_p= (int *)bb_call_rpc(bb_passwd_set_1, bb_passwd_in_p)) 
		!= NULL) {
		printf("\n");
		switch (*status_p) {
		case	BB_SUCCESS:
				printf("Password changed\n");
				break;
		case	BB_FAILURE:
				fprintf(stderr, "Operation failed.\n");
				break;
		case	BB_BAD_CLIENT:
				fprintf(stderr, "Bad client identifier: %s.\n",
					bb_passwd_in_p->client);
				break;
		case	BB_BAD_PASSWD:
				fprintf(stderr, "Bad password.\n");
				break;
		default:
				fprintf(stderr, "Unexpected error code %d.\n",
					*status_p);
				break;
		}
	} else
		clnt_perror(client_handle_p, server_name);

	return;
}

/*
 ******************************************************************************
 *
 * Function: bb_print_vendor_info
 *
 * Description: print vendor information from returned RPC call
 *
 * Input:
 *	bb_vendor_p -- vendor info
 *
 * Output:
 *	prints vendor info on stdout
 *
 * Returns:
 *
 * Side Effects:
 *
 * Notes:
 *
 ******************************************************************************
 */
void
bb_print_vendor_info(bb_vendor_p)
BB_vendor	*bb_vendor_p;
{
	printf("\tBooth: %d\n", bb_vendor_p->booth);
	printf("\tCompany: %s\n", bb_vendor_p->company);
	printf("\tImplementation: %s\n", bb_vendor_p->imp);
	printf("\tIdentifier: %s\n\n", bb_vendor_p->id);

	return;
}

