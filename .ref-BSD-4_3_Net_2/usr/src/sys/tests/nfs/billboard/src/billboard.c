/*
 ******************************************************************************
 *
 * Module: billboard.c
 *
 * Description: billboard.c
 *		Billboard is an RPC program that handles information about
 *		testsuites completions.  The module here is the user program
 *		that communicates with the billboard server.  This program
 *		allows user to update and modify information maintained
 *		by the server.  The features supported are (described below
 *		using the command line options):
 *		i) -s <client identifier> <server identifier>
 *		   	to set test between <client identifier> and 
 *		   	<server identifier> as successfully tested
 *		ii) -u <client identifier> <server identifier>
 *		   	to set test between <client identifier> and 
 *		   	<server identifier> as NOT successfully tested
 *		iii) -a <client identifier>
 *			to list server implementations that are successfully
 *			tested against client <client identifier>
 *		iv) -b <client identifier>
 *			to list server implementations that are NOT 
 *			successfully tested against client <client identifier>
 *		v) -c <server identifier>
 *			to list client implementations that are successfully
 *			tested against server <server identifier>
 *		vi) -d <server identifier>
 *			to list client implementations that are NOT 
 *			successfully tested against server <server identifier>
 *		vii) -p <identifier>
 *			to change the password of the <identifier> 
 *			implementation
 *		   
 *		where <client identifier> and <server identifier> is
 *		      identifier of the client and server implementation
 *		      respectively.
 *
 *		This user program also supports interactive interface whereby
 *		the user is presented with a list of options (same as the 
 *		features described above) to choose from.  User will be prompted
 *		for any additional data.
 *
 * Functions: 
 *	main(argc, argv)
 *	_cmd_line_option(argc, argv)
 *	_interactive_option()
 *	_bb_get_passwd(bb_passwd_in_p)
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
#include "common.h"
#include "protocol.h"


/*
 ******************************************************************************
 * Manifest Constants
 ******************************************************************************
 */
#define	_CONTROL_STR_SIZE	5	/* sscanf control string size */

/*
 ******************************************************************************
 * Macro Definitions
 ******************************************************************************
 */
/* print error messages */
#define	PRINT_ERROR()		{ 	fprintf(stderr, _Usage, argv[0]);\
					return(1);			\
				}


/*
 ******************************************************************************
 * Module Local Definitions
 ******************************************************************************
 */
/* usage message */
char	*_Usage=
"Usage: %s	[-s|-u client_idenitfier server_identifier]\n\
           	[-a|-b client_idenitfier]\n\
           	[-c|-d server_identifier]\n\
		[-p identifier]\n";
			
/* 
 *randomly picked salt key for password encryption 	
 * This may be modified if other algorithm is preferred 
 */
char	*_bb_salt= "kR";

/*
 ******************************************************************************
 * External Declarations
 ******************************************************************************
 */
extern char	*optarg;
extern int	optind;


/*
 ******************************************************************************
 * Function Declarations
 ******************************************************************************
 */
int		main();
static int 	_cmd_line_option();
static void 	_interactive_option();
static int 	_bb_get_passwd();


/*
 ******************************************************************************
 *
 * Function: main
 *
 * Description:
 * 	Invokes modules to handle command line interface or 
 * 	interactive according to the user invocation.	
 *
 * Input:
 *	argc
 *	argv
 *
 * Output:
 *	Output goes to stdout, error goes to stderr.
 *
 * Returns:
 *	1 -- operation failed
 *	0 -- operation succeeded.
 *
 * Side Effects:
 *
 * Notes:
 *
 ******************************************************************************
 */
int
main(argc, argv)
int	argc;
char	*argv[];
{
	if (argc > 1)
		return(_cmd_line_option(argc, argv));
	else
		_interactive_option();

	exit(0);
}

/*
 ******************************************************************************
 *
 * Function: _cmd_line_option
 *
 * Description:
 *	Parses command line input and perform the requested
 *	operation accordingly.				    
 *
 * Input:
 *	argc
 *	argv
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
static int
_cmd_line_option(argc, argv)
int	argc;
char	*argv[];
{
int		option;
BB_set_in	bb_set_in;
BB_list_in	bb_list_in;
BB_passwd_in	bb_passwd_in;

 	/* initialise termination of string buffers */
	bb_set_in.client[BB_ID_NAME_LEN-1]= bb_set_in.server[BB_ID_NAME_LEN-1]
						= '\0';
	bb_list_in.id[BB_ID_NAME_LEN-1]= '\0';

	/*
	 * parses the command line input, and make the remote procedure call
	 */
	if ((option= getopt(argc, argv, "s:u:a:b:c:d:p:")) != -1) {
		switch (option) {
		/* 
		 * gets client and server identifier and
		 * calls bb_call_set_unset to handle the
		 * remote procedure call
		 */
		case	's':	strncpy(bb_set_in.client, optarg, 
					BB_ID_NAME_LEN-1);
				strncpy(bb_set_in.server, argv[optind], 
					BB_ID_NAME_LEN-1);	
				bb_call_set_unset(BB_SET, &bb_set_in);
				break;

		case	'u':	strncpy(bb_set_in.client, optarg,
					BB_ID_NAME_LEN-1);
				strncpy(bb_set_in.server, argv[optind],
					BB_ID_NAME_LEN-1);	
				bb_call_set_unset(BB_UNSET, &bb_set_in);
				break;

		/* 
		 * gets client or server identifier and
		 * calls bb_list to handle the remote procedure
		 * call
		 */
		case	'a':	strncpy(bb_list_in.id, optarg,
					BB_ID_NAME_LEN-1);
				bb_list(BB_ALIST, &bb_list_in);
				break;

		case	'b':	strncpy(bb_list_in.id, optarg,
					BB_ID_NAME_LEN-1);
				bb_list(BB_BLIST, &bb_list_in);
				break;

		case	'c':	strncpy(bb_list_in.id, optarg,
					BB_ID_NAME_LEN-1);
				bb_list(BB_CLIST, &bb_list_in);
				break;

		case	'd':	strncpy(bb_list_in.id, optarg,
					BB_ID_NAME_LEN-1);
				bb_list(BB_DLIST, &bb_list_in);
				break;

		/* to change password */
		case	'p':
				strncpy(bb_passwd_in.client, optarg,
					BB_ID_NAME_LEN-1);
				if (_bb_get_passwd(&bb_passwd_in) == TRUE)
					bb_change_passwd(&bb_passwd_in);
				break;

		case	'?': 	PRINT_ERROR();
		}

	} else
		PRINT_ERROR();

	return(0);
}

/*
 ******************************************************************************
 *
 * Function: _interactive_option
 *
 * Description:
 *	Handles the interactive interface.  Promt user for inforamtion and 
 *	performs the operation selected by the user.
 *
 * Input:
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
static void 	
_interactive_option()
{
int	option= 0;
BB_id	client_id;
BB_id	server_id;
char	buffer[BB_MAX_LINE_LEN];
char	control_str[_CONTROL_STR_SIZE];

BB_set_in	bb_set_in;
BB_list_in	bb_list_in;
BB_passwd_in	bb_passwd_in;

	/* terminate the string buffers */
	bb_set_in.client[BB_ID_NAME_LEN-1]= bb_set_in.server[BB_ID_NAME_LEN-1]
						= '\0';
	bb_list_in.id[BB_ID_NAME_LEN-1]= '\0';

	sprintf(control_str, "%%%ds", BB_ID_NAME_LEN-1);

	/*
	 * requests for user input, and call the remote procedure to
	 * handle the operation
	 */
	for (;;) {
		/* print user interface options */
		printf("\n\
Options:\n\
-------:\n\
1) set test passed\n\
2) set test failed\n\
3) list servers successfully tested against\n\
4) list servers not tested against\n\
5) list clients successfully tested against\n\
6) list clients not tested against\n\
7) set/change password\n\
8) exit\n\n");

		printf("Enter option: ");
		gets(buffer);
		sscanf(buffer, "%d", &option);
		switch (option) {
		/* 
		 * gets client and server identifier and
		 * calls bb_call_set_unset to handle the
		 * remote procedure call
		 */
		case	1:
				printf("Client Identifier: ");
				gets(buffer);
				sscanf(buffer, control_str, bb_set_in.client);
				printf("Server Identifier: ");
				gets(buffer);
				sscanf(buffer, control_str, bb_set_in.server);
				bb_call_set_unset(BB_SET, &bb_set_in);
				break;

		case	2:
				printf("Client Identifier: ");
				gets(buffer);
				sscanf(buffer, control_str, bb_set_in.client);
				printf("Server Identifier: ");
				gets(buffer);
				sscanf(buffer, control_str, bb_set_in.server);
				bb_call_set_unset(BB_UNSET, &bb_set_in);
				break;

		/* 
		 * gets client or server identifier and
		 * calls bb_list to handle the remote procedure
		 * call
		 */
		case	3:
				printf("Client Identifier: ");
				gets(buffer);
				sscanf(buffer, control_str, bb_list_in.id);
				bb_list(BB_ALIST, &bb_list_in);
				break;

		case	4:
				printf("Client Identifier: ");
				gets(buffer);
				sscanf(buffer, control_str, bb_list_in.id);
				bb_list(BB_BLIST, &bb_list_in);
				break;

		case	5:
				printf("Server Identifier: ");
				gets(buffer);
				sscanf(buffer, control_str, bb_list_in.id);
				bb_list(BB_CLIST, &bb_list_in);
				break;

		case	6:
				printf("Server Identifier: ");
				gets(buffer);
				sscanf(buffer, control_str, bb_list_in.id);
				bb_list(BB_DLIST, &bb_list_in);
				break;

		/* to change password */
		case	7: 	
				printf("Identifier: ");
				gets(buffer);
				sscanf(buffer, control_str, bb_passwd_in.client);
				if (_bb_get_passwd(&bb_passwd_in) == TRUE)
					bb_change_passwd(&bb_passwd_in);
				break;

		case	8: 	return;

		default: 	
				printf("invalid option\n\n");
				break;
		}
	}
}

/*
 ******************************************************************************
 *
 * Function: _bb_get_passwd
 *
 * Description:
 *	Prompt for old and new passwords, and verifies the new one.
 *	Both passwords are then encrypted.
 *	We use Unix DES with fixed salt key for the password
 *      encryption for simplicity, you are welcome to use 
 *      your own algorithms.				   
 *
 * Input:
 *	bb_passwd_in_p -- input structure to RPC call
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
static bool_t
_bb_get_passwd(bb_passwd_in_p)
BB_passwd_in	*bb_passwd_in_p;
{
char	buffer[BB_MAX_LINE_LEN];

	/* initialised password buffers */
	memset(bb_passwd_in_p->old, NUL, BB_PASSWD_LEN);
	memset(bb_passwd_in_p->new, NUL, BB_PASSWD_LEN);
	memset(buffer, NUL, BB_MAX_LINE_LEN);

	/*
	 * get old and new password, and verify the new password
	 */
	strncpy(bb_passwd_in_p->old, crypt(getpass("Old password:"), _bb_salt),
		BB_PASSWD_LEN-1);
	strcpy(buffer, getpass("New password:"));
	if (strcmp(buffer, getpass("Retype new password: ")) != 0) {
		printf("Mismatch - password unchanged.\n");
		return(FALSE);
	} else 
		strncpy(bb_passwd_in_p->new, crypt(buffer, _bb_salt), 
			BB_PASSWD_LEN-1);
	return(TRUE);
}

