/*
 ******************************************************************************
 *
 * Module: bb_passwd.c
 *
 * Functions: 
 *	    bb_read_passwords()	- Read or create the password file.
 *	    bb_check_passwd()	- Check a password against the passwd list.
 *	    bb_passwd_set_1()	- Set a password in the passwd list.
 *	    bb_passwd_update()	- Update the password list on disk.
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
**  bb_read_passwords() - If the password file exists read it and store	**
**  each line in the password array.  If it does not exist then create	**
**  it and fill in a blank entry for each implementation.		**
**									**
*************************************************************************/
BB_passwd	passwds[BB_MAX_IMP];	/* The password array.		*/

int
bb_read_passwords()
{
    FILE	*passwd_file;		/* Pointer to the passwd file.	*/
    int		i = 0;			/* Index into passwd array.	*/
    int		num_imps;		/* The number of implementations*/
    BB_passwd	empty_pass;		/* An empty password record.	*/

    num_imps = bb_get_imp_cnt();

    /*
    **  If the file exists read it into the password array.
    */
    if ( (passwd_file = fopen( BB_PASSWD_FILE, "r")) != NULL )
    {
	while ( fread( passwds[i], BB_PASSWD_LEN, 1, passwd_file) != 0 )
	{
	    passwds[i][BB_PASSWD_LEN-1] = NULL;
	    i++;
	    if ( i > num_imps )
	    {
		fprintf( stderr, "ERROR: Too many passwords in file.\n");
		fclose( passwd_file);
		return BB_FAILURE;
	    }
	}

	fclose( passwd_file);
	return BB_SUCCESS;
    }

    /*
    **  The file did not exist so create it.
    */
    if ( (passwd_file = fopen( BB_PASSWD_FILE, "w")) == NULL )
    {
	fprintf( stderr, "FAILED opening the password data file '%s'.\n",
		 BB_PASSWD_FILE);
	fclose( passwd_file);
	return BB_FAILURE;
    }

    /*
    **  Fill the file with empty passwords.
    */
    memset( empty_pass, NUL, BB_PASSWD_LEN);
    for( i = 0; i < num_imps; i++ )
    {
	if ( fwrite( empty_pass, BB_PASSWD_LEN, 1, passwd_file) != 1 )
	{
	    fprintf( stderr, "ERROR: Could not create password file.\n");
	    fclose( passwd_file);
	    return BB_FAILURE;
	}
	memset( passwds[i], NUL, BB_PASSWD_LEN);
    }

    fclose( passwd_file);
    return BB_SUCCESS;
}



/*************************************************************************
**									**
**  bb_check_passwd() - Check to see if the password matches the id.	**
**									**
*************************************************************************/
int
bb_check_passwd( id, passwd)
  BB_id		id;		/* The identifier of the passwd owner.	*/
  BB_passwd	passwd;		/* The password to verify.		*/
{
    int		index;		/* The index of the identifier.		*/

    if ( (index = bb_get_hash( id)) == BB_HASH_ID_NOT_FOUND )
    {
	return BB_BAD_CLIENT;
    }

    /*
    **  If no password exists return success.
    */
    if ( passwds[index][0] == NUL )
	return BB_SUCCESS;

    if ( strncmp( passwd, passwds[index], BB_PASSWD_LEN) != 0 )
    {
	return BB_BAD_PASSWD;
    }

    return BB_SUCCESS;
}



/*************************************************************************
**									**
**  bb_passwd_set_1() - Set the password for the client in the database.**
**  check to make sure the old password is correct first.		**
**									**
*************************************************************************/
int *
bb_passwd_set_1( p_passwd)
  BB_passwd_in	*p_passwd;	/* The passwd_in structure.		*/
{
    static int	result;		/* The result of the set operation.	*/
    int		index;		/* The index of the identifier.		*/

    /*
    **  bb_check_passwd() returns the reason for failure, so use that
    **  as the result of this function call.
    */
    if ( (result = bb_check_passwd( p_passwd->client, p_passwd->old))
	 != BB_SUCCESS )
    {
	return &result;
    }

    if ( (index = bb_get_hash( p_passwd->client)) == BB_HASH_ID_NOT_FOUND )
    {
	result = BB_BAD_CLIENT;
	return &result;
    }

    /*
    **  Copy the password to the data structure and update the data file.
    */
    strncpy( passwds[index], p_passwd->new, BB_PASSWD_LEN);
    result = bb_passwd_update( index);
    return &result;
}



/*************************************************************************
**									**
**  bb_passwd_update() - Write the password which has changed out to	**
**  password data file.							**
**									**
*************************************************************************/
int
bb_passwd_update( index)
  int		index;		/* The index of the changed password.	*/
{
    int		num_imps;	/* The number of implementations.	*/
    FILE	*passwd_file;	/* Pointer to the passwd file.		*/
    long	seek_to;	/* The position to seek to in the file.	*/

    num_imps = bb_get_imp_cnt();

    if ( index >= num_imps ) 
    {
	return BB_FAILURE;
    }

    /*
    **  The password that we want to write is at location index
    **  passwords into the file.  Multiply index by the size of
    **  a password and that is the offset.
    */
    seek_to = index * BB_PASSWD_LEN;

    /*
    **  Open the password file.
    */
    if ( (passwd_file = fopen( BB_PASSWD_FILE, "r+")) == NULL )
    {
	fprintf( stderr, "ERROR: Could not open password file for update.\n");
	return BB_FAILURE;
    }

    if ( fseek( passwd_file, seek_to, 0) != 0 )
    {
	fprintf( stderr, "ERROR: Could not seek in password file.\n");
	fclose( passwd_file);
	return BB_FAILURE;
    }

    if ( fwrite( passwds[index], BB_PASSWD_LEN, 1, passwd_file) != 1 )
    {
	fprintf( stderr, "ERROR: Could not update password file.\n");
	fclose( passwd_file);
	return BB_FAILURE;
    }

    fclose( passwd_file);
    return BB_SUCCESS;
}
