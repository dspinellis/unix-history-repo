/*
 ******************************************************************************
 *
 * Module: bb_codata.c
 *
 * Description: 
 *
 * Functions: 
 *	    bb_put_codata()	- Place a company data entry in the structure.
 *	    bb_get_codata()	- Get a company data entry from the structure.
 *	    bb_get_imp_cnt()	- Return the number of implementations.
 *	    bb_read_companies()	- Read the company data file.
 *	    bb_read_1_codata()	- Read a single company data from the file.
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


static int		co_cnt;	/* The number of company data entries.	*/
static BB_co_data	codata[BB_MAX_IMP];/* The company data records.	*/

void bb_get_ip_lines();
char *strtok();


int
bb_put_codata( p_codata)
  BB_co_data	*p_codata;	/* The company data to put in the table	*/
{
    if ( co_cnt + 1 >= BB_MAX_IMP )
    {
	fprintf( stderr, "FAILURE can't install company data record: %s.\n",
		 p_codata->company);
	return BB_FAILURE;
    }

    memcpy( &codata[ co_cnt], p_codata, sizeof( *p_codata));
    return co_cnt++;
}

int
bb_get_codata( index, p_codata)
  int		index;		/* The index of the data to retrieve.	*/
  BB_co_data	*p_codata;	/* Space to put the data.		*/
{
    if ( index >= co_cnt )
    {
	return BB_FAILURE;
    }

    memcpy( p_codata, &codata[ index], sizeof( *p_codata));
    return BB_SUCCESS;
}

int
bb_get_imp_cnt()
{
    return co_cnt;
}


/*************************************************************************
**									**
**  bb_read_companies() - Read the file containing the company data.	**
**  For each entry create a company data record, and add an entry in the**
**  hash table.  The company data file also contains IP addresses, which**
**  are stored in the ip_table.						**
**									**
*************************************************************************/
int
bb_read_companies()
{
    int		status;		/* Status of function call returns.	*/
    int		index;		/* Index of the company data record.	*/
    BB_co_data	cdata;		/* Space for the readding of records.	*/

    while( (status = bb_read_1_codata( &cdata)) != BB_END_OF_FILE )
    {
	/*
	**  If the read was not successful exit.
	*/
	if ( status != BB_SUCCESS )
	{
	    fprintf( stderr, "ABORTING: Can't read company data file.\n");
	    return BB_FAILURE;
	}
	
	/*
	**  Put the company data record in the array.
	*/
	if ( (index = bb_put_codata( &cdata)) == BB_FAILURE )
	{
	    fprintf( stderr, "ABORTING: Not enough company data space.\n");
	    return BB_FAILURE;
	}

	/*
	**  Fill in the hash record for the company data record.
	*/
	if ( bb_put_hash( codata[index].id, index) != BB_SUCCESS )
	{
	    fprintf( stderr, "ABORTING: Unable to hash company: %s.\n",
		     cdata.company);
	    return BB_FAILURE;
	}
    }

    /*
    **  Read all of the company data with no problems.
    */
    return BB_SUCCESS;
}


/*************************************************************************
**									**
**  bb_read_1_codata() - Read one company data record from the co data	**
**  file.  If the file is not open then open it and start readding. The	**
**  file is ordered in the same manner as the BB_co_data structure.  IP	**
**  means a set of internet addresses are on the same line.  Each of	**
**  these addresses belongs to the companies that follow up to the next	**
**  IP specification.							**
**									**
*************************************************************************/
static FILE	*file = NULL;	/* File pointer to company data file.	*/

int
bb_read_1_codata( p_codata)
  BB_co_data	*p_codata;	/* Output a company data record.	*/
{
    static int	ip_count;	/* Number of IP addresses for company.	*/
    static int	ip_index;	/* Place where IP addresses start.	*/
    bool_t	done = FALSE;	/* Done readding this codata record.	*/
    char	line[BB_MAX_LINE_LEN];	/* Input line buffer.		*/

    /*
    **  If the file is not open then it neads to be.
    */
    if ( file == NULL ) 
    {
	if ( (file = fopen( BB_CODATA_FILE, "r")) == NULL )
	{
	    fprintf( stderr, "FAILED opening the company data file '%s'.\n",
		     BB_CODATA_FILE);
	    return BB_FAILURE;
	}

	ip_count = ip_index = 0;
    }

    while ( done != TRUE )
    {
	do
	{
	    if ( fgets( line, BB_MAX_LINE_LEN, file) == NULL )
	    {
		return BB_END_OF_FILE;
	    }
	}
	while ( line[0] == BB_COMMENT_DESIGNATOR );

	switch ( line[BB_DES_CHAR] )
	{
	    case BB_IP_DESIGNATOR:
		bb_get_ip_lines( &line[BB_DES_START],
				 &ip_count, &ip_index);
		break;
	    case BB_CO_DESIGNATOR:
		line[strlen(line)-1] = NUL;
		strncpy( p_codata->company, &line[BB_DES_START],
			 BB_COMPANY_NAME_LEN);
		break;
	    case BB_IMP_DESIGNATOR:
		line[strlen(line)-1] = NUL;
		strncpy( p_codata->imp, &line[BB_DES_START],
			 BB_IMP_NAME_LEN);
		break;
	    case BB_ID_DESIGNATOR:
		line[strlen(line)-1] = NUL;
		strncpy( p_codata->id, &line[BB_DES_START],
			 BB_ID_NAME_LEN);
		break;
	    case BB_BOOTH_DESIGNATOR:
		p_codata->booth = atoi( &line[BB_DES_START]);
		break;
	    case BB_FLAGS_DESIGNATOR:
		p_codata->flags = atoi( &line[BB_DES_START]);
		break;
	    case BB_END_DESIGNATOR:
		done = TRUE;
		break;
	    default :
		if ( line[0] != '\n' )
		{
		    fprintf( stderr, "ERROR: Invalid field designator '%s'.\n",
			    line);
		}
		break;
	}
    }
    p_codata->ip_cnt = ip_count;
    p_codata->ip_idx = ip_index;

    return BB_SUCCESS;
}

