/*
 ******************************************************************************
 *
 * Module: bb_list.c
 *
 * Functions: 
 *	    bb_alist_1() - Return list of servers successfully tested against
 *	    bb_blist_1() - Return list of servers unsuccessfully tested against
 *	    bb_clist_1() - Return list of clients successfully tested against
 *	    bb_dlist_1() - Return list of clients unsuccessfully tested against
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
**  bb_alist_1() - Given the identifier get the list of servers this	**
**  client has successfully tested against.				**
**									**
*************************************************************************/
BB_list_out *
bb_alist_1( p_in)
  BB_list_in	*p_in;		/* The list input structure.		*/
{
    static BB_list_out	result;	/* The result of the list operation.	*/
    int			imp_cnt;/* The number of implementations.	*/
    int			i;	/* Nice loop variable name.		*/
    int			j;	/* The number of vendors found.		*/
    BB_row		list;	/* The list of tests in the database.	*/
    BB_co_data		codata;	/* The company data returned.		*/
    BB_vendor		vendors[BB_MAX_IMP];	/* Place to store output*/

    result.status = BB_SUCCESS;
    result.data.data_len = 0;

    /*
    **  If the machine being used to send this request is not one
    **  of the ones owned by this client group then a password must
    **  be specified.
    */
    if ( bb_check_ip( p_in->id) != BB_SUCCESS )
    {
	if ( (result.status=bb_check_passwd( p_in->id, p_in->passwd))
	     != BB_SUCCESS )
	{
	    result.status = BB_BAD_PASSWD;
	    return &result;
	}
    }

    /*
    **  Passed verification so get the data.
    */
    imp_cnt = bb_get_imp_cnt();
    if ( bb_get_servers( p_in->id, list) != BB_SUCCESS )
    {
	result.status = BB_FAILURE;
	return &result;
    }

    /*
    **  For each implementation check to see if the tests have
    **  been passed and if so get the company data for that index.
    */
    j = 0;
    for( i = 0; i < imp_cnt; i++ )
    {
	if ( list[i] == BB_BOARD_SET )
	{
	    /*
	    **  If can't get the company data just skip it.
	    */
	    if ( bb_get_codata( i, &codata) == BB_SUCCESS )
	    {
		vendors[j].booth = codata.booth;
		strncpy( vendors[j].company, codata.company,
			 BB_COMPANY_NAME_LEN);
		strncpy( vendors[j].imp, codata.imp, BB_IMP_NAME_LEN);
		strncpy( vendors[j].id, codata.id, BB_ID_NAME_LEN);
		j++;
	    }
	}
    }
    result.data.data_len = j;
    result.data.data_val = vendors;
    return &result;
}


/*************************************************************************
**									**
**  bb_blist_1() - Given the identifier get the list of servers this	**
**  client has not successfully tested against.				**
**									**
*************************************************************************/
BB_list_out *
bb_blist_1( p_in)
  BB_list_in	*p_in;		/* The list input structure.		*/
{
    static BB_list_out	result;	/* The result of the list operation.	*/
    int			imp_cnt;/* The number of implementations.	*/
    int			i;	/* Nice loop variable name.		*/
    int			j;	/* The number of vendors found.		*/
    BB_row		list;	/* The list of tests in the database.	*/
    BB_co_data		codata;	/* The company data returned.		*/
    BB_vendor		vendors[BB_MAX_IMP];	/* Place to store output*/

    result.status = BB_SUCCESS;
    result.data.data_len = 0;

    /*
    **  If the machine being used to send this request is not one
    **  of the ones owned by this client group then a password must
    **  be specified.
    */
    if ( bb_check_ip( p_in->id) != BB_SUCCESS )
    {
	if ( (result.status=bb_check_passwd( p_in->id, p_in->passwd))
	     != BB_SUCCESS )
	{
	    return &result;
	}
    }

    /*
    **  Passed verification so get the data.
    */
    imp_cnt = bb_get_imp_cnt();
    if ( bb_get_servers( p_in->id, list) != BB_SUCCESS )
    {
	result.status = BB_FAILURE;
	return &result;
    }

    /*
    **  For each implementation check to see if the tests have
    **  been passed and if so get the company data for that index.
    */
    j = 0;
    for( i = 0; i < imp_cnt; i++ )
    {
	if ( list[i] == BB_BOARD_UNSET )
	{
	    /*
	    **  If can't get the company data just skip it.
	    */
	    if ( bb_get_codata( i, &codata) == BB_SUCCESS )
	    {
		vendors[j].booth = codata.booth;
		strncpy( vendors[j].company, codata.company,
			 BB_COMPANY_NAME_LEN);
		strncpy( vendors[j].imp, codata.imp, BB_IMP_NAME_LEN);
		strncpy( vendors[j].id, codata.id, BB_ID_NAME_LEN);
		j++;
	    }
	}
    }
    result.data.data_len = j;
    result.data.data_val = vendors;
    return &result;
}


/*************************************************************************
**									**
**  bb_clist_1() - Given the identifier get the list of clients this	**
**  server has successfully tested against.				**
**									**
*************************************************************************/
BB_list_out *
bb_clist_1( p_in)
  BB_list_in	*p_in;		/* The list input structure.		*/
{
    static BB_list_out	result;	/* The result of the list operation.	*/
    int			imp_cnt;/* The number of implementations.	*/
    int			i;	/* Nice loop variable name.		*/
    int			j;	/* The number of vendors found.		*/
    BB_row		list;	/* The list of tests in the database.	*/
    BB_co_data		codata;	/* The company data returned.		*/
    BB_vendor		vendors[BB_MAX_IMP];	/* Place to store output*/

    result.status = BB_SUCCESS;
    result.data.data_len = 0;

    /*
    **  If the machine being used to send this request is not one
    **  of the ones owned by this client group then a password must
    **  be specified.
    */
    if ( bb_check_ip( p_in->id) != BB_SUCCESS )
    {
	if ( (result.status=bb_check_passwd( p_in->id, p_in->passwd))
	     != BB_SUCCESS )
	{
	    return &result;
	}
    }

    /*
    **  Passed verification so get the data.
    */
    imp_cnt = bb_get_imp_cnt();
    if ( bb_get_clients( p_in->id, list) != BB_SUCCESS )
    {
	result.status = BB_FAILURE;
	return &result;
    }

    /*
    **  For each implementation check to see if the tests have
    **  been passed and if so get the company data for that index.
    */
    j = 0;
    for( i = 0; i < imp_cnt; i++ )
    {
	if ( list[i] == BB_BOARD_SET )
	{
	    /*
	    **  If can't get the company data just skip it.
	    */
	    if ( bb_get_codata( i, &codata) == BB_SUCCESS )
	    {
		vendors[j].booth = codata.booth;
		strncpy( vendors[j].company, codata.company,
			 BB_COMPANY_NAME_LEN);
		strncpy( vendors[j].imp, codata.imp, BB_IMP_NAME_LEN);
		strncpy( vendors[j].id, codata.id, BB_ID_NAME_LEN);
		j++;
	    }
	}
    }
    result.data.data_len = j;
    result.data.data_val = vendors;
    return &result;
}


/*************************************************************************
**									**
**  bb_dlist_1() - Given the identifier get the list of clients this	**
**  server has not successfully tested against.				**
**									**
*************************************************************************/
BB_list_out *
bb_dlist_1( p_in)
  BB_list_in	*p_in;		/* The list input structure.		*/
{
    static BB_list_out	result;	/* The result of the list operation.	*/
    int			imp_cnt;/* The number of implementations.	*/
    int			i;	/* Nice loop variable name.		*/
    int			j;	/* The number of vendors found.		*/
    BB_row		list;	/* The list of tests in the database.	*/
    BB_co_data		codata;	/* The company data returned.		*/
    BB_vendor		vendors[BB_MAX_IMP];	/* Place to store output*/

    result.status = BB_SUCCESS;
    result.data.data_len = 0;

    /*
    **  If the machine being used to send this request is not one
    **  of the ones owned by this client group then a password must
    **  be specified.
    */
    if ( bb_check_ip( p_in->id) != BB_SUCCESS )
    {
	if ( (result.status=bb_check_passwd( p_in->id, p_in->passwd))
	     != BB_SUCCESS )
	{
	    return &result;
	}
    }

    /*
    **  Passed verification so get the data.
    */
    imp_cnt = bb_get_imp_cnt();
    if ( bb_get_clients( p_in->id, list) != BB_SUCCESS )
    {
	result.status = BB_FAILURE;
	return &result;
    }

    /*
    **  For each implementation check to see if the tests have
    **  been passed and if so get the company data for that index.
    */
    j = 0;
    for( i = 0; i < imp_cnt; i++ )
    {
	if ( list[i] == BB_BOARD_UNSET )
	{
	    /*
	    **  If can't get the company data just skip it.
	    */
	    if ( bb_get_codata( i, &codata) == BB_SUCCESS )
	    {
		vendors[j].booth = codata.booth;
		strncpy( vendors[j].company, codata.company,
			 BB_COMPANY_NAME_LEN);
		strncpy( vendors[j].imp, codata.imp, BB_IMP_NAME_LEN);
		strncpy( vendors[j].id, codata.id, BB_ID_NAME_LEN);
		j++;
	    }
	}
    }
    result.data.data_len = j;
    result.data.data_val = vendors;
    return &result;
}

