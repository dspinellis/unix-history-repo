/*
 ******************************************************************************
 *
 * Module: bb_ip.c
 *
 * Functions: 
 *	    bb_get_ip_lines()	- Interpret a string to contain IP addresses.
 *				- Place the addresses in the IP array and
 *				- return the number found and index of first.
 *	    bb_check_ip()	- Check if the IP address of the caller is
 *				- in the IP list of his specified id name.
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

char	*strtok();
void	bb_get_ip();



/*************************************************************************
**									**
**  get_ip_lines() - Interpret the input 'line' to be a list of internet**
**  addresses.  Place them in the internet table and return the starting**
**  index of this block as well as the number of addresses in the block.**
**									**
*************************************************************************/
BB_ip		bb_ips[BB_MAX_IP];	/* IP Address array.		*/
int		bb_ip_count;		/* The number of IP's in array.	*/

void
bb_get_ip_lines( line, p_count, p_index)
  char	*line;		/* The input line of IP address tokens.		*/
  int	*p_count;	/* Output number of IP addresses in the block.	*/
  int	*p_index;	/* The starting index of the block of IP addrs.	*/
{
    char	*ip_addr;	/* Points to ip address string.		*/

    /*
    **  The index of this block of ip addresses is equal to the count.
    */
    *p_index = bb_ip_count;
    *p_count = 0;


    if ( (ip_addr = strtok( line, BB_IP_SEPARATOR)) == NULL )
    {
	fprintf( stderr, "ERROR: Null IP address list in data file.\n");
    }

    do
    {
	strncpy( bb_ips[bb_ip_count++], ip_addr, BB_IP_ADDR_LEN);
	(*p_count)++;
    }
    while ( (ip_addr = strtok( NULL, BB_IP_SEPARATOR)) != NULL );

    /*
    **  Strtok() leaves the \n on the last IP addres, take it off.
    */
    bb_ips[bb_ip_count -1][strlen(bb_ips[bb_ip_count-1])-1] = NUL;
}


/*************************************************************************
**									**
**  bb_check_ip() - This function checks to see if the client has an 	**
**  entry in the ip list which matches the ip address of this call.	**
**									**
*************************************************************************/
int
bb_check_ip( client)
  BB_id		client;		/* The clients identifier.		*/
{
    int		client_id;	/* The index of the client.		*/
    BB_co_data	codata;		/* The company data of the client.	*/
    int		i;		/* Nice loop variable name.		*/
    BB_ip	ip;		/* The ip address of the caller.	*/

    /*
    **  Get the client's id.
    */
    if ( (client_id = bb_get_hash( client)) == BB_HASH_ID_NOT_FOUND )
    {
	return BB_BAD_CLIENT;
    }

    /*
    **  Get the company data of the client.
    */
    if ( bb_get_codata( client_id, &codata) != BB_SUCCESS )
    {
	return BB_BAD_CLIENT;
    }

    /*
    **  Get the ip address of the caller and check it agains the
    **  clients list of ip addresses.
    */
    bb_get_ip( ip);
    for( i = codata.ip_idx; i < codata.ip_idx + codata.ip_cnt; i++ )
    {
	if ( strncmp( bb_ips[i], ip, BB_IP_ADDR_LEN) == 0 )
	{
	    return BB_SUCCESS;
	}
    }

    return BB_FAILURE;
}
