/*
 ******************************************************************************
 *
 * Module: bb_grid.c
 *
 * Functions: 
 *	    bb_grid_1()	- Return the information on successful testing.
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
**  bb_grid_1() - Return a list of all of the test results.  This is	**
**  just an indication of how the tests are coming in general, no info	**
**  on which companies have done what tests will be revield.		**
**									**
*************************************************************************/
BB_grid *
bb_grid_1()
{
    static BB_grid	grid;		/* The output grid structure.	*/

    /*
    **  XXXX-This will be filled in later.
    */
    grid.status == BB_SUCCESS;

    return &grid;
}
