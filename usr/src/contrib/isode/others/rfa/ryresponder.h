/* ryresponder.h - include file for the generic idempotent responder 
 *
 * $Header: /f/osi/others/rfa/RCS/ryresponder.h,v 7.2 91/02/22 09:28:33 mrose Interim $
 *
 * $Log:	ryresponder.h,v $
 * Revision 7.2  91/02/22  09:28:33  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/14  13:55:08  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:10:08  ow
 * Initial revision
 * 
 */

/*
 *                              NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#include "rosy.h"
#include "logger.h"


static struct dispatch {
    char   *ds_name;
    int	    ds_operation;

    IFP	    ds_vector;
};


void	adios (), advise ();
void	acs_advise ();
void	ros_adios (), ros_advise ();
void	ryr_advise ();

int	ryresponder ();


extern int  debug;
