/* ryinitiator.h - include file for the generic interactive initiator */

/* 
 * $Header: /f/osi/others/lookup/RCS/ryinitiator.h,v 7.2 91/02/22 09:27:39 mrose Interim $
 *
 *
 * $Log:	ryinitiator.h,v $
 * Revision 7.2  91/02/22  09:27:39  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:04:30  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:56:43  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include "rosy.h"


static struct dispatch {
    char   *ds_name;
    int	    ds_operation;

    IFP	    ds_argument;
    modtyp	*ds_mod;	/* pointer to module table */
    int		ds_ind;		/* Index into module table */

    IFP	    ds_result;
    IFP	    ds_error;

    char   *ds_help;
};


void	adios (), advise ();
void	acs_adios (), acs_advise ();
void	ros_adios (), ros_advise ();

int	ryinitiator ();
