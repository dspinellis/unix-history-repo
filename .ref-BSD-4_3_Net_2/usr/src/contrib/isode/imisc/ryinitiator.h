/* ryinitiator.h - include file for the generic interactive initiator */

/* 
 * $Header: /f/osi/imisc/RCS/ryinitiator.h,v 7.2 91/02/22 09:26:26 mrose Interim $
 *
 *
 * $Log:	ryinitiator.h,v $
 * Revision 7.2  91/02/22  09:26:26  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:04:12  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:57:44  mrose
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


#ifndef PEPSY_VERSION
#define PEPSY_VERSION 1
#endif
#include "rosy.h"


static struct dispatch {
    char   *ds_name;
    int	    ds_operation;

    IFP	    ds_argument;
    modtyp *ds_fr_mod;        /* pointer to table for arguement type */
    int     ds_fr_index;      /* index to entry in tables */

    IFP	    ds_result;
    IFP	    ds_error;

    char   *ds_help;
};


void	adios (), advise ();
void	acs_adios (), acs_advise ();
void	ros_adios (), ros_advise ();

int	ryinitiator ();
