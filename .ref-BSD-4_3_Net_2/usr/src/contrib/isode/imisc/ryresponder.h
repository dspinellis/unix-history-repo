/* ryresponder.h - include file for the generic idempotent responder */

/* 
 * $Header: /f/osi/imisc/RCS/ryresponder.h,v 7.2 91/02/22 09:26:29 mrose Interim $
 *
 *
 * $Log:	ryresponder.h,v $
 * Revision 7.2  91/02/22  09:26:29  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:04:13  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:57:46  mrose
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
