/* ds_ext.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/ds_ext.c,v 7.1 91/02/22 09:19:06 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/ds_ext.c,v 7.1 91/02/22 09:19:06 mrose Interim $
 *
 *
 * $Log:	ds_ext.c,v $
 * Revision 7.1  91/02/22  09:19:06  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/12/01  18:07:30  mrose
 * *** empty log message ***
 * 
 * Revision 7.0  89/11/23  21:50:30  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/common.h"

extern	LLog	* log_dsap;

subords_free(subp)
struct subordinate      *subp;
{
	if(subp == NULLSUBORD)
		return;
	subords_free(subp->sub_next);
        rdn_free(subp->sub_rdn); 
	free((char *)subp);
}

ems_free(emp)
struct entrymod *emp;
{
	if(emp == NULLMOD)
		return;
	ems_free(emp->em_next);
	as_free(emp->em_what);
	free((char *)emp);
}

aps_free(app)
struct access_point     *app;
{
	if(app == NULLACCESSPOINT)
		return;

	aps_free(app->ap_next);
	dn_free(app->ap_name);
	if (app->ap_address)
		psap_free (app->ap_address);

	free((char *)app);
}

crefs_free(crefp)
ContinuationRef crefp;
{
	if(crefp == NULLCONTINUATIONREF)
		return;
	crefs_free(crefp->cr_next);
	dn_free(crefp->cr_name);
	aps_free(crefp->cr_accesspoints);
	free((char *)crefp);
}

