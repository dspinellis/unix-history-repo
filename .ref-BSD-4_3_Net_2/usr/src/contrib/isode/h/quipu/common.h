/* common.h - */

/*
 * $Header: /f/osi/h/quipu/RCS/common.h,v 7.2 91/02/22 09:25:28 mrose Interim $
 *
 *
 * $Log:	common.h,v $
 * Revision 7.2  91/02/22  09:25:28  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/20  15:30:29  mrose
 * cjr
 * 
 * Revision 7.0  89/11/23  21:56:25  mrose
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


#ifndef COMH
#define COMH

#include "quipu/attrvalue.h"
#include "quipu/dsp.h"
#include "quipu/commonarg.h"
#include "quipu/bind.h"
#include "quipu/read.h"
#include "quipu/compare.h"
#include "quipu/abandon.h"
#include "quipu/list.h"
#include "quipu/ds_search.h"
#include "quipu/add.h"
#include "quipu/modify.h"
#include "quipu/modifyrdn.h"
#include "quipu/remove.h"
#include "quipu/ds_error.h"

typedef struct simple_creds
	{
	DN      usc_dn;
	int     usc_passwd_len;
	char    *usc_passwd;
	char    *usc_time1;
        char    *usc_time2;
	} USC;

#endif
