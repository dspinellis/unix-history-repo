/* read.h - read operation */

/*
 * $Header: /f/osi/h/quipu/RCS/read.h,v 7.1 91/02/22 09:26:05 mrose Interim $
 *
 *
 * $Log:	read.h,v $
 * Revision 7.1  91/02/22  09:26:05  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:56:43  mrose
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


#ifndef QUIPUREAD
#define QUIPUREAD

#include "quipu/commonarg.h"
#include "quipu/ds_error.h"
#include "quipu/dap.h"

	/* THIS SECTION GIVES OPERATION PARAMETERS */

struct ds_read_arg {
    CommonArgs rda_common;
    DN rda_object;
    EntryInfoSelection rda_eis;
};

struct ds_read_result {
    CommonResults rdr_common;
    EntryInfo rdr_entry;
};

#endif
