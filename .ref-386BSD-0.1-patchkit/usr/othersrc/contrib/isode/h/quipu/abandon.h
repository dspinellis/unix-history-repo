/* abandon.h - */

/*
 * $Header: /f/osi/h/quipu/RCS/abandon.h,v 7.1 91/02/22 09:25:21 mrose Interim $
 *
 *
 * $Log:	abandon.h,v $
 * Revision 7.1  91/02/22  09:25:21  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:56:19  mrose
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


#ifndef QUIPUABANDON
#define QUIPUABANDON

#include "quipu/dap.h"

struct ds_abandon_arg {
    int aba_invokeid;
};

#endif
