/* xdr_rdb.c  - xdr routines for Remote Debug interface to VxWorks  */

/* Copyright 1992 Free Software Foundation, Inc.

   This code was donated by Wind River Systems, Inc. */

/*
modification history
--------------------
01a,21mar90,llk  created using modification 01d of xdr_dbx.c.
*/

/*
DESCRIPTION
This module contains the eXternal Data Representation (XDR) routines
for the RDB interface for VxWorks.
*/

#include "vxWorks.h"
#include <rpc/rpc.h>
#include "xdr_rdb.h"

/* forward declarations */

bool_t
xdr_arg_type(xdrs, objp)
        XDR *xdrs;
        arg_type *objp;
{
        if (!xdr_enum(xdrs, (enum_t *)objp)) {
                return (FALSE);
        }
        return (TRUE);
}

bool_t
xdr_arg_value(xdrs, objp)
        XDR *xdrs;
        arg_value *objp;
{
        if (!xdr_arg_type(xdrs, &objp->type)) {
                return (FALSE);
        }
        switch (objp->type) {
        case T_BYTE:
                if (!xdr_char(xdrs, &objp->arg_value_u.v_byte)) {
                        return (FALSE);
                }
                break;
        case T_WORD:
                if (!xdr_short(xdrs, &objp->arg_value_u.v_word)) {
                        return (FALSE);
                }
                break;
        case T_INT:
                if (!xdr_int(xdrs, &objp->arg_value_u.v_int)) {
                        return (FALSE);
                }
                break;
        case T_FLOAT:
                if (!xdr_float(xdrs, &objp->arg_value_u.v_fp)) {
                        return (FALSE);
                }
                break;
        case T_DOUBLE:
                if (!xdr_double(xdrs, &objp->arg_value_u.v_dp)) {
                        return (FALSE);
                }
                break;
        case T_UNKNOWN:
                break;
        }
        return (TRUE);
}

bool_t
xdr_func_call(xdrs, objp)
        XDR *xdrs;
        func_call *objp;
{
        if (!xdr_int(xdrs, &objp->func_addr)) {
                return (FALSE);
        }
        if (!xdr_array(xdrs, (char **)&objp->args.args_val, (u_int *)&objp->args.args_len, MAX_FUNC_ARGS, sizeof(arg_value), xdr_arg_value)) {
                return (FALSE);
        }
        return (TRUE);
}

bool_t
xdr_arg_one(xdrs, objp)
        XDR *xdrs;
        arg_one *objp;
{
        if (!xdr_string(xdrs, objp, MAX_ARG_LEN)) {
                return (FALSE);
        }
        return (TRUE);
}




bool_t
xdr_arg_array(xdrs, objp)
        XDR *xdrs;
        arg_array *objp;
{
        if (!xdr_array(xdrs, (char **)&objp->arg_array_val, (u_int *)&objp->arg_array_len, MAX_ARG_CNT, sizeof(arg_one), xdr_arg_one)) {
                return (FALSE);
        }
        return (TRUE);
}

/*********************************************************************
*
* xdr_EVENT_TYPE -
*
*/

bool_t xdr_EVENT_TYPE(xdrs, objp)
    XDR *xdrs;
    EVENT_TYPE *objp;

    {
    if (!xdr_enum (xdrs, (enum_t *) objp))
	return (FALSE);
    return (TRUE);
    }

/*********************************************************************
*
* xdr_RDB_EVENT -
*
*/

bool_t xdr_RDB_EVENT (xdrs, objp)
    XDR *xdrs;
    RDB_EVENT *objp;

    {
    if (!xdr_int (xdrs, &objp->status))
	return (FALSE);
    if (!xdr_int (xdrs, &objp->taskId))
	return (FALSE);
    if (!xdr_EVENT_TYPE (xdrs, &objp->eventType))
	return (FALSE);
    if (!xdr_int (xdrs, &objp->sigType))
	return (FALSE);
    return (TRUE);
    }        

/*********************************************************************
*
* xdr_TASK_START -
*
*/

bool_t
xdr_TASK_START (xdrs, objp)
    XDR *xdrs;
    TASK_START *objp;

    {
    if (!xdr_int (xdrs, &objp->status))
	return (FALSE);
    if (!xdr_int (xdrs, &objp->pid))
	return (FALSE);
    return (TRUE);
    }


/*********************************************************************
*
* xdr_SYMBOL_ADDR -
*
*/

bool_t
xdr_SYMBOL_ADDR (xdrs, objp)
    XDR *xdrs;
    SYMBOL_ADDR *objp;

    {
    if (!xdr_int (xdrs, &objp->status))
	return (FALSE);
    if (!xdr_u_int (xdrs, &objp->addr))
	return (FALSE);
    return (TRUE);
    }

/*********************************************************************
*
* xdr_SOURCE_STEP -
*
*/

bool_t
xdr_SOURCE_STEP (xdrs, objp)
    XDR *xdrs;
    SOURCE_STEP *objp;

    {
    if (!xdr_int (xdrs, &objp->taskId))
	return (FALSE);
    if (!xdr_u_int (xdrs, &objp->startAddr))
	return (FALSE);
    if (!xdr_u_int (xdrs, &objp->endAddr))
	return (FALSE);
    return (TRUE);
    }
