/*
 * $Id: amq_xdr.c,v 5.2 90/06/23 22:20:14 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)amq_xdr.c	5.1 (Berkeley) %G%
 */

#include "am.h"
#include "amq.h"


bool_t
xdr_amq_string(xdrs, objp)
	XDR *xdrs;
	amq_string *objp;
{
	if (!xdr_string(xdrs, objp, AMQ_STRLEN)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_time_type(xdrs, objp)
	XDR *xdrs;
	time_type *objp;
{
	if (!xdr_long(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_amq_mount_tree(xdrs, objp)
	XDR *xdrs;
	amq_mount_tree *objp;
{
	if (!xdr_amq_string(xdrs, &objp->mt_mountinfo)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, &objp->mt_directory)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, &objp->mt_mountpoint)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, &objp->mt_type)) {
		return (FALSE);
	}
	if (!xdr_time_type(xdrs, &objp->mt_mounttime)) {
		return (FALSE);
	}
	if (!xdr_u_short(xdrs, &objp->mt_mountuid)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->mt_getattr)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->mt_lookup)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->mt_readdir)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->mt_readlink)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->mt_statfs)) {
		return (FALSE);
	}
	if (!xdr_pointer(xdrs, (char **)&objp->mt_next, sizeof(amq_mount_tree), xdr_amq_mount_tree)) {
		return (FALSE);
	}
	if (!xdr_pointer(xdrs, (char **)&objp->mt_child, sizeof(amq_mount_tree), xdr_amq_mount_tree)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_amq_mount_tree_p(xdrs, objp)
	XDR *xdrs;
	amq_mount_tree_p *objp;
{
	if (!xdr_pointer(xdrs, (char **)objp, sizeof(amq_mount_tree), xdr_amq_mount_tree)) {
		return (FALSE);
	}
	return (TRUE);
}



bool_t
xdr_amq_mount_info(xdrs, objp)
	XDR *xdrs;
	amq_mount_info *objp;
{
	if (!xdr_amq_string(xdrs, &objp->mi_type)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, &objp->mi_mountpt)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, &objp->mi_mountinfo)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, &objp->mi_fserver)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->mi_error)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->mi_refc)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->mi_up)) {
		return (FALSE);
	}
	return (TRUE);
}



bool_t
xdr_amq_mount_info_list(xdrs, objp)
	XDR *xdrs;
	amq_mount_info_list *objp;
{
	if (!xdr_array(xdrs, (char **)&objp->amq_mount_info_list_val, (u_int *)&objp->amq_mount_info_list_len, ~0, sizeof(amq_mount_info), xdr_amq_mount_info)) {
		return (FALSE);
	}
	return (TRUE);
}



bool_t
xdr_amq_mount_tree_list(xdrs, objp)
	XDR *xdrs;
	amq_mount_tree_list *objp;
{
	if (!xdr_array(xdrs, (char **)&objp->amq_mount_tree_list_val, (u_int *)&objp->amq_mount_tree_list_len, ~0, sizeof(amq_mount_tree_p), xdr_amq_mount_tree_p)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_amq_mount_stats(xdrs, objp)
	XDR *xdrs;
	amq_mount_stats *objp;
{
	if (!xdr_int(xdrs, &objp->as_drops)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->as_stale)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->as_mok)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->as_merr)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->as_uerr)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_amq_opt(xdrs, objp)
	XDR *xdrs;
	amq_opt *objp;
{
	if (!xdr_enum(xdrs, (enum_t *)objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_amq_setopt(xdrs, objp)
	XDR *xdrs;
	amq_setopt *objp;
{
	if (!xdr_amq_opt(xdrs, &objp->as_opt)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, &objp->as_str)) {
		return (FALSE);
	}
	return (TRUE);
}


