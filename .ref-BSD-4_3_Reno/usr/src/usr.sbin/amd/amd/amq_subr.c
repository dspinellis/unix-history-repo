/*
 * $Id: amq_subr.c,v 5.2 90/06/23 22:19:20 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)amq_subr.c	5.1 (Berkeley) 6/29/90
 */
/*
 * Auxilliary routines for amq tool
 */

#include "am.h"
#include "amq.h"

#include <sys/param.h>

/*ARGSUSED*/
voidp
amqproc_null_1(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
	static char res;

	return (voidp) &res;
}

/*
 * Return a sub-tree of mounts
 */
/*ARGSUSED*/
amq_mount_tree_p *
amqproc_mnttree_1(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
	static am_node *mp;
	mp = find_ap(*(char **) argp);
	return (amq_mount_tree_p *) &mp;
}

/*
 * Unmount a single node
 */
/*ARGSUSED*/
voidp
amqproc_umnt_1(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
	static char res;
	am_node *mp = find_ap(*(char **) argp);
	if (mp)
		forcibly_timeout_mp(mp);

	return (voidp) &res;
}

/*
 * Return global statistics
 */
/*ARGSUSED*/
amq_mount_stats *
amqproc_stats_1(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
	return (amq_mount_stats *) &amd_stats;
}

/*
 * Return the entire tree of mount nodes
 */
/*ARGSUSED*/
amq_mount_tree_list *
amqproc_export_1(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
	static amq_mount_tree_list aml;

#ifdef oldcode
	static am_node **mvec;
	int i;
	int n = 0;

	mvec = (struct am_node **)
		xrealloc(mvec, (1+last_used_map) * sizeof(am_node *));
	for (i = last_used_map; i >= 0; --i) {
		am_node *mp = exported_ap[i];
		if (mp && (mp->am_flags & AMF_ROOT))
			mvec[n++] = mp;
	}

	aml.amq_mount_tree_list_val = (amq_mount_tree_p *) mvec;
	aml.amq_mount_tree_list_len = n;
#else
	aml.amq_mount_tree_list_val = (amq_mount_tree_p *) &exported_ap[0];
	aml.amq_mount_tree_list_len = 1;	/* XXX */
#endif /* oldcode */
	return &aml;
}

int *
amqproc_setopt_1(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
	static int rc;

	amq_setopt *opt = (amq_setopt *) argp;

	rc = 0;
	switch (opt->as_opt) {
	case AMOPT_DEBUG:
#ifdef DEBUG
		if (debug_option(opt->as_str))
			rc = EINVAL;
#else
		rc = EINVAL;
#endif /* DEBUG */
		break;

	case AMOPT_LOGFILE:
#ifdef not_yet
		if (switch_to_logfile(opt->as_str))
			rc = EINVAL;
#else
		rc = EACCES;
#endif /* not_yet */
		break;

	case AMOPT_XLOG:
		if (switch_option(opt->as_str))
			rc = EINVAL;
		break;

	case AMOPT_FLUSHMAPC:
		if (amd_state == Run) {
			plog(XLOG_INFO, "amq says flush cache");
			do_mapc_reload = 0;
		}
		break;
	}
	return &rc;
}

amq_mount_info_list *
amqproc_getmntfs_1(argp, rqstp)
voidp argp;
struct svc_req *rqstp;
{
extern qelem mfhead;
	return (amq_mount_info_list *) &mfhead;	/* XXX */
}

/*
 * XDR routines.
 */
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
xdr_amq_setopt(xdrs, objp)
	XDR *xdrs;
	amq_setopt *objp;
{
	if (!xdr_enum(xdrs, (enum_t *)&objp->as_opt)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->as_str, AMQ_STRLEN)) {
		return (FALSE);
	}
	return (TRUE);
}

/*
 * More XDR routines  - Should be used for OUTPUT ONLY.
 */
bool_t
xdr_amq_mount_tree(xdrs, objp)
	XDR *xdrs;
	amq_mount_tree *objp;
{
	am_node *mp = (am_node *) objp;

	if (!xdr_amq_string(xdrs, &mp->am_mnt->mf_info)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, &mp->am_path)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, mp->am_link ? &mp->am_link : &mp->am_mnt->mf_mount)) {
		return (FALSE);
	}
	if (!xdr_amq_string(xdrs, &mp->am_mnt->mf_ops->fs_type)) {
		return (FALSE);
	}
	if (!xdr_long(xdrs, &mp->am_stats.s_mtime)) {
		return (FALSE);
	}
	if (!xdr_u_short(xdrs, &mp->am_stats.s_uid)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &mp->am_stats.s_getattr)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &mp->am_stats.s_lookup)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &mp->am_stats.s_readdir)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &mp->am_stats.s_readlink)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &mp->am_stats.s_statfs)) {
		return (FALSE);
	}
	if (!xdr_pointer(xdrs, (char **)&mp->am_osib, sizeof(amq_mount_tree), xdr_amq_mount_tree)) {
		return (FALSE);
	}
	if (!xdr_pointer(xdrs, (char **)&mp->am_child, sizeof(amq_mount_tree), xdr_amq_mount_tree)) {
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
xdr_amq_mount_tree_list(xdrs, objp)
	XDR *xdrs;
	amq_mount_tree_list *objp;
{
	 if (!xdr_array(xdrs, (char **)&objp->amq_mount_tree_list_val, (u_int *)&objp->amq_mount_tree_list_len, ~0, sizeof(amq_mount_tree_p), xdr_amq_mount_tree_p)) {
		return (FALSE);
	}
	return (TRUE);
}

xdr_amq_mount_info_qelem(xdrs, qhead)
	XDR *xdrs;
	qelem *qhead;
{
	/*
	 * Compute length of list
	 */
	mntfs *mf;
	u_int len = 0;
	for (mf = LAST(mntfs, qhead); mf != HEAD(mntfs, qhead); mf = PREV(mntfs, mf)) {
		if (!(mf->mf_ops->fs_flags & FS_AMQINFO))
			continue;
		len++;
	}
	xdr_u_int(xdrs, &len);

	/*
	 * Send individual data items
	 */
	for (mf = LAST(mntfs, qhead); mf != HEAD(mntfs, qhead); mf = PREV(mntfs, mf)) {
		int up;
		if (!(mf->mf_ops->fs_flags & FS_AMQINFO))
			continue;

		if (!xdr_amq_string(xdrs, &mf->mf_ops->fs_type)) {
			return (FALSE);
		}
		if (!xdr_amq_string(xdrs, &mf->mf_mount)) {
			return (FALSE);
		}
		if (!xdr_amq_string(xdrs, &mf->mf_info)) {
			return (FALSE);
		}
		if (!xdr_amq_string(xdrs, &mf->mf_server->fs_host)) {
			return (FALSE);
		}
		if (!xdr_int(xdrs, &mf->mf_error)) {
			return (FALSE);
		}
		if (!xdr_int(xdrs, &mf->mf_refc)) {
			return (FALSE);
		}
		if (mf->mf_server->fs_flags & FSF_ERROR)
			up = 0;
		else switch (mf->mf_server->fs_flags & (FSF_DOWN|FSF_VALID)) {
		case FSF_DOWN|FSF_VALID: up = 0; break;
		case FSF_VALID: up = 1; break;
		default: up = -1; break;
		}
		if (!xdr_int(xdrs, &up)) {
			return (FALSE);
		}
	}
	return (TRUE);
}
