/*
 * $Id: amq_clnt.c,v 5.2 90/06/23 22:20:16 jsp Rel $
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
 *	@(#)amq_clnt.c	5.1 (Berkeley) %G%
 */

#include "am.h"
#include "amq.h"

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

voidp
amqproc_null_1(argp, clnt)
	voidp argp;
	CLIENT *clnt;
{
	static char res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, AMQPROC_NULL, xdr_void, argp, xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return ((voidp)&res);
}


amq_mount_tree_p *
amqproc_mnttree_1(argp, clnt)
	amq_string *argp;
	CLIENT *clnt;
{
	static amq_mount_tree_p res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, AMQPROC_MNTTREE, xdr_amq_string, argp, xdr_amq_mount_tree_p, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


voidp
amqproc_umnt_1(argp, clnt)
	amq_string *argp;
	CLIENT *clnt;
{
	static char res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, AMQPROC_UMNT, xdr_amq_string, argp, xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return ((voidp)&res);
}


amq_mount_stats *
amqproc_stats_1(argp, clnt)
	voidp argp;
	CLIENT *clnt;
{
	static amq_mount_stats res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, AMQPROC_STATS, xdr_void, argp, xdr_amq_mount_stats, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


amq_mount_tree_list *
amqproc_export_1(argp, clnt)
	voidp argp;
	CLIENT *clnt;
{
	static amq_mount_tree_list res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, AMQPROC_EXPORT, xdr_void, argp, xdr_amq_mount_tree_list, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

int *
amqproc_setopt_1(argp, clnt)
	amq_setopt *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, AMQPROC_SETOPT, xdr_amq_setopt, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


amq_mount_info_list *
amqproc_getmntfs_1(argp, clnt)
	voidp argp;
	CLIENT *clnt;
{
	static amq_mount_info_list res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, AMQPROC_GETMNTFS, xdr_void, argp, xdr_amq_mount_info_list, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

