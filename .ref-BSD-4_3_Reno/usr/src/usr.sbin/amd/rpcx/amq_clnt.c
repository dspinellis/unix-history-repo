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
 *	@(#)amq_clnt.c	5.1 (Berkeley) 7/19/90
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

