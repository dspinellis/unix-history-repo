/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)rpcv2.h	7.4 (Berkeley) %G%
 */

/*
 * Definitions for Sun RPC Version 2, from
 * "RPC: Remote Procedure Call Protocol Specification" RFC1057
 */

/* Version # */
#define	RPC_VER2	2

/* Authentication */
#define	RPCAUTH_NULL	0
#define	RPCAUTH_UNIX	1
#define	RPCAUTH_SHORT	2
#define	RPCAUTH_MAXSIZ	400
#define	RPCAUTH_UNIXGIDS 16

/* Rpc Constants */
#define	RPC_CALL	0
#define	RPC_REPLY	1
#define	RPC_MSGACCEPTED	0
#define	RPC_MSGDENIED	1
#define	RPC_PROGUNAVAIL	1
#define	RPC_PROGMISMATCH	2
#define	RPC_PROCUNAVAIL	3
#define	RPC_GARBAGE	4		/* I like this one */
#define	RPC_MISMATCH	0
#define	RPC_AUTHFAIL	1

/* Authentication failures */
#define	AUTH_BADCRED	1
#define	AUTH_REJECTCRED	2
#define	AUTH_BADVERF	3
#define	AUTH_REJECTVERF	4
#define	AUTH_TOOWEAK	5		/* Give em wheaties */

/* Sizes of rpc header parts */
#define	RPC_SIZ		24
#define	RPC_REPLYSIZ	28

/* RPC Prog definitions */
#define	RPCPROG_MNT	100005
#define	RPCMNT_VER1	1
#define	RPCMNT_MOUNT	1
#define	RPCMNT_DUMP	2
#define	RPCMNT_UMOUNT	3
#define	RPCMNT_UMNTALL	4
#define	RPCMNT_EXPORT	5
#define	RPCMNT_NAMELEN	255
#define	RPCMNT_PATHLEN	1024
#define	RPCPROG_NFS	100003
