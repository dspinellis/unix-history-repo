/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*
 * Copyright (c) 1991, Sun Microsystems Inc.
 */

/*
 *	nis_tags.h
 *
 *	This file contains the tags and statistics definitions. It is
 *	automatically included by nis.h
 */

#ifndef	_RPCSVC_NIS_TAGS_H
#define	_RPCSVC_NIS_TAGS_H

/* From: #pragma ident	"@(#)nis_tags.h	1.10	94/05/03 SMI" */
/* from file: zns_tags.h	1.7 Copyright (c) 1990 Sun Microsystems */
#pragma ident "$Id: nis_tags.h,v 1.2 1996/07/29 14:26:03 wpaul Exp $"

#ifdef	__cplusplus
extern "C" {
#endif

#ifndef ORIGINAL_DECLS
#define		NIS_DIR "data"
#endif

/* Lookup and List function flags */
#define	FOLLOW_LINKS	(1<<0)	/* Follow link objects 			*/
#define	FOLLOW_PATH	(1<<1)	/* Follow the path in a table 		*/
#define	HARD_LOOKUP	(1<<2)	/* Block until successful 		*/
#define	ALL_RESULTS	(1<<3)	/* Retrieve all results 		*/
#define	NO_CACHE	(1<<4)	/* Do not return 'cached' results 	*/
#define	MASTER_ONLY	(1<<5)	/* Get value only from master server	*/
#define	EXPAND_NAME	(1<<6)	/* Expand partitially qualified names	*/

/* Semantic modification for table operations flags */
#define	RETURN_RESULT	(1<<7)	/* Return resulting object to client    */
#define	ADD_OVERWRITE	(1<<8)	/* Allow overwrites on ADD		*/
#define	REM_MULTIPLE	(1<<9)	/* Allow wildcard deletes		*/
#define	MOD_SAMEOBJ	(1<<10)	/* Check modified object before write	*/
#define	ADD_RESERVED	(1<<11)	/* Spare ADD semantic			*/
#define	REM_RESERVED	(1<<12)	/* Spare REM semantic			*/
#ifdef ORIGINAL_DECLS
#define	MOD_RESERVED	(1<<13)	/* Spare MOD semantic			*/
#else
#define	MOD_EXCLUSIVE	(1<<13)	/* Modify no overwrite on modified keys	*/
#endif

/* Transport specific modifications to the operation */
#define	USE_DGRAM	(1<<16) /* Use a datagram transport 		*/
#define	NO_AUTHINFO	(1<<17) /* Don't bother attaching auth info	*/

/*
 * Declarations for "standard" NIS+ tags
 * State variable tags have values	0 - 2047
 * Statistic tags have values		2048 - 65535
 * User Tags have values		>2^16
 */
#define	TAG_DEBUG	1	/* set debug level 		*/
#define	TAG_STATS	2	/* Enable/disable statistics 	*/
#define	TAG_GCACHE	3	/* Flush the Group Cache	*/
#ifndef ORIGINAL_DECLS
#define	TAG_GCACHE_ALL	TAG_GCACHE
#endif
#define	TAG_DCACHE	4	/* Flush the directory cache	*/
#ifndef ORIGINAL_DECLS
#define	TAG_DCACHE_ONE	TAG_DCACHE
#endif
#define	TAG_OCACHE	5	/* Flush the Object Cache	*/
#define	TAG_SECURE	6	/* Set the security level 	*/
#ifndef ORIGINAL_DECLS
#define	TAG_TCACHE_ONE	7	/* Flush the table cache	*/
#define	TAG_DCACHE_ALL	8	/* Flush entire directory cache */
#define TAG_TCACHE_ALL	9	/* Flush entire table cache	*/
#define	TAG_GCACHE_ONE	10	/* Flush one group object	*/
#define	TAG_DCACHE_ONE_REFRESH 11 /* Flush and refresh one DO	*/
#endif

#define	TAG_OPSTATS	2048	/* NIS+ operations statistics   */
#define	TAG_THREADS	2049	/* Child process/thread status  */
#define	TAG_HEAP	2050	/* Heap usage statistics	*/
#define	TAG_UPDATES	2051	/* Updates to this service	*/
#define	TAG_VISIBLE	2052	/* First update that isn't replicated */
#define	TAG_S_DCACHE	2053	/* Directory cache statistics	*/
#define	TAG_S_OCACHE	2054	/* Object cache statistics	*/
#define	TAG_S_GCACHE	2055	/* Group cache statistics	*/
#define	TAG_S_STORAGE	2056	/* Group cache statistics	*/
#define	TAG_UPTIME	2057	/* Time that server has been up */
#ifndef ORIGINAL_DECLS
#define	TAG_DIRLIST	2058    /* Dir served by this server	*/
#define	TAG_NISCOMPAT	2059    /* Whether supports NIS compat mode */
#define	TAG_DNSFORWARDING 2060	/* Whether DNS forwarding supported*/
#define	TAG_SECURITY_LEVEL 2061 /* Security level of the server */
#define	TAG_ROOTSERVER	2062	/* Whether root server		*/
#endif

/*
 * Declarations for the Group object flags. Currently
 * there are only 3.
 */
#define	IMPMEM_GROUPS  1	/* Implicit Membership allowed 	*/
#define	RECURS_GROUPS  2	/* Recursive Groups allowed 	*/
#define	NEGMEM_GROUPS  4	/* Negative Groups allowed	*/

#ifdef	__cplusplus
}
#endif

#endif	/* _RPCSVC_NIS_TAGS_H */
