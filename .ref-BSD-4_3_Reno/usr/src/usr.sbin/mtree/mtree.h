/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)mtree.h	5.7 (Berkeley) 5/25/90
 */

#include <string.h>
#include <stdlib.h>

typedef struct _node {
	struct _node	*parent, *child;	/* up, down */
	struct _node	*prev, *next;		/* left, right */
	off_t	st_size;			/* size */
	time_t	st_mtime;			/* last modification time */
	u_long	cksum;				/* check sum */
	char	*slink;				/* symbolic link reference */
	uid_t	st_uid;				/* owner */
	gid_t	st_gid;				/* group */
#define	MBITS	(S_ISUID|S_ISGID|S_ISTXT|S_IRWXU|S_IRWXG|S_IRWXO)
	mode_t	st_mode;			/* mode */
	nlink_t	st_nlink;			/* link count */

#define	F_BLOCK	0x001				/* block special */
#define	F_CHAR	0x002				/* char special */
#define	F_DIR	0x004				/* directory */
#define	F_FIFO	0x008				/* fifo */
#define	F_FILE	0x010				/* regular file */
#define	F_LINK	0x020				/* symbolic link */
#define	F_SOCK	0x040				/* socket */
	u_short	type;				/* file type */

#define	F_CKSUM	0x0001				/* check sum */
#define	F_DONE	0x0002				/* directory done */
#define	F_GROUP	0x0004				/* group */
#define	F_IGN	0x0008				/* ignore */
#define	F_MAGIC	0x0010				/* name has magic chars */
#define	F_MODE	0x0020				/* mode */
#define	F_NLINK	0x0040				/* number of links */
#define	F_OWNER	0x0080				/* owner */
#define	F_SIZE	0x0100				/* size */
#define	F_SLINK	0x0200				/* link count */
#define	F_TIME	0x0400				/* modification time */
#define	F_TYPE	0x0800				/* file type */
#define	F_VISIT	0x1000				/* file visited */
	u_short	flags;				/* items set */

	char	name[1];			/* file name (must be last) */
} NODE;

#define	RP(p)	(p->fts_path + 2)
