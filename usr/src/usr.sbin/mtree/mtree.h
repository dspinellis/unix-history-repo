/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mtree.h	5.6 (Berkeley) %G%
 */

#include <string.h>
#include <stdlib.h>

typedef struct _node {
	struct _node	*parent, *child;	/* up, down */
	struct _node	*prev, *next;		/* left, right */
	off_t	st_size;			/* size */
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

#define	F_CKSUM	0x001				/* check sum */
#define	F_DONE	0x002				/* directory done */
#define	F_GROUP	0x004				/* group */
#define	F_IGN	0x008				/* ignore */
#define	F_MAGIC	0x010				/* name has magic chars */
#define	F_MODE	0x020				/* mode */
#define	F_NLINK	0x040				/* number of links */
#define	F_OWNER	0x080				/* owner */
#define	F_SIZE	0x100				/* size */
#define	F_SLINK	0x200				/* link count */
#define	F_TYPE	0x400				/* file type */
#define	F_VISIT	0x800				/* file visited */
	u_short	flags;				/* items set */

	char	name[1];			/* file name (must be last) */
} NODE;

#define	RP(p)	(p->fts_path + 2)
