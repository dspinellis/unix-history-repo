/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mtree.h	5.3 (Berkeley) %G%
 */

#include <string.h>
#include <stdlib.h>

typedef struct _info {
#define	F_BLOCK	0x001				/* block special */
#define	F_CHAR	0x002				/* char special */
#define	F_DIR	0x004				/* directory */
#define	F_FILE	0x008				/* regular file */
#define	F_LINK	0x010				/* symbolic link */
#define	F_SOCK	0x020				/* socket */
#define	F_NONE	0x040				/* unknown */
	u_short	type;				/* file type */

#define	F_CKSUM	0x001				/* check sum */
#define	F_GROUP	0x002				/* group */
#define	F_IGN	0x004				/* ignore */
#define	F_MODE	0x008				/* mode */
#define	F_NLINK	0x010				/* number of links */
#define	F_OWNER	0x020				/* owner */
#define	F_SIZE	0x040				/* size */
#define	F_SLINK	0x080				/* link count */
#define	F_TYPE	0x100				/* file type */
	u_short	flags;				/* items set */

	off_t	st_size;
	u_long	cksum;				/* check sum */
	uid_t	st_uid;
	gid_t	st_gid;
#define	MBITS	(S_ISUID|S_ISGID|S_ISTXT|S_IRWXU|S_IRWXG|S_IRWXO)
	mode_t	st_mode;
	nlink_t	st_nlink;
	char	*slink;				/* symbolic link reference */
} INFO;

typedef struct _entry {
	struct _entry	*child, *parent;	/* up, down */
	struct _entry	*next, *prev;		/* left, right */
	INFO	info;				/* node info structure */
#define	F_DONE	0x01				/* directory done */
#define	F_VISIT	0x02				/* visited this node */
	u_char	flags;				/* flags */
	char	*name;				/* node name */
} ENTRY;

#define	RP(p)	(p->fts_path + 2)
