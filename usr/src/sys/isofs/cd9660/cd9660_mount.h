/*
 * Copyright (c) 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley
 * by Pace Willisson (pace@blitz.com).  The Rock Ridge Extension
 * Support code is derived from software contributed to Berkeley
 * by Atsushi Murai (amurai@spec.co.jp).
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cd9660_mount.h	8.1 (Berkeley) %G%
 */

/*
 * Arguments to mount ISO 9660 filesystems.
 */
struct iso_args {
	char	*fspec;			/* block special device to mount */
	struct	export_args export;	/* network export info */
	int	flags;			/* mounting flags, see below */
};
#define	ISOFSMNT_NORRIP	0x00000001	/* disable Rock Ridge Ext.*/
#define	ISOFSMNT_GENS	0x00000002	/* enable generation numbers */
#define	ISOFSMNT_EXTATT	0x00000004	/* enable extended attributes */
