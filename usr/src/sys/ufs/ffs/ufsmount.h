/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ufsmount.h	7.3 (Berkeley) %G%
 */

/*
 * Mount structure.
 * One allocated on every mount.
 * Used to find the super block.
 */
struct	ufsmount {
	struct	mount *um_mountp;	/* vfs structure for this filesystem */
	dev_t	um_dev;			/* device mounted */
	struct	vnode *um_devvp;	/* vnode for block device mounted */
	struct	fs *um_fs;		/* pointer to superblock */
	struct	inode *um_qinod;	/* QUOTA: pointer to quota file */
	char	um_mntname[MNAMELEN];	/* mounted filesystem */
};
#ifdef KERNEL
/*
 * Convert mount ptr to ufsmount ptr.
 */
#define VFSTOUFS(mp)	((struct ufsmount *)((mp)->m_data))

/*
 * mount table
 */
extern struct ufsmount	mounttab[NMOUNT];

#endif
