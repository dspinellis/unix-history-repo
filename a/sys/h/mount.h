/*	mount.h	4.4	82/07/19	*/

/*
 * Mount structure.
 * One allocated on every mount.
 * Used to find the super block.
 */
struct	mount
{
	dev_t	m_dev;		/* device mounted */
	struct	buf *m_bufp;	/* pointer to superblock */
	struct	inode *m_inodp;	/* pointer to mounted on inode */
	struct	inode *m_qinod;	/* QUOTA: pointer to quota file */
};
#ifdef KERNEL
struct	mount mount[NMOUNT];
#endif
