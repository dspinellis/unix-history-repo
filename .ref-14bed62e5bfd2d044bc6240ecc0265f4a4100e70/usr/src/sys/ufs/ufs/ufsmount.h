/*	ufsmount.h	4.3	81/02/26	*/

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
};
#ifdef KERNEL
struct	mount mount[NMOUNT];
#endif
