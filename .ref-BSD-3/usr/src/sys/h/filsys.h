/*	filsys.h	2.1	1/5/80	*/

/*
 * Structure of the super-block
 */
struct	filsys
{
	unsigned short s_isize;		/* size in blocks of i-list */
	daddr_t	s_fsize;   		/* size in blocks of entire volume */
	short  	s_nfree;   		/* number of addresses in s_free */
	daddr_t	s_free[NICFREE];	/* free block list */
	short  	s_ninode;  		/* number of i-nodes in s_inode */
	ino_t  	s_inode[NICINOD];	/* free i-node list */
	char   	s_flock;   		/* lock during free list manipulation */
	char   	s_ilock;   		/* lock during i-list manipulation */
	char   	s_fmod;    		/* super block modified flag */
	char   	s_ronly;   		/* mounted read-only flag */
	time_t 	s_time;    		/* last super block update */
	/* begin not maintained by this version of the system */
	daddr_t	s_tfree;   		/* total free blocks*/
	ino_t  	s_tinode;  		/* total free inodes */
	short  	s_m;       		/* interleave factor */
	short  	s_n;       		/* " " */
	char   	s_fname[6];		/* file system name */
	char   	s_fpack[6];		/* file system pack name */
	/* end not maintained */
	ino_t	s_lasti;		/* start place for circular search */
	ino_t	s_nbehind;		/* est # free inodes before s_lasti */
};

#ifdef KERNEL
struct	filsys *getfs();
#endif
