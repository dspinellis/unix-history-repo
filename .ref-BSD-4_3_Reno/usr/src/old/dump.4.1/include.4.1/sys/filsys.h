/*	filsys.h	4.3	81/03/03	*/

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
	daddr_t	s_tfree;   		/* total free blocks*/
	ino_t  	s_tinode;  		/* total free inodes */
	short	s_dinfo[2];		/* interleave stuff */
#define	s_m	s_dinfo[0]
#define	s_n	s_dinfo[1]
	char   	s_fsmnt[12];		/* ordinary file mounted on */
	/* end not maintained */
	ino_t	s_lasti;		/* start place for circular search */
	ino_t	s_nbehind;		/* est # free inodes before s_lasti */
};

#ifdef KERNEL
struct	filsys *getfs();
#endif
