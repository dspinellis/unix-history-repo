/*	namei.h	4.1	82/09/04	*/

struct namidata {
	int	ni_offset;
	int	ni_count;
	struct	inode *ni_pdir;
	struct	direct ni_dent;
};

enum nami_op { NAMI_LOOKUP, NAMI_CREATE, NAMI_DELETE };
