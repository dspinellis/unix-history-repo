/*	nami.h	4.2	82/11/13	*/

struct namidata {
	int	ni_offset;
	int	ni_count;
	struct	inode *ni_pdir;
	struct	direct ni_dent;
};

enum nami_op { NAMI_LOOKUP, NAMI_CREATE, NAMI_DELETE };

/* this is temporary until the namei interface changes */
#define	LOOKUP		0	/* perform name lookup only */
#define	CREATE		1	/* setup for file creation */
#define	DELETE		2	/* setup for file deletion */
#define	LOCKPARENT	0x10	/* see the top of namei */
