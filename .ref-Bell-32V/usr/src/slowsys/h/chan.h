#define NGROUPS		10
#define	NCHANS		20


struct chan {
	int	c_flags;
	char	c_index;
	char	c_msg;
	struct	group *c_group;
	struct	tty	*c_ttyp;
	int	c_pgrp;
	union {
		struct	clist	datq;
	} cx;
	union {
		struct	clist	datq;
		struct	chan	*c_chan;
	} cy;
	struct	clist	c_ctlx;
	struct	clist	c_ctly;
};


struct group {
	char	g_state;
	char	g_mindex;
	short	g_index;
	short	g_count;
	struct	inode 	*g_inode;
	struct	group	*g_master;
	struct	clist	g_cntl;
	struct	clist	g_datq;
};

/*
 * flags
 */
#define	INUSE	01
#define	XGRP	04
#define	YGRP	010
#define	WCLOSE	020
#define	TTYO	040
#define	SUBGRP	0100
#define	BLOCK	0200
#define	BLOCK1	0400
#define	SIGBLK	01000
#define	BLKMSG	01000
#define	ENAMSG	02000
#define	WFLUSH	04000


/*
 * mpxchan command codes
 */
#define	GROUP	5
#define	GROUPN	6
#define	CHAN	1
#define JOIN	2
#define EXTR	3
#define	ATTACH	4
#define	CONNECT	7
#define	DETACH	8
#define	DISCON	9
#define DEBUG	10
#define	NPGRP	11
#define	CSIG	12

#define NDEBUGS	30
/*
 * control channel message codes
 */
#define	WATTCH	1
#define	SCLOSE	2
#define	UID	3
#define	M_BLK	4
#define	M_UBLK	5
#define	DO_BLK	6
#define	DO_UBLK	7


/*
 * debug codes other than mpxchan cmds
 */
#define MCCLOSE 29
#define MCOPEN	28
#define	ALL	27
#define SCON	26
#define	MSREAD	25
#define	SDATA	24
#define	MCREAD	23
#define MCWRITE	22

union u_index {
	short val;
	struct {
		char byte0;
		char byte1;
	};
};
typedef union u_index index_t;
