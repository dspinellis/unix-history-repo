#define NGROUPS		10
#define	NCHANS		20

#define	DCHAN		1
#define	CCHAN		0

struct chan {
	short	c_flags;
	char	c_index;
	char	c_msg;
	struct	group *c_group;
	struct	tty	*c_ttyp;
	struct	file	*c_fy;
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
	short	g_state;
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
#define COPEN	02
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
#define	RZERO	010000


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
#define PACK	13

#define NDEBUGS	30
/*
 * control channel message codes
 */
#define	WATTCH	1
#define M_WATCH 1
#define	SCLOSE	2
#define M_CLOSE 2
#define	M_ATTACH 3
#define	M_BLK	4
#define	M_UBLK	5
#define	DO_BLK	6
#define	DO_UBLK	7
#define	M_XINT	8
#define	M_RINT	9
#define	M_ACK	10
#define	M_NAK	11


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

/* HDRSIZE == NLEVELS + sizeof (short) */
#define	HDRSIZE		4
#define NLEVELS		2
union u_index {
	short val;
	char	bytes[NLEVELS];
};
typedef union u_index index_t;
