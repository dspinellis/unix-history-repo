#define	NGROUPS		10	/* number of mpx files permitted at one time */
#define	NCHANS		20	/* number of channel structures */
#define	NPORTS		30	/* number of channels to i/o ports */
#define	CNTLSIZ		10
#define	NLEVELS		4
#define	NMSIZE		50	/* max size of mxlstn file name */

/*
 * header returned on read of mpx
 */
struct	rh {
	short	index;
	short	count;
	short	ccount;
};

/*
 * head expected on write of mpx
 */
struct	wh {
	short	index;
	short	count;
	short	ccount;
	char	*data;
};

struct	mx_args {
	char	*m_name;
	int	m_cmd;
	int	m_arg[3];
};


#ifdef KERNEL
/*
 * internal structure for channel
 */

struct chan {
	short	c_flags;
	char	c_index;
	char	c_line;
	struct	group	*c_group;
	struct	file	*c_fy;
	struct	tty	*c_ttyp;
	struct	clist	c_ctlx;
	int	c_pgrp;
	struct	tty	*c_ottyp;
	char	c_oline;
	union {
		struct	clist	datq;
	} cx;
	union {
		struct	clist	datq;
		struct	chan	*c_chan;
	} cy;
	struct	clist	c_ctly;
};

struct schan {
	short	c_flags;
	char	c_index;
	char	c_line;
	struct	group	*c_group;
	struct	file	*c_fy;
	struct	tty	*c_ttyp;
	struct	clist	c_ctlx;
	int	c_pgrp;
};


/*
 * flags
 */
#define	INUSE	01
#define COPEN	02
#define	XGRP	04
#define	YGRP	010
#define	WCLOSE	020
#define	ISGRP	0100
#define	BLOCK	0200
#define	EOTMARK	0400
#define	SIGBLK	01000
#define	BLKMSG	01000
#define	ENAMSG	02000
#define	WFLUSH	04000
#define	NMBUF	010000
#define	PORT	020000

#endif









/*
 * mpxchan command codes
 */
#define	MPX	5
#define	MPXN	6
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
#define M_WATCH 1
#define M_CLOSE 2
#define	M_EOT	3
#define	M_OPEN	4
#define	M_BLK	5
#define	M_UBLK	6
#define	DO_BLK	7
#define	DO_UBLK	8
#define	M_IOCTL	12


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

