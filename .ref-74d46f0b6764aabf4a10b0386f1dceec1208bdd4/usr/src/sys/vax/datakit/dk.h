/*
 *	@(#)dk.h	1.3 (Berkeley) %G%
 *
 *	DATAKIT VCS User Level definitions
 *		@(#)dk.h	2.1 DKHOST 84/07/03
 */


/*
 *	ioctl codes
 */

/*
 * Note: these take paramters, but the copyin-copyout must be handled in
 * the driver.
 */
#define	DKIODIAL	_IO('k', 0) /* dial out */
#define	DKIOCNEW	_IO('k', 1) /* offer a service */
#define	DKIOCREQ	_IO('k', 2) /* request service (SU only) */
#define	DKIOCSPL	_IO('s', 1) /* splice two circuits (SU only) */
#define	DIOCSWAIT	_IO('s', 2) /* wait for splice to take place */

/*     driver control        */

#define DIOCEXCL	_IO('d', 1)	/* exclusive use */
#define DIOCNXCL	_IO('d', 2)	/* reset exclusive use */
#define	DIOCRMODE	_IOW('d', 3, short) /* set receiver termination modes */
#define	DIOCQQABO	_IOR('d', 4, struct dkqqabo) /* inquire status of last read */
#define	DIOCXCTL	_IOW('d', 8, short) /* send ctl envelope on next write */
#define DIOCFLUSH	_IO('d', 9)	/* flush output */
#define DIOCSETK	_IOW('d', 10, short) /* debug info from kmc xmit&recv */
#define	DIOCXWIN	_IOW('d', 11, struct diocxwin)	/* Set window size */
#define	KIOCINIT	_IO('d', 12)	/* Reinitialize transmitter */
#define	DIOCRESET	_IOW('d', 13, short)	/* Reset a channel */
#define	DIOCCTYPE	_IOW('d', 14, struct diocctype)	/* Set conn type */
#define	DIOCINFO	_IOR('d', 15, struct diocinfo)	/* Get chan #, max # */
#define	DIOCSTAT	_IOWR('d', 16, int)	/* Check if channel open */

/*	special codes used by dkxstdio	*/
#define	DXIOEXIT	_IOW('D', 'T', int)  /* process exit code */

/*
 *	structure returned from DIOCQQABO giving receive status
 */
struct dkqqabo {
	short	rcv_resid ;		/* residual length in buffer */
	short	rcv_reason ;		/* set of bits giving reasons */
	short	rcv_ctlchar ;		/* ctl char that finished block */
} ;

/*
 *   receive mode, and completion indicators
 *	also defined in sys/dkit.h
 */

#ifndef DKR_FULL
#define	DKR_FULL	01	/* buffer full, normal read done */
#define	DKR_CNTL	02	/* read terminated on control character */
#define	DKR_ABORT	010	/* receive aborted by higher level command */
#define	DKR_BLOCK	040	/* end of block */
#define	DKR_TIME	0100	/* end of time limit reached */
#endif



/*
 *	structure passed with ioctl to request a service
 */
struct diocreq {
	short	req_traffic ;		/* traffic intensity generated */
	short	req_1param ;		/* typ:  service requested */
	short	req_2param ;		/* parameter to server */
} ;

/*
 *	structure passed to dial a number
 */
struct diocdial {
		struct	diocreq iocb;
		char	dialstring[MLEN-sizeof (struct diocreq)];
};

/*
 * Structure passed to set URP limits
 */
struct diocxwin	{
	int		xwin_msgsize;	/* Size of one message */
	unsigned char	xwin_nmsg;	/* Number of outstanding messages */
};

/*
 * Structure passed to request configuration info
 */
struct diocinfo {
	short	dioc_nchan;		/* Number of channels configured */
	short	dioc_channum;		/* This channel number */
	short	dioc_commchan;		/* Common supervisory channel */
};

/*
 *	structure passed with ioctl to request a splice
 */
struct diocspl {
	short	spl_fdin;		/* the other file descriptor */
	short	spl_un1used;
	short	spl_un2used;
} ;

struct diocctype {
	int	dct_type;		/* Annotation type */
	char	dct_name[MLEN-sizeof (int)-1];	/* Connection type */
};
#define	DCT_DIAL	1		/* Dialstring uttered */
#define	DCT_SERVE	2		/* Server name */
#define	DCT_FROM	3		/* Connection source */


/*
 *	values returned from service request
 */
#define	req_error	req_traffic
#define	req_driver	req_traffic

#ifndef KERNEL
#include <sys/ioctl.h>
/*
 *	structure received by server when new connection made
 */
struct mgrmsg {
	short	m_chan ;		/* channel number of connection */
	unsigned short	m_tstamp ;	/* time stamp of request */
	char *	m_protocol ;		/* protocol options from user */
	char *	m_origtype ;		/* type of originating device */
	char *	m_parm ;		/* parameter string from user */
	char *	m_uid ;			/* param from system/user, aka UID */
	char *	m_dial ;		/* dial string entered */
	char *	m_source ;		/* originator, as known to remote node */
	char *	m_lname ;		/* originator, as known to local node */
	char *	m_service ;		/* service type requested by user */
	char *	m_lflag ;		/* L == call from local node,
					 * R == call from a remote one.   */
	char *	m_srcnode;		/* originating node (last segment)   */
	char *	m_srcmod;		/* originating mod		     */
	char *	m_srcchan;		/* originating channel		     */
	char *	m_cflag;		/* call flag: F=first, P=previous    */
	char *	m_errmsg ;		/* possible error msg if m_chan <= 0 */
	char *	m_baudrate ;		/* incoming terminal baud rate */
} ;



/*
 *	routines declared in libdk.a
 */
extern char		*dknamer();
extern char		*dtnamer();
extern char		*dxnamer();
extern char		*dinamer();
extern char		*dketcf();
extern char		*dkfcanon(), *dktcanon();
extern char		*dkerr();
extern char		*maphost(), *miscfield();
extern char		mh_hostname[];
extern struct mgrmsg	*dkmgr();

extern int		dk_verbose, dk_errno;
#endif KERNEL

