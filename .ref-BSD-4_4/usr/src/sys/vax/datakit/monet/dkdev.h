/*
 * 	DATAKIT Common Kernel Structure interface definitions
 *		@(#)dkdev.h	1.3 Garage 84/05/14
 */

/*
 * datakit ioctl commands
 */
#define DKIOCMASK	(0377 << 8)	/*  if (code & mask) == val   */
#define DKIOCVAL	('k' << 8)	/*    then datakit control    */



/*
 *	common structure for dk.c and dkxqt.c (and a little of dktty.c)
 *	  for an open channel
 */

struct dkdev {
	short	d_state ;		/* various bit values, defined below */
	short	dc_state;		/* more bit values */
	short	d_bufct ;		/* count of buffers in write Q */
	short	d_rmode ;		/* receive termination modes */
	short	d_rresid ; /* 1 of 3 */	/* residual buffer left in receive */
	short	d_rdone ;  /* 2 of 3 */	/* completion status of last receive */
	short	d_rctl ;   /* 3 of 3 */	/* control char received */
	short	d_xctl ;		/* control char to send */
	int	d_pgrp ;		/* process group number */
	short	d_prot;			/* active protocols on this channel */
	struct	diocxwin d_win;		/* Requested window size */
	struct	diocctype *d_ctype;	/* Connection type */
#	  define	DpURP	0x0001	/* plain URP -- dk.c */
#	  define	DpTTY	0x0002	/* tty protocol -- dktty.c */
#	  define	DpXQT	0x0004	/* remote execution -- dkxqt.c */
#	  define	DpIP	0x0008	/* IP */

	union { struct {	/* DK */
		caddr_t	dk_addr ;	/* address of current bp or cb */
		short	dk_param[3] ;	/* parameters from user from ioctl */
		short	dk_Xstate ;		/* various state bits */
		short	dk_error ;		/* call setup error code */
			} d_dk ;


	      struct {		/* DKXQT */
		caddr_t	dkx_rbuf ;	/* receive buffer address */
		caddr_t	dkx_xbuf ;	/* transmit buffer address */
		unsigned dkx_discard ;	/* amount to discard from input */
		short	dkx_Xstate ;		/* various state bits */
			} d_dkx ;


				/* others? */

	   } d_var ;
} ;

#define	d_addr	d_var.d_dk.dk_addr
#define	d_param	d_var.d_dk.dk_param
#define	d_error	d_var.d_dk.dk_error


#define	d_rbuf	d_var.d_dkx.dkx_rbuf
#define	d_xbuf	d_var.d_dkx.dkx_xbuf
#define	d_discard  d_var.d_dkx.dkx_discard

#define	dx_state	d_var.d_dkx.dkx_Xstate

/*
 *	bits in d_state
 */
#define	DKOPEN		000001		/* this channel is open somewhere */
#define	DKXCLUDE	000002		/* open exclusive use, or DT open */
#define	DKWAIT		000004		/* process waiting for a buffer */
#define	DKSPLICED	000020		/* received splice request from controller */


/*
 *	bits in dc_state
 */
#define	DKSETUP		000004		/* setup in progress on channel */
#define	DKEXPMT		000010		/* timing experiment mode */
#define	DKXMIT		000010		/* transmit */
#define	DKDROP		000030		/* for ignoring input */
#define	DKRW		000050		/* bounce, read then write */
#define	DKWR		000070		/* bounce, write then read */
#define	DKNOABO		000100		/* do not abort receives, NK mode */
#define	DK_ASYNC	000200		/* generate sigio when complete */
#define	DK_NDELAY	000400		/* non-blocking mode */

/*
 *	bits in dx_state
 */
#define	DXWAIT		000040		/* process is waiting for line */
#define	DXCANCEL	020000		/* last sent cancel, ignore nxt msg */
#define	DXRQRCV		040000		/* response received to request */
#define	DXRQACT		0100000		/* request active on channel */
