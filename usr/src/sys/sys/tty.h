/*	tty.h	4.11	82/03/15	*/

#ifdef KERNEL
#include "../h/ioctl.h"
#else
#include <sys/ioctl.h>
#endif
#include <sgtty.h>

/*
 * A clist structure is the head of a linked list queue of characters.
 * The characters are stored in blocks containing a link and CBSIZE (param.h)
 * characters.  The routines in prim.c manipulate these structures.
 */
struct clist {
	int	c_cc;		/* character count */
	char	*c_cf;		/* pointer to first char */
	char	*c_cl;		/* pointer to last char */
};

/*
 * Per-tty structre.
 *
 * Should be split in two, into device and tty drivers.
 * Glue could be masks of what to echo and circular buffer
 * (low, high, timeout).
 */
struct tty
{
	union {
		struct {
			struct	clist T_rawq;
			struct	clist T_canq;
		} t_t;
#define	t_rawq	t_nu.t_t.T_rawq		/* raw characters or partial line */
#define	t_canq	t_nu.t_t.T_canq		/* raw characters or partial line */
		struct {
			struct	buf *T_bufp;
			char	*T_cp;
			int	T_inbuf;
			int	T_rec;
		} t_n;
#define	t_bufp	t_nu.t_n.T_bufp		/* buffer allocated to protocol */
#define	t_cp	t_nu.t_n.T_cp		/* pointer into the ripped off buffer */
#define	t_inbuf	t_nu.t_n.T_inbuf	/* number chars in the buffer */
#define	t_rec	t_nu.t_n.T_rec		/* have a complete record */
	} t_nu;
	struct	clist t_outq;		/* device */
	int	(*t_oproc)();		/* device */
	struct	proc *t_rsel;		/* tty */
	struct	proc *t_wsel;
				caddr_t	T_LINEP;	/* ### */
	caddr_t	t_addr;			/* ??? */
	dev_t	t_dev;			/* device */
	short	t_flags;		/* some of both */
	short	t_state;		/* some of both */
	short	t_pgrp;			/* tty */
	char	t_delct;		/* tty */
	char	t_line;			/* glue */
	char	t_col;			/* tty */
	char	t_erase, t_kill;	/* tty */
	char	t_char;			/* tty */
	char	t_ispeed, t_ospeed;	/* device */
/* begin local */
	char	t_rocount, t_rocol;	/* tty */
	struct	ltchars t_lchr;		/* tty */
	short	t_local;		/* tty */
	short	t_lstate;		/* tty */
/* end local */
	union {
		struct tchars t_chr;	/* tty */
		struct clist T_CTLQ;
	} t_un;
};

#define	tun	tp->t_un.t_chr
#define	tlun	tp->t_lchr

#define	TTIPRI	28
#define	TTOPRI	29

#define	CTRL(c)	('c'&037)

/* default special characters */
#define	CERASE	'#'
#define	CEOT	CTRL(d)
#define	CKILL	'@'
#define	CQUIT	034		/* FS, cntl shift L */
#define	CINTR	0177		/* DEL */
#define	CSTOP	CTRL(s)
#define	CSTART	CTRL(q)
#define	CBRK	0377

/* limits */
#define	NSPEEDS	16
#define	TTMASK	15
#ifdef KERNEL
short	tthiwat[NSPEEDS], ttlowat[NSPEEDS];
#define	TTHIWAT(tp)	tthiwat[(tp)->t_ospeed&TTMASK]
#define	TTLOWAT(tp)	ttlowat[(tp)->t_ospeed&TTMASK]
#endif
#define	TTYHOG	255

/* hardware bits */
#define	DONE	0200
#define	IENABLE	0100

/* internal state bits */
#define	TS_TIMEOUT	000001		/* delay timeout in progress */
#define	TS_WOPEN	000002		/* waiting for open to complete */
#define	TS_ISOPEN	000004		/* device is open */
#define	TS_FLUSH	000010		/* outq has been flushed during DMA */
#define	TS_CARR_ON	000020		/* software copy of carrier-present */
#define	TS_BUSY		000040		/* output in progress */
#define	TS_ASLEEP	000100		/* wakeup when output done */
#define	TS_XCLUDE	000200		/* exclusive-use flag against open */
#define	TS_TTSTOP	000400		/* output stopped by ctl-s */
#define	TS_HUPCLS	001000		/* hang up upon last close */
#define	TS_TBLOCK	002000		/* tandem queue blocked */
#define	TS_RCOLL	004000		/* collision in read select */
#define	TS_WCOLL	010000		/* collision in write select */
#define	TS_NBIO		020000		/* tty in non-blocking mode */
#define	TS_ASYNC	040000		/* tty in async i/o mode */

/* define partab character types */
#define	ORDINARY	0
#define	CONTROL		1
#define	BACKSPACE	2
#define	NEWLINE		3
#define	TAB		4
#define	VTAB		5
#define	RETURN		6

/* define dmctl actions */
#define	DMSET		0
#define	DMBIS		1
#define	DMBIC		2
#define	DMGET		3
