/*	tty.h	4.6	81/10/17	*/

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
	struct	clist t_rawq;		/* device */
	struct	clist t_canq;		/* tty */
	struct	clist t_outq;		/* device */
	int	(*t_oproc)();		/* device */
	struct	proc *t_rsel;		/* tty */
				struct chan *T_CHAN;	/* ### */
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
#define	TIMEOUT	000001		/* delay timeout in progress */
#define	WOPEN	000002		/* waiting for open to complete */
#define	ISOPEN	000004		/* device is open */
#define	FLUSH	000010		/* outq has been flushed during DMA */
#define	CARR_ON	000020		/* software copy of carrier-present */
#define	BUSY	000040		/* output in progress */
#define	ASLEEP	000100		/* wakeup when output done */
#define	XCLUDE	000200		/* exclusive-use flag against open */
#define	TTSTOP	000400		/* output stopped by ctl-s */
#define	HUPCLS	001000		/* hang up upon last close */
#define	TBLOCK	002000		/* tandem queue blocked */
#define	RCOLL	004000		/* collision in read select */

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
