/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty.h	6.4 (Berkeley) %G%
 */

#ifdef KERNEL
#include "ttychars.h"
#include "ttydev.h"
#else
#include <sys/ttychars.h>
#include <sys/ttydev.h>
#endif

/*
 * A clist structure is the head of a linked list queue
 * of characters.  The characters are stored in blocks
 * containing a link and CBSIZE (param.h) characters. 
 * The routines in tty_subr.c manipulate these structures.
 */
struct clist {
	int	c_cc;		/* character count */
	char	*c_cf;		/* pointer to first char */
	char	*c_cl;		/* pointer to last char */
};

/*
 * Per-tty structure.
 *
 * Should be split in two, into device and tty drivers.
 * Glue could be masks of what to echo and circular buffer
 * (low, high, timeout).
 */
struct tty {
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
	int	t_flags;		/* some of both */
	int	t_state;		/* some of both */
	short	t_pgrp;			/* tty */
	char	t_delct;		/* tty */
	char	t_line;			/* glue */
	char	t_col;			/* tty */
	char	t_ispeed, t_ospeed;	/* device */
	char	t_rocount, t_rocol;	/* tty */
	struct	ttychars t_chars;	/* tty */
	struct	winsize t_winsize;	/* window size */
/* be careful of tchars & co. */
#define	t_erase		t_chars.tc_erase
#define	t_kill		t_chars.tc_kill
#define	t_intrc		t_chars.tc_intrc
#define	t_quitc		t_chars.tc_quitc
#define	t_startc	t_chars.tc_startc
#define	t_stopc		t_chars.tc_stopc
#define	t_eofc		t_chars.tc_eofc
#define	t_brkc		t_chars.tc_brkc
#define	t_suspc		t_chars.tc_suspc
#define	t_dsuspc	t_chars.tc_dsuspc
#define	t_rprntc	t_chars.tc_rprntc
#define	t_flushc	t_chars.tc_flushc
#define	t_werasc	t_chars.tc_werasc
#define	t_lnextc	t_chars.tc_lnextc
};

#define	TTIPRI	28
#define	TTOPRI	29

/* limits */
#define	NSPEEDS	16
#define	TTMASK	15
#define	OBUFSIZ	100
#define	TTYHOG	255
#ifdef KERNEL
short	tthiwat[NSPEEDS], ttlowat[NSPEEDS];
#define	TTHIWAT(tp)	tthiwat[(tp)->t_ospeed&TTMASK]
#define	TTLOWAT(tp)	ttlowat[(tp)->t_ospeed&TTMASK]
extern	struct ttychars ttydefaults;
#endif

/* internal state bits */
#define	TS_TIMEOUT	0x000001	/* delay timeout in progress */
#define	TS_WOPEN	0x000002	/* waiting for open to complete */
#define	TS_ISOPEN	0x000004	/* device is open */
#define	TS_FLUSH	0x000008	/* outq has been flushed during DMA */
#define	TS_CARR_ON	0x000010	/* software copy of carrier-present */
#define	TS_BUSY		0x000020	/* output in progress */
#define	TS_ASLEEP	0x000040	/* wakeup when output done */
#define	TS_XCLUDE	0x000080	/* exclusive-use flag against open */
#define	TS_TTSTOP	0x000100	/* output stopped by ctl-s */
#define	TS_HUPCLS	0x000200	/* hang up upon last close */
#define	TS_TBLOCK	0x000400	/* tandem queue blocked */
#define	TS_RCOLL	0x000800	/* collision in read select */
#define	TS_WCOLL	0x001000	/* collision in write select */
#define	TS_NBIO		0x002000	/* tty in non-blocking mode */
#define	TS_ASYNC	0x004000	/* tty in async i/o mode */
/* state for intra-line fancy editing work */
#define	TS_BKSL		0x010000	/* state for lowercase \ work */
#define	TS_QUOT		0x020000	/* last character input was \ */
#define	TS_ERASE	0x040000	/* within a \.../ for PRTRUB */
#define	TS_LNCH		0x080000	/* next character is literal */
#define	TS_TYPEN	0x100000	/* retyping suspended input (PENDIN) */
#define	TS_CNTTB	0x200000	/* counting tab width; leave FLUSHO alone */

#define	TS_LOCAL	(TS_BKSL|TS_QUOT|TS_ERASE|TS_LNCH|TS_TYPEN|TS_CNTTB)

/* define partab character types */
#define	ORDINARY	0
#define	CONTROL		1
#define	BACKSPACE	2
#define	NEWLINE		3
#define	TAB		4
#define	VTAB		5
#define	RETURN		6
