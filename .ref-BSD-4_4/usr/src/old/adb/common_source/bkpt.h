/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)bkpt.h	5.2 (Berkeley) 4/4/91
 */

#define	MAX_BKPTCOM	64	/* maximum length of command at bkpt */

enum bkflag { BKPT_FREE, BKPT_SET, BKPT_TRIPPED };

struct bkpt {
	struct	bkpt *next;	/* linked list */
	enum	bkflag state;	/* state */
	addr_t	loc;		/* where set (in SP_INSTR) */
	bpt_t	ins;		/* original instruction(s) */
	int	count;		/* ??? */
	int	initcnt;	/* ??? */
	char	comm[MAX_BKPTCOM];/* command to execute when tripped */
};

struct bkpt *scanbkpt();	/* look up a breakpoint given an address */
