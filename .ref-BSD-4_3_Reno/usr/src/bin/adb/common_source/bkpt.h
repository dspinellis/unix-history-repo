/*
 * adb: breakpoints.
 *
 *	@(#)bkpt.h	5.1 (Berkeley) 1/16/89
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
