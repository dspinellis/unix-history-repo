/*	rpb.h	4.2	81/02/19	*/

/*
 * The restart parameter block, which is a page in (very) low
 * core which runs after a crash.  Currently, the restart
 * procedure takes a dump.
 */
struct rpb {
	struct	rpb *rp_selfref;	/* self-reference */
	int	(*rp_dumprout)();	/* routine to be called */
	long	rp_checksum;		/* checksum of 31 words of dumprout */
	long	rp_flag;		/* set to 1 when dumprout runs */
/* the dump stack grows from the end of the rpb page not to reach here */
};
#ifdef KERNEL
extern	struct rpb rbp;
#endif
