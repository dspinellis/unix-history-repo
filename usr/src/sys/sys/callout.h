/*	callout.h	4.3	81/02/27	*/

/*
 * The callout structure is for
 * a routine arranging
 * to be called by the clock interrupt
 * (clock.c) with a specified argument,
 * in a specified amount of time.
 * Used, for example, to time tab
 * delays on typewriters.
 */

struct	callo
{
	int	c_time;		/* incremental time */
	caddr_t	c_arg;		/* argument to routine */
	int	(*c_func)();	/* routine */
};
#ifdef KERNEL
extern	struct callo *callout;
extern	int ncallo;
#endif
