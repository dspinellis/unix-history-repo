/*	vlimit.h	4.2	81/02/19	*/

/*
 * Limits for u.u_limit[i], per process, inherited.
 */
#define	LIM_NORAISE	0	/* if <> 0, can't raise limits */
#define	LIM_CPU		1	/* max secs cpu time */
#define	LIM_FSIZE	2	/* max size of file created */
#define	LIM_DATA	3	/* max growth of data space */
#define	LIM_STACK	4	/* max growth of stack */
#define	LIM_CORE	5	/* max size of ``core'' file */

#define	NLIMITS		5

#define	INFINITY	0x7fffffff
