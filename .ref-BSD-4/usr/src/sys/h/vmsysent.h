/*	vmsysent.h	4.1	11/9/80	*/

/*
 * Externals for functions defined in vmsys.c.
 */

int	nosys();
int	nullsys();

int	vfork();		/* later, just fork? */
int	vread();		/* deprecated */
int	vwrite();		/* deprecated */
int	vadvise();		/* later, segadvise */

int	vhangup();		/* should just do in exit() */
int	vlimit();
int	vswapon();
int	vtimes();

int	segalloc();		/* not in yet */
int	segfree();		/* not in yet */
int	segsync();		/* not in yet */
