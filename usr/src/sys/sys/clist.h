/*	clist.h	4.1	%G%	*/

/*
 * Raw structures for the character list routines.
 */
struct cblock {
	struct cblock *c_next;
	char	c_info[CBSIZE];
};
struct	cblock	cfree[];
