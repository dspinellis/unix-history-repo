/*	clist.h	4.2	81/02/19	*/

/*
 * Raw structures for the character list routines.
 */
struct cblock {
	struct cblock *c_next;
	char	c_info[CBSIZE];
};
struct	cblock	cfree[];
