/* sh.dir.h 4.1 10/9/80 */

/*
 * Structure for entries in directory stack.
 */
struct	directory	{
	struct	directory *di_next;	/* next in loop */
	struct	directory *di_prev;	/* prev in loop */
	unsigned short *di_count;	/* refcount of processes */
	char	*di_name;		/* actual name */
};
struct directory *dcwd;		/* the one we are in now */
