/*
**  Definitions for the range table.
**
**	Version:
**		@(#)range.h	7.1	2/5/81
*/

# ifndef MAXRANGE

# define	MAXRANGE	(MAXVAR + 1)


/*
**	DESCRIPTOR struct
**
**	The DESCRIPTOR struct is initialized by OPENR to describe any
**	open relation.  The first part of the descriptor is the tuple
**	from the RELATION relation.  The remainder contains some magic
**	numbers and a template initialized from the ATTRIBUTE relation.
**
**	This structure also defines the range table.
*/

struct descriptor
{
	struct relation	reldum;
		/*the above part of the descriptor struct is identical
		  to the relation struct and the inormation in this
		  part of the struct is read directly from the
		  relation tuple by openr.  the rest of the descriptor
		  struct is calculated by openr.  */
	char	relvname[MAXNAME];	/* range variable name */
	i_2	relfp;		/*filep for relation , if open	*/
	i_2	relopn;		/*indicates if relation is really open*/
	tid_type reltid;	/*when relation is open, this indicates
				  the tid in the relation relation for
				  this relation */
	i_4	reladds;	/*no. of additions of tuples during this open*/
	i_2	reloff[MAXDOM];	/*reloff[i] is offset to domain i 	*/
	c_1	relfrmt[MAXDOM]; /* format of domain i
				 ** INT, FLOAT, or CHAR  */
	c_1	relfrml[MAXDOM]; /* relfrml[i] is an unsigned integer
				  which indicates length
				  in bytes of domain */
	c_1	relxtra[MAXDOM]; /*relxtra[i] is non-zero if domain i is
				 ** a key domain for the relation */
	c_1	relgiven[MAXDOM]; /*cleared by openr and set before
				  call to find to indicate value of this
				  domain has been supplied in the key*/
};

typedef struct descriptor	DESC;


typedef struct
{
	DESC	*rngvdesc;	/* pointer to descriptor for this var */
	bool	rngvmark;	/* send if marked */
}  RANGEV;


# endif MAXRANGE
