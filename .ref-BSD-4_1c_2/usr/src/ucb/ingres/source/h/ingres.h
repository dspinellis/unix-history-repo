#
/*
**  INGRES.H -- basic header file for ingres.
**
**	See also aux.h for definitions used by some but not all.
**
**	Version:
**		@(#)ingres.h	7.1	2/5/81
*/

# ifndef MAXNAME


/*
**  Some generally useful stuff.
*/

# include <useful.h>


 
/*
**	definition of machine type
**
**	If PDP11 is defined, the wordsize is defined as 16, and any other
**	machine dependant things are set to the values needed for a PDP 11/70.
**	Currently the default is to the 32 bits existant on VAX 11/780s.
*/

# define	VAX




# include <trace.h>

/*
**	INGRES manifest constants
**
**	These constants are manifest to the operation of the entire
**	system.  If anything
**	is changed part or all of the system will stop working.
**	The values have been carefully chosen and are not intended
**	to be modifiable by users.
*/

# define	MAXDOM		50		/* max # + 1 of doms in a relation */
# define	MAXNAME		12		/* max size of a name (in bytes) */
# define	MAXVAR		10		/* max # of variables */
# define	MAXKEYS		6		/* max # of keys in secondary index */
# define	MAXAGG		50		/* max number of aggs in a qry */
# define	STACKSIZ	20		/* max depth for arith. expr. stacks */
# define	I1MASK		0377		/* mask out sign extension that occurs
						**  when a c1 or i1 field is converted
						**  to an i2 field. 
						*/

# define	i_1		char
# define	i_2		short
# define	i_4		long
# define	c_1		char
# define	c_2		char
# define	c_12		char

/*
**	RELATION relation struct
**
**	The RELATION relation contains one tuple for each relation
**	in the database.  This relation contains information which
**	describes how each relation is actually stored in the
**	database, who the owner is, information about its size,
**	assorted operation information, etc.
*/

# define	RELID		1	/* domain for setkey */
# define	RELOWNER	2

struct relation
{
	c_12	relid[MAXNAME];	/* relation name	*/
	c_2	relowner[2];	/* code of relation owner */
	i_1	relspec;	/* storage mode of relation	*/
				/* M_HEAP  unsorted paged heap	*/
				/* -M_HEAP compressed heap	*/
				/* M_ISAM  isam			*/
				/* -M_ISAM compressed isam	*/
				/* M_HASH  hashed		*/
				/* -M_HASH compressed hash	*/
	i_1	relindxd;	/* -1 rel is an index, 0 not indexed, 1 indexed */
	i_2	relstat2;	/* more status bits */
	i_2	relstat;	/* relation status bits */
	i_4	relsave;	/*unix time until which relation is saved*/
	i_4	reltups;	/*number of tuples in relation	*/
	i_2	relatts;	/*number of attributes in relation	*/
	i_2	relwid;		/*width (in bytes) of relation	*/
	i_4	relprim;	/*no. of primary pages in relation*/
	i_4	relfree;	/* head of freelist (b-trees only) */
	i_4	relstamp;	/*time of last mod*/
};


/*
**	ATTRIBUTE relation struct
**
**	The ATTRIBUTE relation contains one tuple for each domain
**	of each relation in the database.  This relation describes
**	the position of each domain in the tuple, its format,
**	its length, and whether or not it is used in part of the key.
*/

# define	ATTRELID	1
# define	ATTOWNER	2
# define	ATTID		3
# define	ATTNAME		4


struct attribute
{
	c_12 	attrelid[MAXNAME];	/*relation name of which this is an attr */
	c_2	attowner[2];	/* code of relation owner */
	i_2	attid;		/*domain number (from 1 to relatts)	*/
	c_12	attname[MAXNAME];	/*alias for this domain*/
	i_2	attoff;		/*offset in tuple (no. of bytes*/
	i_1	attfrmt;	/* INT, FLOAT, CHAR (in symbol.h) */
	i_1	attfrml;	/* unsigned integer no of bytes	*/
	i_1	attxtra;	/* flag indicating whether this dom is part of a key */
};

/*
**	tuple id struct
**
**	We want the line_id to be in the low-order of a long, in
**	order to make index work efficiently; since the order
**	of halfwords is reversed in a VAX, this is dependent...
*/

struct tup_id
{
# ifdef PDP11
	c_1	pg1, pg0;
	c_1	line_id, pg2;
# else PDP11
	c_1	line_id, pg2, pg1, pg0;
# endif PDP11
};

typedef struct tup_id	TID;

typedef union
{
	long	ltid;
	TID	s_tupid;
} tid_type;


# include <range.h>		/* to get the descriptor struct */


/* modes to find */
# define	NOKEY		1	/* scan entire relation */
# define	EXACTKEY	2
# define	LRANGEKEY	3	/* low range key */
# define	FULLKEY		4	/* forces full key comparison */
# define	HRANGEKEY	5	/* high range key */

/*
**	anytype union -- union of ingres types
*/

union anytype
{
	char		i1type;
	short		i2type;
	long		i4type;
	float		f4type;
	double		f8type;
	char		c0type[1];	/* the 1 is bogus, only needs 
					 * starting address
					 */
	char		*cptype;
	char		**cpptype;
};

typedef union anytype	ANYTYPE;


/*
**  Definitions for interface to the control module.
*/

extern char	Qbuf[];


# endif MAXNAME
