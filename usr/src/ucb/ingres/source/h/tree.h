/*
**  TREE.H -- defines the structure of a querytree
**
**	Version:
**		@(#)tree.h	7.1	2/5/81
*/


# ifndef QT_HDR_SIZ

# include	<ingres.h>


/*
**	Structures Used In The Value Fields of Querytree nodes
*/

/*
**  VAR node
**
**	This node type contains info for a tuple variable.
**	varno -- index into range table
**	attno -- attribute number in this relation
**	varfrmt -- type of this domain
**	varfrml -- length of this domain
**	valptr -- pointer to value when bound.
**
**	If varno == -1, then this variable has been substituted; to
**	get the actual VAR node, follow the chain of valptr's.
*/

struct varnode
{
	char	varno;			/* variable number */
	char	attno;			/* attribute number */
	char	varfrmt;		/* type */
	char	varfrml;		/* length */
	ANYTYPE	*valptr;		/* pointer to value */
};


/*
**	STRUCTURE FOR AND, AGHEAD, AND ROOT NODES
**
**		In the parser and qrymod none of these fields are used.
**		Decomp maintains information about the variables
**		in the left and right descendents of the nodes.
**		The "rootuser" flag is present only in the ROOT and AGHEAD
**		nodes.  It is TRUE only in the original ROOT node of the query.
*/

struct rootnode 		/* AND, AGHEAD, ROOT nodes */
{
	char	tvarc;			/* total of var's in sub-tree */
	char	lvarc;			/* # of variables in left branch */
	short	lvarm;			/* bit map of var's in left branch */
	short	rvarm;			/* bit map of var's in right branch*/
	short	rootuser;		/* flag: TRUE if root of user generated query */
};

struct opnode			/* AOP, BOP, UOP nodes */
{
	short	opno;			/* operator number */
	char	opfrmt;			/* format of function */
	char	opfrml;			/* length of function */
	char	agfrmt;			/* in AOP, format of result */
	char	agfrml;			/* in AOP, length of result */
};

struct resdomnode		/* RESDOM node */
{
	short	resno;			/* result domain number */
	char	resfrmt;		/* result format */
	char	resfrml;		/* result length */
};


struct srcid			/* SOURCEID node */
{
	short	srcvar;			/* variable number */
	DESC	srcdesc;		/* descriptor for this var */
};


/*
**	SYMVALUE UNION
**
**		This union contains all of the types available
**		in the value field of a querytree node.
*/

union symvalue
{
	union anytype		sym_data;
	struct varnode		sym_var;
	struct rootnode		sym_root;
	struct opnode		sym_op;
	struct resdomnode	sym_resdom;
	struct srcid		sym_srcid;
};


/*
**	SYMBOL DEFINITION
**
**		Basic symbol structure. "Type" is one of the symbols
**		in "symbol.h", "len" is the length of the "value"
**		field (0 to 255 bytes), "value" is variable length and
**		holds the actual value (if len != 0) of the node.
**		The "value" is one of the types contained in "union symvalue".
**
**		On a thirty-two bit machine, there are two bytes of padding
**		after type and length.  These two bytes are discarded 
**		when a symbol is written to a pipe.
*/

struct symbol
{
	char		type;			/* type codes in symbol.h */
	char		len;			/* length in bytes of value field */
	union symvalue	value;
};

typedef struct symbol	SYMBOL;


/*
**	QUERYTREE NODE
**
**		Basic node in the querytree. Each node has a left and
**		right descendent. If the node is a leaf node then the
**		left and right pointers will be NULL. Depending on the
**		"type" field of the symbol structure, there may be additional
**		information.
*/

struct querytree
{
	struct querytree	*left;
	struct querytree	*right;
	struct symbol		sym;
};

typedef struct querytree	QTREE;


/*
**	SUNDRY CONSTANTS
**
**		There are several differences in the handling of data
**		structures on 16 and 32 bit machines:
**			1).  A pointer to  memory is either 2 or 4 bytes.
**			2).  Padding is inserted in structures to insure
**				alignment of 16 and 32 bit numbers.
**
**		For these reasons the following constant definitions
**		are useful for machine independent allocation.
**
**		These are based on the PDP11 compile flag.
**
**		QT_HDR_SIZ -- size of left and right pointers, typ,
**			len and padding
**		SYM_HDR_SIZ -- size of type and len in symbol
**			structure -- includes any padding before
**			the value field
**		TYP_LEN_SIZ -- size of type and len in symbol
**			structure -- without padding
**
**		INGRES FOLKS: don't change these back to sizeof's!!!
**			      The PDP-11 compiler doesn't understand!
*/

# ifdef PDP11
# define	QT_HDR_SIZ	6
# define	SYM_HDR_SIZ	2
# define	TYP_LEN_SIZ	2
# else
# define	QT_HDR_SIZ	12
# define	SYM_HDR_SIZ	4
# define	TYP_LEN_SIZ	2




# endif PDP11





/*
**  Query Tree Header.
**
**	Qt_ctx should be of type 'ctx_t *', but it is 'char *'
**		to insure that we don't need ctlmod.h.
*/

struct qthdr
{
	char	qt_active;		/* if set, Qt area is in use */
	char	*qt_ctx;		/* pointer to context */
	short	qt_qmode;		/* query mode */
	short	qt_resvar;		/* result variable number */
	RANGEV	qt_rangev[MAXRANGE];	/* the range table */
	short	qt_remap[MAXRANGE];	/* variable remapping (for QM) */
}  Qt;


# endif QT_HDR_SIZ
