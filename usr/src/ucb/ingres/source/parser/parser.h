#
/*
**	COPYRIGHT
**
**	The Regents of the University of California
**
**	1977
**
**	This program material is the property of the
**	Regents of the University of California and
**	may not be reproduced or disclosed without
**	the prior written permission of the owner.
*/

/*
**	Version:
**		@(#)parser.h	7.1	2/5/81
*/



# define	DBUFSIZ		2000	/* size of buffer for dbu commands */
# define	TREEMAX		2500	/* max number of bytes for tree */
# define	MAXATT		150	/* max number of attributes in the att stash */

# define	V6POINT3COMPAT

# define	WARN		0	/* for a non fatal error */
# define	FATAL		1	/* for a fatal error */

/* mode parameters for range table manipulation */
# define	LOOKREL		1
# define	LOOKVAR		2
# define	R_INTERNAL	3
# define	R_EXTERNAL	4

# define	RELVUSED	01

/* the first argument in argv which may be an ad hoc flag */
# define	FREEFLAGS	6

/* error numbers */
# define	SUMMARY		2000	/* summary of errors */
# define	TREEOFLO	2118	/* over flow tree buffer */
# define	DBUFOFLO	2106	/* over flow dbu arg buffer */

# define	NOATTRIN	2100	/* attrib not in relation */
# define	CANTUPDATE	2107	/* can't update rel */
# define	NOVBLE		2109	/* vble not declared */
# define	NOPATMAT	2120	/* no pattern matching in tl */
# define	RNGEXIST	2117	/* can't find rel for var */
# define	REPALL		2123	/* x.all on replace */
# define	BADCONSTOP	2134	/* bad constant operator */

# define	INDEXTRA	2111	/* too many atts in key */
# define	RESXTRA		2130	/* too many resdoms in tl */
# define	TARGXTRA	2131	/* tl larger than MAXTUP */
# define	AGGXTRA		2132	/* too many aggs */

# define	MODTYPE		2119	/* type conflict for MOD */
# define	CONCATTYPE	2121	/* type conflict for CONCAT */
# define	AVGTYPE		2125	/* type conflict for AVG(U) */
# define	SUMTYPE		2126	/* type conflict for SUM(U) */
# define	FOPTYPE		2127	/* type conflict for func ops */
# define	UOPTYPE		2128	/* type conflict for unary ops */
# define	NUMTYPE		2129	/* type conflict for numeric ops */
# define	RELTYPE		2133	/* type conflict for relatv op */

# define	RESTYPE		2103	/* result type mismatch w/expr */
# define	RESAPPEX	2108	/* append res rel not exist */
# define	RESEXIST	2135	/* result rel already exists */

# define	NXTCMDERR	2500	/* misspelt where problem */
# define	NOQRYMOD	2139	/* no qrymod in database */

# define	BADHOURS	2136	/* no such hour */
# define	BADMINS		2137	/* no such minute */
# define	BAD24TIME	2138	/* only 24:00 can be used */

/* -- ASSORTED DATA STRUCTURES -- */
struct atstash					/* attribute table */
{
	char		atbid;			/* attribute number */
	char		atbfrmt;		/* attribute form type */
	char		atbfrml;		/* attribute form length */
	char		atbname[MAXNAME];	/* attribute name */
	struct atstash	*atbnext;		/* pointer to next entry in chain */
};

struct parrng				/* auxiliary range table */
{
	DESC		vardesc;
	struct parrng	*frontpt;
	struct parrng	*backpt;
	struct atstash	*attlist;		/* head of attrib list for this reln */
	int		relvused;		/* whether variable in use */
};

typedef struct parrng	 PARRNG;

struct constop				/* constant operator lookup table */
{
	char	*copname;		/* string name for identification */
	int	copnum;			/* op number */
	char	coptype;		/* op result type for formating */
	char	coplen;			/* op result length for formatting */
};
