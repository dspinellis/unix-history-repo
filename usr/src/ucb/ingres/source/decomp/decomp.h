/*
**	This header file contains all the defined constant
**	and special structures used by decomposition. Certain
**	global variables which are referenced by many modules
**	are also included. By convention global names always
**	begin with a capital letter.
**
**	Version:
**		@(#)decomp.h	7.2	3/5/81
*/


# include	<pv.h>


   
# define OVHDP		2		/*  overhead for a projection  */
# define OVHDM		10		/*  overhead for a modify  */

# define MAXRELN	6		/* size of relation descriptor cache */
  
# define QBUFSIZ	2000		/* buffer size (bytes) of original query tree */
# define SQSIZ		10000		/* buffer size for tree copies + sub-queries */
# define AGBUFSIZ	350		/* buffer size for temp agg tree components */
# define PBUFSIZE	500		/* size of parameter buffer area for setp() */
# define PARGSIZE	PV_MAXPC	/* max number of arguments for setp() */

/* error messages */
# define NODESCAG	4602	/* no descriptor for aggr func */
# define QBUFFULL	4610	/* Initial query buffer overflow */
# define SQBUFFULL	4612	/* sub-query buffer overflow */
# define STACKFULL	4613	/* trbuild stack overflow */
# define AGBUFFULL	4614	/* agg buffer overflow */
# define AGFTOBIG	4615	/* agg function exceeds MAXTUP or MAXDOM */
# define TOOMANYAGGS	4616	/* more than MAXAGG aggregates */
# define RETUTOBIG	4620	/* retr unique target list exceeds MAXTUP */

/* symbolic values for GETNXT parameter of fcn GET */
# define NXTTUP	1	/* get next tuple after one specified by tid */

/* flag for no result relation */
# define	NORESULT	-1

/* Range table slot which is always free for aggregate temp rels */
# define	FREEVAR		MAXRANGE	/* free var number for aggs */

/* Range table slot which is used for secondary index */
# define	SECINDVAR	MAXRANGE + 1



# define	FIRSTNUM	MAXRANGE + 3
# define	LASTNUM		100
