/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
#ifdef MAIN
static char	*h_sccsid = "@(#)def.h	8.1 (down!honey) 86/01/19";
#endif /*MAIN*/
#endif /*lint*/

#include <stdio.h>
#include <ctype.h>
#include "config.h"

typedef	long Cost;
typedef struct node node;
typedef struct link link;

#ifdef lint
#define vprintf fprintf
#else /*!lint -- this gives null effect warning*/
/* because it's there ... */
#define vprintf		!Vflag ? 0 : fprintf
#endif /*lint*/

#define NTRACE	16	/* can trace up to NTRACE hosts/links */

/* scanner states (yylex, parse) */
#define OTHER 0
#define COSTING 1
#define NEWLINE 2

#define	isnetc(c)	((c)=='!' || (c)==':' || (c)=='@' || (c)=='%')

#define dirbits(c)	(c)

/* flags for n_flag */
#define ISPRIVATE  0x0001 /* this node invisible outside its definition file */
#define COLLISION  0x0002 /* collides with a private host name */
#define ATSIGN	   0x0004 /* seen an at sign?  used for magic @/% rules */
#define MAPPED	   0x0008 /* done mapping this node */
#define	NDEAD	   0x0010 /* node is dead */
#define HASLEFT	   0x0020 /* route has a left side net character */
#define HASRIGHT   0x0040 /* route has a right side net character */
#define	NNET	   0x0080 /* node is a network name */
#define INDFS	   0x0100 /* used when removing net cycles */
#define DUMP	   0x0200 /* we have dumped this net's edges */
#define GATEWAYIN  0x0400 /* heaped via gatewayed net */

#define ISADOMAIN(n) ((n)->n_name[0] == '.')
#define ISANET(n) (((n)->n_flag & NNET) || ISADOMAIN(n))
#define DEADHOST(n) (((n)->n_flag & (NNET | NDEAD)) == NDEAD)
#define GATEWAYED(n) (((n)->n_flag & (NNET | NDEAD)) == (NNET | NDEAD) || ISADOMAIN(n))


/*
 * save some space in nodes -- there are > 10,000 allocated!
 *
 *	node	*n_net		others in this network (parsing)
 * 	node	*n_root		root of net cycle (mapping)
 *
 *	node	*n_private	other privates in this file (parsing)
 *	char	*n_parent	parent in shortest path tree (mapping)
 *		
 */

#define n_root n_net_root
#define n_net n_net_root

#define n_private n_private_parent
#define n_parent  n_private_parent

/* WARNING: if > 2^16 nodes, type of n_tloc must change */
struct node {
	char	*n_name;	/* host name */
	link	*n_link;	/* head of adjacency list */
	node 	*n_net_root;
	node	*n_private_parent;
	Cost	n_cost;		/* cost to this host */
	unsigned short	n_tloc;	/* back ptr to heap/hash table */
	short	n_flag;		/* see manifests above */
};

#define	DEFNET	'!'			/* default network operator */
#define	DEFDIR	LLEFT			/* host on left is default */
#define	DEFCOST	((Cost) 4000)		/* default cost of a link */
#define	INF	((Cost) 30000000)	/* infinitely expensive link */

/* data structure for adjacency list representation */

/* flags for l_dir */

/*
 * there's an ugly dependency between the following manifests and the
 * variable Netchars = "!:^@%", defined in extern.c.  this saves 2
 * bytes per link (of which there are well over 20k).  this does not
 * mean i'm satsified with bad design.
 */
#define NETDIR(l)	((l)->l_flag & LDIR)
#define NETCHAR(l)	(Netchars[(l)->l_flag & LNETCHARS])

#define LNETCHARS	0x3
#define LBANG		0x0
#define LCOLON		0x1
#define LAT		0x2
#define LPERCENT	0x3

#define LDIR	0x8	/* 0 for left, 1 for right */
#define LRIGHT	0x0	/* user@host style */
#define LLEFT	0x8	/* host!user style */

#define LDEAD	 0x10	/* this link is dead */
#define LTREE	 0x20	/* member of shortest path tree */
#define LALIAS	 0x40	/* alias */
#define LGATEWAY 0x80	/* this link is a gateway */

/*
 * borrow a field for link/node tracing
 *
 *	link	*l_next;	rest of adjacency list (not tracing)
 *	link	*l_from;	source node (tracing) -- requires a cast
 *		
 */

#define l_next	l_next_from
#define l_from	l_next_from

struct link {
	link	*l_next_from;
	node	*l_to;		/* adjacent node */
	Cost	l_cost;		/* edge cost */
	char	l_flag;		/* right/left syntax */
};

/*
 * static functions don't show up in prof(1), so ...
 * someday i'll be done profiling.
 * yeah, sure, like when hell freezes over.
 */
#define STATIC /*static*/

/* external functions */
extern node	*addnode(), *newnode(), **newtable(), *addprivate();
extern link	*addlink(), *addgateway(), *newlink();
extern char	*strsave(), *local();
extern void	pack();

/* external variables */
extern char	*optarg;
extern int	optind;
extern node	*Home;
extern char	*Cfile;
extern char	**Ifiles;
extern char	*ProgName;
extern int	Lineno;
extern node	**Table;
extern long	Tabsize;
extern char	*Netchars;
extern int	Vflag;
extern int	Cflag;
extern int	Iflag;
extern int	Tflag;
extern int	Ncount;
extern int	Lcount;
extern char	*Graphout;
extern char	*Linkout;
extern node	*Private;
extern long	Hashpart;
extern int	Scanstate;
