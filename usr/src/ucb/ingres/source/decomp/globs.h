# include	<tree.h>
# include	<func.h>
# include	<access.h>
# include	"../decomp/decomp.h"
# include	"../ovqp/ovqp.h"

/*	@(#)globs.h	7.3	3/7/81	*/


struct agglist
{
	QTREE	**father;	/* addr of pointer to you */
	QTREE	*agpoint;	/* pointer to aghead */
	QTREE	*agfather;	/* is your father an aggregate? */
	short	agvarno;	/* var # assigned to aggr fnct */
};

struct hitlist
{
	QTREE	**trepr;	/* position in tree to be changed */
	short	byno;		/* by-list position */
};

struct rang_tab
{
	int		relnum;		/* internal decomp relation number */
	int		rtspec;		/* relspec of relation */
	int		rtstat;		/* relstat of relation */
	int		rtwid;		/* relwidth of relation */
	long		rtcnt;		/* tupcount of relation */
	int		rtaltnum;	/* reserved for distributed decomp */
	char		*rtattmap;	/* reserved for distributed decomp */
	long		rtdcnt;		/* reserved for distributed decomp */
	struct d_range	*rtsrange;	/* reserved for distributed decomp */
};


/* The following structure reserved for distributed decomp */
/* The order of this has been changed for proper alignment */
struct d_range
{
	int		relnum;
	int		draltnum;
	long		drtupcnt;
	struct d_range	*drnext;
	int		drstat;
	char		drsite[2];
};


/* structure used by reduction to maintain component pieces */
struct complist
{
	struct complist	*nextcomp;	/* next comp piece */
	struct complist	*linkcomp;	/* next clause of this comp */
	QTREE		*clause;	/* associated clause */
	int		bitmap;		/* map of all assoc clauses */
};

bool	Batchupd;

struct desc_tab
{
	int	relnum;		/* relation number */
	char	dtmode;		/* status of descriptor */
	char	dtpos;		/* position of last access */
	DESC	desc;		/* descriptor */
};


struct stacksym
{
	char	s_type;
	char	s_len;
	long	s_value[2];
};				/* stack for OVQP interpreter */


struct simp
{
	int	relop;	/* value of relop in simp clause*/
	int	att;	/* attno of attribute */
	SYMBOL	*const;	/* pointer to constant value symbol */
};


struct key
{
	SYMBOL	*keysym;
	int	dnumber;
};

extern int	Equel;

struct
{
		/* OVQP variables */
	char		ov_outtup[MAXTUP];
	char		ov_intup[MAXTUP];
	char		*ov_origtup;
	char		*ov_tend;	/* pts to end of data in ov_outtup */
	short		ov_bopen;	/* TRUE if batch file is open */
	short		ov_targvc;	/* var count in Target list (flags constant Targ. list) */
	short		ov_qualvc;	/* var count in Qual list */
	short		ov_userqry;	/* flags a query on the users's result rel */
	short		ov_retrieve;	/* true is a retrieve, else false */
	char		*ov_ovqpbuf;
	short		ov_diffrel;	/* true is ov_source and ov_result are different */
	short		ov_agcount;	/* count of the # of aggregates in the query */
	short		ov_qvpointer;
	long		ov_intid;
	long		ov_uptid;
	long		*ov_counter;	/* cnts "gets" done in OVQP */
	long		ov_lotid;
	long		ov_hitid;	/* lo & hi limits of scan in OVQP */
	long		ov_tupsfound;	/* counts # tuples which satified the query */
	DESC		*ov_scanr;	/* pts to desc of reln to be scanned */
	DESC		*ov_source;	/* 0 if no source for qry, else poshorts to ov_srcdesc */
	DESC		*ov_result;	/* 0 if no result for qry, else poshorts to ov_reldesc */
	SYMBOL		**ov_tlist;
	SYMBOL		**ov_alist;
	SYMBOL		**ov_qlist;
	SYMBOL		**ov_bylist;
	struct stacksym	ov_stack[STACKSIZ];
	char		ov_keyl[MAXTUP];
	char		ov_keyh[MAXTUP];
	int		ov_nsimp;	/* Current no. entries in ov_simp vector */
	int		ov_fmode;	/* find-mode determined by strategy */
	struct simp	ov_simp[NSIMP];
	struct key	ov_lkey_struct[MAXDOM+1];
	struct key	ov_hkey_struct[MAXDOM+1];

		/* DECOMP/OVQP variables */

	struct agglist	*de_aggnext;	/* next in aggregate list */
	struct agglist	*de_agglim;	/* limit in aggregate list */
	struct hitlist	*de_hnext;
	struct hitlist	*de_hlimit;
	char		de_d_dbu70;
	char		de_d_ovqp70;
	int		de_synconly;
	int		de_error_flag;
	int		de_qvptr;		/* index into available de_qvect space in ovqpnod() */
	SYMBOL		*de_qvect[MAXNODES];
	short		de_newq;		/* OVPQ must devise new strategy */
	short		de_newr;		/* force OVQP to reopen result relation */
	int		de_qmode;		/* flag set to indicate mode of tuple disposition */
	int		de_resultvar;		/* if >= 0 result variable */
	int		de_sourcevar;		/* likewise for source variable */
	char		*de_qbuf;		/* pointer to query buffer */



	QTREE		*de_qle;		/* ptr to QLEND node */
	QTREE		*de_tr;			/* ptr to TREE node */
	int		de_dfiles;
	int		de_dopnfiles;
	struct desc_tab	de_desc[MAXRELN];	/* descriptors available for use */
	struct rang_tab	de_rangev[MAXRANGE+2];	/* global range table with extra slot for FREEVAR and SECINDVAR */
	int		de_qry_mode;		/* mode of original query (not nec same as de_qmode) */
	char		de_name_table[FIRSTNUM-1][MAXNAME];
	char		de_num_used[LASTNUM+1];
	char		de_buflag;
} De;

short		tTdecomp[100];
# ifdef tTf
# undef tTf
# endif tTf
# define tTf(a, b)	((b < 0) ? tTdecomp[a] : (tTdecomp[a] & (1 << b)))
