# include	<ingres.h>
# include	<range.h>
# include	<tree.h>
# include	<func.h>
# include	<pv.h>
# include	"parser.h"
# include	<sccs.h>

SCCSID(@(#)parser.c	7.1	2/5/81)

short			tTparser[100];

extern int		parser();
extern int		par_init();

struct fn_def		ParserFn =
			{
				"PARSER",
				parser,
				par_init,
				NULL,
				NULL,
				0,
				tTparser,
				100,
				'P',
				0
			};

DESC	Reldesc;
struct atstash		Attable[MAXATT];/* attrib stash space, turned into a list later */
struct atstash		*Freeatt;	/* free list of attrib stash */
QTREE	*Tidnode;	/* pointer to tid node of targ list
					   for REPLACE, DELETE */
QTREE	*Lastree;	/* pointer to root node of tree */
extern struct atstash	Faketid;	/* atstash structure for TID node */
#ifdef	DISTRIB
extern struct atstash	Fakesid;	/* atstash structure for SID node */
#endif

int			Rsdmno;		/* result domain number */
int			Opflag;		/* operator flag contains query mode */
char			*Relspec;	/* ptr to storage structure of result relation */
char			*Indexspec;	/* ptr to stor strctr of index */
char			*Indexname;	/* ptr to name of index */
char			Trfrmt;		/* format for type checking */
char			Trfrml;		/* format length for type checking */
char			*Trname;	/* pointer to attribute name */
int			Agflag;		/* how many aggs in this qry */
int			Equel;		/* indicates EQUEL preprocessor on */
int			Ingerr;		/* set to error num if a query returns
					   an error from processes below */
int			Patflag;	/* signals a pattern match reduction */
int			Qlflag;		/* set when processing a qual */
int			Noupdt;		/* INGRES user override of no update restriction */
int			Err_fnd;	/* no actions done if 1 */
int			Err_current;	/* 1 if error found in current statement */
int			yyline;		/* line counter */
int			Dcase;		/* default case mapping */
int			Permcomd;
int			Qrymod;		/* qrymod on in database flag */


/*
**  PARSER -- the actual main routine
**
**	Trace Flags:
**		Parser ~~ 64
*/

parser()
{

# ifdef	xPTR1
	tTfp(64, 0, "Parser %d\n", getpid());
# endif

	if (startgo() < 0)
	{
		endgo();
		return (-1);
	}

	if (yyparse())		/* yyparse returns 1 in case of error */
	{
		endgo();
		return (-2);
	}

	if (endgo() < 0)
		return (-3);

	return(0);
}
