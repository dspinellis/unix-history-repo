# include	<ingres.h>
# include	<aux.h>
# include	<pv.h>
# include	<opsys.h>
# include	<func.h>
# include	<tree.h>
# include	"qrymod.h"
# include	<sccs.h>

SCCSID(@(#)qrymod.c	7.1	2/5/81)


/*
**  QRYMOD -- query modification process
**
**	This process modifies queries to implement views, protection,
**	and integrity.
**
**	Return Codes:
**		standard
**
**	Trace Flags:
**		none.
*/



DESC		Prodes;		/* protection catalog descriptor */
DESC		Reldes;		/* relation catalog descriptor */
DESC		Treedes;	/* tree catalog descriptor */
DESC		Intdes;		/* integrity catalog descriptor */
extern int	Equel;		/* equel flag */

# define TTYIDSIZE	8	/* length of tty id */

extern	qrymod(), qm_init(), null_fn();
short	tTqm[80];
char	Terminal[TTYIDSIZE + 1];

struct fn_def	QryModFn =
{
	"QRYMOD",
	qrymod,
	qm_init,
	null_fn,
	(char *) &Qm,
	sizeof Qm,
	tTqm,
	80,
	'Q',
	0,
};



qm_init(argc, argv)
int	argc;
char	**argv;
{
#	ifdef xV7_UNIX
	extern char	*ttyname();
	extern char	*rindex();
	char		*tty;
#	endif

	/* determine user's terminal for protection algorithm */
#	ifndef xV7_UNIX
	pmove("tty", Terminal, TTYIDSIZE, ' ');
	Terminal[3] = ttyn(1);
	if (Terminal[3] == 'x')
		pmove(" ", Terminal, TTYIDSIZE, ' ');
#	else
	tty = rindex(ttyname(1), '/') + 1;
	pmove((tty != NULL ? tty : " "), Terminal, TTYIDSIZE, ' ');
#	endif
	Terminal[TTYIDSIZE] = '\0';
# ifdef xQTR1
	if (tTf(75, 0))
		printf("Terminal = \"%s\"\n", Terminal);
# endif
}
/*
**  QRYMOD -- main driver for query modification
**
**	Reads in the query tree, performs the modifications, writes
**	it out, and does process syncronization with below.  The
**	calling routine must sync with the process above.
**
**	Parameters:
**		pc -- parameter count (must = 1).
**		pv -- parameter vector:
**			pv[0] -- tree to modify.
**
**	Returns:
**		zero.
**
**	Side Effects:
**		The tree is modified to one that is guaranteed to
**		be directly processable.
**
**	Trace Flags:
**		none.
*/


qrymod(pc, pv)
int	pc;
PARM	*pv;
{
	register QTREE	*root;
	extern QTREE	*view(), *integrity(), *protect();

	/*
	**  Get parameters.
	*/

	if (pc != 1)
		syserr("pc=%d", pc);
	if (pv[0].pv_type != PV_QTREE)
		syserr("pv[0].type=%d", pv[0].pv_type);
	root = pv[0].pv_val.pv_qtree;

	/* view processing */
	root = view(root);

	/* integrity processing */
	root = integrity(root);

	/* protection processing */
	root = protect(root);

	return (0);
}
