/*
**  CTLMOD.H -- control module header file
**
**	This header file defines all the internal data structures
**	used by the control module.
**
**	If something other than the control module or the initializer
**	has to include this, something is wrong!
**
**	Compilation Flags:
**		xCM_DEBUG -- if set, certain code is compiled in that
**			does consistancy checks on the data structures
**			at opportune times.
**		xMONITOR -- if set, turns on performance evaluation
**			code.
**		xCTR1, xCTR2, xCTR3 -- if set, turns on various levels
**			of trace information.  These must be properly
**			nested; if xCTR2 is set, xCTR1 *MUST* be set.
**
**	Version:
**		@(#)ctlmod.h	7.1	2/5/81
*/






/* include some other files */
# include	"state.h"
# include	"proc.h"
# include	<pv.h>
# include	"pipes.h"
# include	<func.h>
# include	<pmon.h>
# include	<setjmp.h>

/* basic constants */
# include	<useful.h>


/*
**  Trace Flag settings.
*/

# include	<trace.h>
# define	xCTR1		1
# define	xCTR2		1
# define	xCTR3		1
# define	xCM_DEBUG	1
# define	xMONITOR	1

/*
**  STRUCT CM -- the configuration structure.
*/

struct _cm_t
{
	char	cm_myname[12];		/* my process name */
	state_t	cm_state[CM_MAXST];	/* the state descriptions */
	proc_t	cm_proc[CM_MAXPROC];	/* the process descriptions */
	int	cm_myproc;		/* my process id */
	char	cm_input;		/* the current input file */
	char	cm_rinput;		/* the reset input file */
};

/*
**  STRUCT CTX -- the context structure.
**
**	There is one of these around for every currently known
**	context.  There is a pipe block associated with the
**	context, defined in call() or main(); only a pointer
**	is kept here so that the pipe block can be more efficiently
**	allocated off the stack.
**
**	Some of the fields describe the NEXT block in the sequence.
**	These are: ctx_size, ctx_link.
**
**	Ctx_cmark is useful ONLY when this block is not currently
**	active.
**
**	Ctx_qt should be of type 'struct qthdr *'; it is 'char *' to
**	avoid including qtree.h.  It points to the saved query
**	tree header after the first qt is read in.
**
**	Ctx_pv MUST be last.
*/

typedef struct _ctx_t
{
	char		*ctx_name;	/* the printname of this proc */
	pb_t		*ctx_ppb;	/* the pb associated w/ this ctx */
	short		*ctx_tvect;	/* the trace vect for this ctx */
	int		(*ctx_errfn)();	/* the error handling function */
	char		*ctx_qt;	/* pointer to saved Qt struct */
	struct fn_def	*ctx_fn;	/* pointer to fn descriptor */
	char		*ctx_glob;	/* ptr to saved global area */
	struct _ctx_t	*ctx_link;	/* a link to the next ctx */
	struct monitor	*ctx_mon;	/* ptr to monitor struct */
	short		ctx_size;	/* the size of the next ctx */
	bool		ctx_init;	/* set if between initp & call */
	bool		ctx_new;	/* set if this is a new context */
	char		ctx_resp;	/* process to respond to */
	int		ctx_cmark;	/* the Qbuf context mark */
	int		ctx_pmark;	/* the Qbuf parameter mark */
	jmp_buf		ctx_jbuf;	/* longjmp point on fatal error */
	long		ctx_ofiles;	/* files that should be kept open */
	int		ctx_pc;		/* the parm count */
	PARM		ctx_pv[PV_MAXPC+1];	/* the parm vector */
}  ctx_t;

extern ctx_t	Ctx;		/* the current context */
extern char	Qbuf[];		/* the free space buffer */
extern int	QbufSize;	/* the size of Qbuf */
extern struct _cm_t	Cm;	/* the system configuration */
extern int	Syncs[CM_MAXPROC];	/* the number of SYNCs expected */


/*
**  ERROR NUMBERS
*/

# define	ERR_QBUF	100	/* Qbuf overflow */
