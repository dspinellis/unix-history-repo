/*
**  FUNC.H -- declarations for function headers.
**
**	Version:
**		@(#)func.h	7.1	2/5/81
*/


/* the function definition struct */
struct fn_def
{
	char		*fn_name;	/* the name of the function */
	int		(*fn_fn)();	/* a pointer to the actual function */
	int		(*fn_initfn)();	/* initialization function */
	int		(*fn_cleanup)();/* interrupt cleanup function */
	char		*fn_gptr;	/* pointer to global space */
	unsigned	fn_gsize;	/* size of global space */
	short		*fn_tvect;	/* the trace vector itself */
	short		fn_tsize;	/* size of trace vector */
	char		fn_tflag;	/* the trace flag letter */
	char		fn_active;	/* > 0 if active */
};

extern struct fn_def	*FuncVect[];
extern int		NumFunc;	/* the number of functions */
