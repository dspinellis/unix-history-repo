/*	ndu.h	4.1	85/03/19	*/

/*
 * This file defines the basic tree node data structure for the PCC.
 */

union ndu {
	struct {		/* interior node */
		int	op;
		int	rall;
		TWORD	type;
		int	su;
#ifndef FLEXNAMES
		char	name[NCHNAM];
#else
		char	*name;
		int	stalign;
#endif
		NODE	*left;
		NODE	*right;
	} in;
	struct {		/* terminal node */
		int	op;
		int	rall;
		TWORD	type;
		int	su;
#ifndef FLEXNAMES
		char	name[NCHNAM];
#else
		char	*name;
		int	stalign;
#endif
		CONSZ	lval;
		int	rval;
	} tn;
	struct {		/* branch node */
		int	op;
		int	rall;
		TWORD	type;
		int	su;
		int	label;		/* for use with branching */
	} bn;
	struct {		/* structure node */
		int	op;
		int	rall;
		TWORD	type;
		int	su;
		int	stsize;		/* sizes of structure objects */
		int	stalign;	/* alignment of structure objects */
	} stn;
	struct {		/* front node */
		int	op;
		int	cdim;
		TWORD	type;
		int	csiz;
	} fn;
	/*
	 * This structure is used when a double precision
	 * floating point constant is being computed
	 */
	struct {			/* DCON node */
		int	op;
		TWORD	type;
		int	cdim;
		int	csiz;
		double	dval;
	} dpn;
	/*
	 * This structure is used when a single precision
	 * floating point constant is being computed
	 */
	struct {			/* FCON node */
		int	op;
		TWORD	type;
		int	cdim;
		int	csiz;
		float	fval;
	} fpn;
};
