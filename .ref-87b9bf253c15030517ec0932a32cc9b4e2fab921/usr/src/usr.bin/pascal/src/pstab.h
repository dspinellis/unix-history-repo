    /*	static	char sccsid[] = "@(#)pstab.h 1.3 %G%"; */

    /*
     *	subtypes within the above type
     *	subtypes N_PSO and N_PSOL are	.stabs	name,,0,subtype,0
     *	others subtypes are		.stabs	name,,0,subtype,line
     */
#define	N_PSO		0x1	/* source file name */
#define	N_PSOL		0x2	/* include file name */
#define	N_PGLABEL	0x3	/* global label */
#define	N_PGCONST	0x4	/* global constant */
#define	N_PGTYPE	0x5	/* global type */
#define	N_PGVAR		0x6	/* global variable */
#define	N_PGFUNC	0x7	/* global function */
#define	N_PGPROC	0x8	/* global procedure */
#define	N_PEFUNC	0x9	/* external function */
#define	N_PEPROC	0xa	/* external procedure */
#define	N_PLDATA	0xb	/* library variable */
#define	N_PLTEXT	0xc	/* library routine */
