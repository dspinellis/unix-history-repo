/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pstab.h	5.1 (Berkeley) %G%
 */

    /*
     *	subtypes within the stab type N_PC
     *
     *	subtypes N_PSO and N_PSOL are	.stabs	name,N_PC,0,subtype,checksum
     *	others subtypes are		.stabs	name,N_PC,0,subtype,line
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

    /*
     *	checksums are used to check if included files have changed.
     *	we also use them to check that .o files are up to date with
     *	the libraries.
     *	if a checksum is less than the flag checksum,
     *	then the checksum (and therefore the .o file) is out of date.
     *	if a checksum is equal to the flag checksum,
     *	then no furthur checking of the checksum is done.
     *	this is for the 2nd and subsequent times a file is stabed.
     *	to declare a flag day, increment this value. (also be sure 
     *  to update this value in ../utilities/externs.awk)
     */
#define	N_FLAGCHECKSUM	1
