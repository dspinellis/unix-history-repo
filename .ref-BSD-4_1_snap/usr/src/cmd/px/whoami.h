/* Copyright (c) 1979 Regents of the University of California */

/* static	char sccsid[] = "@(#)piwhoami.h 1.2 3/6/81"; */

/*
 *	am i generating an obj file (OBJ),
 *	postfix binary input to the 2nd pass of the portable c compiler (PC),
 *	or pTrees (PTREE)?
 */
#define	OBJ
#undef	PC
#undef	PTREE

/*
 *	am i the vax or the pdp11 version
 */
#define VAX
#undef	PDP11
#define DEC11

/*
 *	am i pi or pxp?
 */
#define PI
#undef	PXP

/*
 *	am i both passes, or am i only one of the two passes pi0 or pi1?
 */
#define	PI01
#undef	PI0
#undef	PI1

