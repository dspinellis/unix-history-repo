/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)x25err.h	7.2 (Berkeley) %G%
 */

/* 
 *  
 *  X.25 Reset and Clear errors and diagnostics.  These values are 
 *  returned in the u_error field of the u structure.
 *
 */

#define EXRESET		100	/* Reset: call reset			*/
#define EXROUT		101	/* Reset: out of order			*/
#define EXRRPE		102	/* Reset: remote procedure error	*/
#define EXRLPE		103	/* Reset: local procedure error		*/
#define EXRNCG		104	/* Reset: network congestion		*/

#define EXCLEAR		110	/* Clear: call cleared			*/
#define EXCBUSY 	111	/* Clear: number busy			*/
#define EXCOUT		112	/* Clear: out of order			*/
#define EXCRPE		113	/* Clear: remote procedure error	*/
#define EXCRRC		114	/* Clear: collect call refused		*/
#define EXCINV		115	/* Clear: invalid call			*/
#define EXCAB		116	/* Clear: access barred			*/
#define EXCLPE		117	/* Clear: local procedure error		*/
#define EXCNCG		118	/* Clear: network congestion		*/
#define EXCNOB		119	/* Clear: not obtainable		*/

