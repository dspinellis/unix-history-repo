/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)specialreg.h	7.1 (Berkeley) %G%
 */

/*
 * 386 Special registers:
 */

#define	CR0_PE	0x00000001	/* Protected mode Enable */
#define	CR0_MP	0x00000002	/* "Math" Present (e.g. npx), wait for it */
#define	CR0_EM	0x00000004	/* EMulate NPX, e.g. trap, don't execute code */
#define	CR0_TS	0x00000008	/* Process has done Task Switch, do NPX save */
#define	CR0_ET	0x00000010	/* 32 bit (if set) vs 16 bit (387 vs 287) */
#define	CR0_PG	0x80000000	/* Paging Enable */
