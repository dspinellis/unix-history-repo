/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)rodata.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * adb - machine dependent read-only data
 */

#include "defs.h"
#include <machine/reg.h>

#define	N(arr)	(sizeof(arr) / sizeof(arr[0]))

/*
 * Registers.  The offset value is an offset from u.u_ar0 if negative,
 * or if positive, is an offset into the pcb in u.u_pcb.  PCC will not
 * let us scale the pcb offsets (grr) so instead we scale the ar0 offsets.
 *
 * The `address in pcb' is in the local copy of the kernel pcb, for use
 * with kernel dumps.
 *
 * The registers are printed in the order they are listed here.
 */
extern struct pcb pcb;
#define	pcboff(field)	(int)&((struct pcb *)0)->field
#define	ar0off(off)	off * 4
struct reglist reglist[] = {
	/* name		offset			address in pcb */
	{ "p1lr",	pcboff(pcb_p1lr),	&pcb.pcb_p1lr },
	{ "p1br",	pcboff(pcb_p1br),	(int *)&pcb.pcb_p1br },
	{ "p0lr",	pcboff(pcb_p0lr),	&pcb.pcb_p0lr },
	{ "p0br",	pcboff(pcb_p0br),	(int *)&pcb.pcb_p0br },
	{ "ksp",	pcboff(pcb_ksp),	&pcb.pcb_ksp },
	{ "esp",	pcboff(pcb_esp),	&pcb.pcb_esp },
	{ "ssp",	pcboff(pcb_ssp),	&pcb.pcb_ssp },
	{ "psl",	ar0off(PS),		&pcb.pcb_psl },
	{ "pc",		ar0off(PC),		&pcb.pcb_pc },
	{ "usp",	ar0off(SP),		&pcb.pcb_usp },
	{ "fp",		ar0off(FP),		&pcb.pcb_fp },
	{ "ap",		ar0off(AP),		&pcb.pcb_ap },
	{ "r11",	ar0off(R11),		&pcb.pcb_r11 },
	{ "r10",	ar0off(R10),		&pcb.pcb_r10 },
	{ "r9",		ar0off(R9),		&pcb.pcb_r9 },
	{ "r8",		ar0off(R8),		&pcb.pcb_r8 },
	{ "r7",		ar0off(R7),		&pcb.pcb_r7 },
	{ "r6",		ar0off(R6),		&pcb.pcb_r6 },
	{ "r5",		ar0off(R5),		&pcb.pcb_r5 },
	{ "r4",		ar0off(R4),		&pcb.pcb_r4 },
	{ "r3",		ar0off(R3),		&pcb.pcb_r3 },
	{ "r2",		ar0off(R2),		&pcb.pcb_r2 },
	{ "r1",		ar0off(R1),		&pcb.pcb_r1 },
	{ "r0",		ar0off(R0),		&pcb.pcb_r0 },
	0
};

/* names for codes for illegal instruction */
char	*illinames[] = {
	" (reserved addressing fault)",
	" (priviliged instruction fault)",
	" (reserved operand fault)"
};
int	nillinames = N(illinames);

/* names for codes for floating point exception */
char	*fpenames[] = {
	"",
	" (integer overflow trap)",
	" (integer divide by zero trap)",
	" (floating overflow trap)",
	" (floating/decimal divide by zero trap)",
	" (floating underflow trap)",
	" (decimal overflow trap)",
	" (subscript out of range trap)",
	" (floating overflow fault)",
	" (floating divide by zero fault)",
	" (floating underflow fault)",
};
int	nfpenames = N(fpenames);
