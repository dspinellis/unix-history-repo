/*
 *	@(#)reg.h	7.1 (Berkeley) %G%
 *	from reg.h	4.2	81/02/19
 */

/*
 * Location of the users' stored
 * registers relative to PSL of 'trap' and 'syscall'.
 * Usage is u.u_ar0[XX].
 */

#define	PS	(-1)
#define	PC	(-2)
/*		(-3)	*/
/*		(-4)	*/
#define	RACL	(-5)
#define	RACH	(-6)
/*		(-7)	*/
/*		(-8)	*/
#define	SP	(-9)
#define	R13	(-10)
#define	FP	(-10)
#define	R12	(-13)
#define	R11	(-14)
#define	R10	(-15)
#define	R9	(-16)
#define	R8	(-17)
#define	R7	(-18)
#define	R6	(-19)
#define	R5	(-20)
#define	R4	(-21)
#define	R3	(-22)
#define	R2	(-23)
#define	R1	(-24)
#define	R0	(-25)

#ifdef IPCREG
#define	NIPCREG 18
int ipcreg[NIPCREG] =
	{R0,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,FP,SP,PC,RACH,RACL};
#endif
