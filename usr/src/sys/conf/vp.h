/*	vp.h	4.1	11/9/80	*/

/*
 * Is there a versatec?
 */
#define	NVP	1

/*
 * UNIBUS address of versatec
 */
#define	VPADDR	((struct vpregs *)(UBA0_DEV + 0177500))
