/*
 *	@(#)psl.h	7.1 (Berkeley) 5/21/88
 */

/*
 * TAHOE processor status longword.
 */
#define	PSL_C		0x00000001	/* carry bit */
#define	PSL_V		0x00000002	/* overflow bit */
#define	PSL_Z		0x00000004	/* zero bit */
#define	PSL_N		0x00000008	/* negative bit */
#define	PSL_ALLCC	0x0000000f	/* all cc bits - unlikely */
#define	PSL_T		0x00000010	/* trace enable bit */
#define	PSL_IV		0x00000020	/* integer overflow enable bit */
#define	PSL_FU		0x00000040	/* float underflow enable 	*/
#define PSL_DBL		0x00000080	/* f.p. prescision indicator	*/
#define	PSL_SFE		0x00000100	/* system-forced-exception */
#define	PSL_IPL		0x001f0000	/* interrupt priority level */
#define	PSL_PRVMOD	0x00000000	/* previous mode (kernel mode) */
#define	PSL_CURMOD	0x01000000	/* current mode (all on is user) */
#define	PSL_IS		0x04000000	/* interrupt stack */
#define	PSL_TP		0x40000000	/* trace pending */

#define	PSL_MBZ		0xbae0fe00	/* must be zero bits */

#define	PSL_USERSET	(PSL_CURMOD)
#define	PSL_USERCLR	(PSL_IS|PSL_IPL|PSL_MBZ|PSL_SFE|PSL_DBL|PSL_FU)
