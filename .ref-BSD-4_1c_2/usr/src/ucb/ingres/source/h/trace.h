/*
**  TRACE.H -- conditional compilation flags
**
**	Some of these may be commented out to set them to "off".
**	The comment should be removed to set them to "on".
**
**	Version:
**		@(#)trace.h	7.1	2/5/81
*/

/* access methods compilation flags */

/*	disable timing information
# define	xATM		/* timing information */
# define	xATR1		/* trace info, level 1 */
# define	xATR2		/* trace info, level 2, implies xTR1 */
# define	xATR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* decomposition compilation flags */

/*	disable timing information
# define	xDTM		/* timing information */
# define	xDTR1		/* trace info, level 1 */
# define	xDTR2		/* trace info, level 2, implies xTR1 */
# define	xDTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* EQUEL compilation flags */

/*	disable timing information
# define	xETM		/* timing information */
# define	xETR1		/* trace info, level 1 */
# define	xETR2		/* trace info, level 2, implies xTR1 */
# define	xETR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* monitor compilation flags */

/*	disable timing information
# define	xMTM		/* timing information */
# define	xMTR1		/* trace info, level 1 */
# define	xMTR2		/* trace info, level 2, implies xTR1 */
# define	xMTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* OVQP compilatiion flags */

/*	disable timing information
# define	xOTM		/* timing information */
# define	xOTR1		/* trace info, level 1 */
# define	xOTR2		/* trace info, level 2, implies xTR1 */
# define	xOTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* parser compilation flags */

/*	disable timing information
# define	xPTM		/* timing information */
# define	xPTR1		/* trace info, level 1 */
# define	xPTR2		/* trace info, level 2, implies xTR1 */
# define	xPTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* qrymod compilation flags */

/*	disable timing information
# define	xQTM		/* timing information */
# define	xQTR1		/* trace info, level 1 */
# define	xQTR2		/* trace info, level 2, implies xTR1 */
# define	xQTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* scanner compilation flags */

/*	disable timing information
# define	xSTM		/* timing information */
# define	xSTR1		/* trace info, level 1 */
# define	xSTR2		/* trace info, level 2, implies xTR1 */
# define	xSTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* DBU compilation flags */

/*	disable timing information
# define	xZTM		/* timing information */
# define	xZTR1		/* trace info, level 1 */
# define	xZTR2		/* trace info, level 2, implies xTR1 */
# define	xZTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/* support compilation flags */

/*	disable timing information
# define	xTTM		/* timing information */
# define	xTTR1		/* trace info, level 1 */
# define	xTTR2		/* trace info, level 2, implies xTR1 */
# define	xTTR3		/* trace info, level 3, implies xTR1 & xTR2 */

/*
**  Inline expansion for trace flags
*/

extern short	*tT;
# ifndef tTf
# define	tTf(a, b)	((b < 0) ? tT[a] : (tT[a] & (1 << b)))
# endif tTf
