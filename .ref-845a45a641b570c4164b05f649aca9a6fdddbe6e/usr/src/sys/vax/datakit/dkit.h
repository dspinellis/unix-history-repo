/*
 * 	DATAKIT VCS Interface Definitions
 *		@(#)dkit.h	1.4 Garage 84/03/30
 *		   @(#)dkit.h	1.2 (Berkeley) %G%
 */

/*
 *	Host Software Version number
 */

#define	HOSTVERSION	2	/* Change the string below when you update this */
#define	S_HOSTVERSION	"@(#) Datakit Host Protocol Version 2"

/*
 *   bits defined in channel flag
 */

#define DK_OPEN 	01	/* channel is open flag */
#define DK_LINGR	02	/* closed by user, waiting sync */
#define DK_BUSY		04	/* output in progress */
#define DK_RESET	010	/* cmc told us to close this channel */
#define DK_RCV		020	/* receive active on channel */
#define	DK_RCVQ		040	/* receive done queued 'cause timer */
#define	DK_SPND		0100	/* output is currently suspended */



/*
 *   command codes passed to dkit_cmd
 */

#define	DKC_XINIT	01	/* re-init transmitter section */
#define	DKC_FLUSH	02	/* flush all pending output */
#define	DKC_SPND	04	/* suspend further output */
#define	DKC_RSME	010	/* resume output after suspend */


/*
 *   receive mode, and completion indicators
 */

#define	DKR_FULL	01	/* buffer full, normal read done */
#define	DKR_CNTL	02	/* read terminated on control character */
#define	DKR_ABORT	010	/* receive aborted by higher level command */
#define	DKR_BLOCK	040	/* end of block */
#define	DKR_TIME	0100	/* end of time limit reached */

/*
 *   Datakit-specific mbuf types
 */
#define	DKMT_HDR	16		/* Header packet with driver stuff */
#define	DKMT_DATA	DKMT_HDR+1	/* Data */
#define	DKMT_CTL	DKMT_HDR+2	/* Control bytes */
#define	DKMT_PCB	DKMT_HDR+3	/* Setup request block */
#define	DKMT_ROUTE	DKMT_HDR+4	/* dkip routing table entry */
#define	DKMT_ITTY	DKMT_HDR+5	/* dktty input buffer */
#define	DKMT_OTTY	DKMT_HDR+6	/* dktty output buffer */
#define	DKMT_CTYPE	DKMT_HDR+7	/* Connection source/dest */
