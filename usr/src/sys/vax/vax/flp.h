/*	flp.h	3.1	%H%	*/

/*
 * Console floppy command/status and sectoring information.
 */
#define	FL_FFC		0x200		/* floppy function complete */
#define	FL_ERR		0x80		/* error bit in floppy status byte */
#define	FL_PERR		0x905		/* floppy protocol error */
#define	FL_DATA		0x100		/* floppy data select code */
#define	FL_RS		0x900		/* floppy read sector command */
#define	FL_WS		0x901		/* floppy write sector command*/
#define	FL_STAT		0x902		/* floppy get status command*/
#define	FL_CANCEL	0x904		/* cancel floppy function */

#define	RXFTRK	77		/* tracks/floppy */
#define	RXSTRK	26		/* sectors/track */
#define	RXBYSEC	128		/* bytes/sector */
#define	MAXSEC (RXFTRK*RXSTRK) 	/* sectors/floppy */

/*
 * In the floppy driver routines, the device active byte is used
 * not as a boolean, but as an indicator of the state we are in.
 * That is, it contains what to do on the next interrupt.
 */

#define	FL_IDLE		0	/* floppy idle */
#define	FL_MAND		1	/* about to send read/write command */
#define	FL_SEC		2	/* about to send sector # to LSI */
#define	FL_TRACK	3	/* about to send track # to LSI */
#define	FL_DAX		4	/* transmitting data */
#define	FL_DAR		5	/* receiving data */
#define	FL_COM		6	/* completing transmission */
#define	FL_CAN		7	/* give cancel order - we had an error,
				   and are to restart */

#define	FLERRS		5	/* number of retries before quitting */

/*
 * The state byte is used to retain exclusivity,
 * and contains the busy flag.
 */
#define	FL_OPEN		1
#define	FL_BUSY		2
