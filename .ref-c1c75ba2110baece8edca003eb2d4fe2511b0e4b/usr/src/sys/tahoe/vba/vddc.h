/*	vddc.h	1.1	85/07/21	*/

/*
**	Header file for the VDDC (Versabus Direct Disk Controller) Driver
*/

#define	NSECPTRK 32		/* #sectors/track - fixed by VDDC */
#define SECTSIZ 512		/* sector size fixed by VDDC */
#define L2SIZ	9		/* log2 of sector size */
#define L2BSIZ	10		/* log2 of block size */
#define NVDDRV	3		/* number of drive types supported */

/*
**	DCB Command Codes
*/

#define	RD	0x80		/* Read Data */
#define	FTR	0xc0		/* Full Track Read */
#define	RAS	0x90		/* Read and Scatter */
#define	C	0xa0		/* Compare */
#define	FTC	0xe0		/* Full Track Compare */
#define	RHDE	0x180		/* Read Header, Data & ECC (not used) */
#define	WD	0x00		/* Write Data */
#define	FTW	0x40		/* Full Track Write */
#define	WTC	0x20		/* Write Then Compare */
#define	FTWTC	0x60		/* Full Track Write Then Compare */
#define	GAW	0x10		/* Gather and Write */
#define	WDE	0x100		/* Write Data & ECC (not used) */
#define	FSECT	0x900		/* Format Sector */
#define	GWC	0x30		/* Gather Write & Compare */
#define	VDSTART 0x800		/* Start drives */
#define	VDRELEASE 0xa00		/* Stop drives */
#define	SEEK	0xb00		/* Seek */
#define	INIT	0xc00		/* Initialize VDDC */
#define	DIAG	0xd00		/* Diagnose (self-test) VDDC */
#define	RSTCFG	0xe00		/* Reset/Configure VDDC/DDI/Drive(s) */
#define	VDSTATUS   0xf00		/* VDDC Status */
#define	ABORT	0x80000000	/* To be written to VDDC Cntrl Register */

/*
 * Error/Status Symbolic Constants
 */
#define	HCRCERR		0x1		/* Header CRC Error */
#define	HCMPERR		0x2		/* Header Compare Error */
#define	WPTERR		0x4		/* Write Protect Error/Status */
#define	SZTIMEOUT	0x8		/* Seize timeout Error */
#define	DSEEKERR	0x10		/* Disk Seek Error */
#define	UCDATERR	0x20		/* Uncorrectable Data Error */
#define	NOTCYLERR	0x40		/* Not on Cylinder Error */
#define	DRVNRDY		0x80		/* Drive Not Ready Error/Status */
#define	ALTACC		0x100		/* Alternate (track) accessed Status */
#define	SEEKSTRT	0x200		/* Seek Started Status */
#define	INVDADR		0x400		/* Invalid Disk Address Error */
#define	DNEMEM		0x800		/* Non-Existant Memory Error */
#define	PARERR		0x1000		/* Memory Parity Error */
#define	DCOMPERR	0x2000		/* Data Compare Error */
#define	DDIRDY		0x4000		/* DDI Ready Error/Status */
#define	OPABRT		0x8000		/* Operator Abort (Host) Error/Status */
#define	DSERLY		0x10000		/* Data Strobe Early */
#define	DSLATE		0x20000		/* Data Strobe Late */
#define	TOPLUS		0x40000		/* Track Offset Plus */
#define	TOMNUS		0x80000		/* Track Offset Minus */
#define	CPDCRT		0x100000	/* Cntlr Performed Data Correction */
#define	HRDERR		0x200000	/* Hard Error */
#define	SFTERR		0x400000	/* Soft Error (retry succesful) */
#define	ANYERR		0x800000	/* Any Error */

#define	ERRBITS	"\20\30\27SOFT\26HARD\25CPDCRT\24TOMNUS\23TOPLUS\22DSLATE\
\21DSERLY\20OPABRT\17DDIRDY\16DCOMPERR\15PARERR\14DNEMEM\13INVADR\12SEEKSTRT\
\11ALTACC\10\DRVNRDY\7NOTCYLERR\6UCDATERR\5DSEEKERR\4SZTIMEOUT\3WPTERR\2HCMPERR\
\1HCRCERR"

/*
 * DCB Status Symbolic Constants
 */
#define	DCBABT		0x10000000	/* DCB Aborted */
#define	DCBUSC		0x20000000	/* DCB Unsuccesfully Completed */
#define	DCBCMP		0x40000000	/* DCB Complete */
#define	DCBSTR		0x80000000	/* DCB Started */

#define	DCBBITS	"\20\40DCBSTR\37DCBCMP\36DCBUSC\35DCBABT"

/*
 * MDCB Status Symbolic Constants
 */
#define	CTLRBSY		0x10000000	/* Cntlr Busy */
#define	INTCCDE		0x60000000	/* Interrupt Cause Code */
#define	DCBINT		0x80000000	/* DCB Interrupt Flag */

#define	MDCBBITS "\20\40DCBINT\37INTCCDE\36CTRLBSY"

/*
**	Hard Error Types
*/

#define	HTYPES	(HCRCERR|HCMPERR|WPTERR|SZTIMEOUT|DSEEKERR|UCDATERR|NOTCYLERR| \
		 DRVNRDY|INVDADR|DNEMEM|PARERR|DCOMPERR)


/*
**	Errors
*/

#define	ERRS	0x3FFF
#define	CANRETRY	(SZTIMEOUT|DSEEKERR|NOTCYLERR|DCOMPERR|UCDATERR| \
			 PARERR|DNEMEM|HCRCERR|HCMPERR)
/*
**	VDDC Interrupt Modes
*/

#define	NOINT	0x0		/* No Interrupt */
#define	INTERR	0x2		/* Interrupt on Error */
#define	INTSUC	0x1		/* Interrupt on Success */
#define	INTDUN	0x3		/* Interrupt on Error or Success */

#define	CMD_MASK 0xFF0		/* Command code mask */
				/* When a tabular approach can be used */
				/* again change this back to 0x1F0 */

#define vdaddr ( (char *)(0xff0000+IOBASE) )

struct	size
{
	daddr_t	nblocks;
	int	block0;
};



#define	VDMF	0x8000		/* Manufacturer Fault 1=good sector */
#define	VDUF	0x4000		/* User Fault 1=good sector */
#define	VDALT	0x2000		/* Alternate Sector 1=alternate */
#define	VDWPT	0x1000		/* Write Protect 1=Read Only Sector */

/*
**	Addr of Memory-Mapped I/O port for VDDC Control Register
*/

#define	VDDC_ADR (char *)(0xFF2000 + IOBASE)	/* device address register */
						/* this is the logical value */
						/* physically @ 0xFF2000 */
						/* location was extracted */
						/* from the VDDC diagnostic */
						/* package */

/*
**	Address of Memory-Mapped I/O Port for VDDC H/W Reset
*/

#define	VDDC_RESET(addr)	*(addr + 4) = 0;	/* reset controller */

/*
**	Start i/o to/from controller.
*/

#define VDDC_ATTENTION(ctrl,mdcbadr)  \
			{ movow(((int)mdcbadr & 0xffff0000)>>16,ctrl) ;\
			  movow( (int)mdcbadr & 0xffff, ctrl+2);\
			}
