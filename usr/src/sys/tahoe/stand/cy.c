/*	cy.c	7.2	86/01/21	*/
/*	cy.c	Tahoe version 	Mar 1983.	*/

/*
 * Cypher tape driver. Stand alone version.
 *
 */
#include "../machine/pte.h"
#include "../machine/mtpr.h"

#include "param.h"
#include "inode.h"
#include "fs.h"

#include "saio.h"
#include "cyvar.h"

#include "../tahoevba/vbaparam.h"

long	cystd[] = { 0xf4000, 0 };
#define	CYADDR(i)	(cystd[i] + (int)VBIOBASE)

struct scp	/* SYSTEM CONFIGURATION POINTER */
{
  char sysbus ;	/* width of system buss 0=8;1=16 */
  char nu1 ;
  char pt_scb[4] ;	/* pointer to ->SYSTEM CONFIGURATION BLOCK */
};

struct scp * SCP;	/* absolute address - jumpered on the controller */
			/* set to 0xC06 for Tahoe */

struct scb	/* SYSTEM CONFIGUREATION BLOCK */
{
  char sysblk[1] ;	/* 0x03 fixed value code */
  char nu2[1] ;
  char pt_ccb[4] ;	/* pointer to ->CHANNEL CONTROL BLOCK */
}scb;

struct ccb	/* CHANNEL CONTROL BLOCK */
{
  char ccw[1] ;		/* 0x11 normal; 0x09 clear non_vect interrupt */
  char gate[1] ;	/* This is "the" GATE */
  char pt_tpb[4] ;	/* pointer to ->TAPE OPERATION BLOCK or MOVE BLOCK */
}ccb;

struct tpb	/* TAPE OPERATIONS PARAMETER BLOCK */
{
  long cmd ;		/* COMMAND (input) */
  char control[2] ;	/* CONTROL (input) */
  short count ;		/* RETURN COUNT (output) */
  short size ;		/* BUFFER SIZE (input/output) */
  short rec_over ;	/* RECORDS/OVERRUN (input/output) */
  char pt_data[4] ;	/* pointer to ->SOURCE/DEST (input) */
  char status[2] ;	/* STATUS (output) */
  char pt_link[4] ;	/* pointer to ->INTERRUPT/PARAMETER BLOCK (input) */
} tpb;

struct tpb cycool	/* tape parameter block to clear interrupts */
= {
	0L,		/* command */
	0,0,		/* control */
	0,		/* count */
	0,		/* size */
	0,		/* rec_over */
	0,0,0,0,	/* pt_data */
	0,0,		/* status */
	0,0,0,0		/* pt_link */
} ;	
int cyblksiz = 1024;	/* foreign tape size as found in open routine */
long cyblock;		/* next block number for i/o */
/*
 * Reset the controller.
 */
cyopen(io)
	register struct iob *io;
{
	register ctlradr;

	ctlradr = CYADDR(0);
	SCP = (struct scp *)0xc06;		/* absolute - for setup */
	TM_RESET(ctlradr,0xff);	/* reset the controller */
	/*
	 * Initialize the system configuration pointer
	 */
	SCP->sysbus = 1;			/* system width = 16 bits. */
	/* initialize the pointer to the system configuration block */
	set_pointer((long)&scb.sysblk[0],(char *)SCP->pt_scb);
	/*
	 * Initialize the system configuration block.
	 */
	scb.sysblk[0] = 0x3;		/* fixed value */
	/* initialize the pointer to the channel control block */
	set_pointer((long)&ccb.ccw[0],(char *)scb.pt_ccb);
	/*
	 * Initialize the channel control block.
	 */
	ccb.ccw[0] = 0x11;		/* normal interrupts */
	/* initialize the pointer to the tape parameter block */
	set_pointer((long)&tpb,(char *)ccb.pt_tpb);
	/*
	 * set the command to be NO_OP.
	 */
	tpb.cmd = NO_OP;
	tpb.control[0] = CW_BL;		/* TPB not used on first attention */
	tpb.control[1] = CW_16bits;
	ccb.gate[0] = GATE_CLOSED;	
	TM_ATTENTION(ctlradr, 0xff);	/* execute! */
	cywait(10*1000);
	/*
	 * set the command to be CONFIGURE.
	 */
	tpb.cmd = CONFIG;
	tpb.control[0] = CW_BL;		/* NO interrupt on completion */
	tpb.control[1] = CW_16bits;
	tpb.status[0] = tpb.status[1] = 0;
	ccb.gate[0] = GATE_CLOSED;	
	TM_ATTENTION(ctlradr, 0xff);	/* execute! */
	cywait(10*1000);
	uncache (&tpb.status[1]) ;
	if (tpb.status[1] & CS_ERm) {
	   printf("Cypher initialization error!\n");
	   cy_decode_error(tpb.status[1]&CS_ERm);
	   _stop("");
	}
	if(cycmd(io,REWD_TA) == -1)
		_stop("Rewind failed!\n");
	while(io->i_boff > 0) {
		if(cycmd(io,SPAC_FM) == -1)
			_stop("cy: seek failure!\n");
		io->i_boff--;
	}
#ifdef NOBLOCK
	if (io->i_flgs & F_READ) {
		if((cyblksiz = cycmd(io,READ_FO)) == -1)
			_stop("Read foriegn tape failed!\n");
		if(cycmd(io,REWD_TA) == -1)
			_stop("Rewind after read failed\n");
	}
#endif
}

/* if tape was open for writing write a file mark */
cyclose(io)
	register struct iob *io;
{
	if (io->i_flgs & F_WRITE) cycmd(io,WRITE_FMARK);
	cycmd(io,REWD_TA);
	cyblock = 0;
}

cystrategy(io,func)
	register struct iob *io;
	register long	func;
{

#ifndef NOBLOCK
	if ((func != SPACE) && (func != REWD_TA) && (io->i_bn != cyblock)) {
		cycmd(io,SPACE);
		tpb.rec_over = 0;
	}
	if(func==READ || func==WRITE) {
		struct iob liob;
		register struct iob *lio = &liob;
		register count;

		liob = *io;
		while(lio->i_cc > 0) {
			if((count = cycmd(lio, func)) == 0)
				return(-1);
			lio->i_cc -= count;
			lio->i_ma += count;
		}
		return(io->i_cc);
	}
#endif
	return(cycmd(io, func));
}

cycmd(io,func)
	register struct iob *io;
	long	func;
{
	register ctlradr;
	short j;

	ctlradr = CYADDR(0);
	cywait(9000); 
	if (func == READ) func = READ_TA;
	else if (func == WRITE) func = WRIT_TA;
	else if (func == WRITE_FMARK) func = WRIT_FM;
	tpb.cmd = func;
	uncache(&ccb.gate[0]);
	while(ccb.gate[0] == GATE_CLOSED)
		uncache(&ccb.gate[0]);
	ccb.gate[0] = GATE_CLOSED;
	tpb.control[0] = CW_BL;
	tpb.control[1] = CW_16bits;
	tpb.status[0] = tpb.status[1] = 0;
	tpb.count = 0;
	set_pointer((long)&tpb,(char *)ccb.pt_tpb);
	switch (func) 
	{
	case READ_TA:
		if (io->i_cc > cyblksiz)
			tpb.size = TM_SHORT(cyblksiz);
		else tpb.size = TM_SHORT(io->i_cc);
		set_pointer((long)io->i_ma,(char *)tpb.pt_data);
		cyblock += 1;
		break;
	case WRIT_TA:
		tpb.size = TM_SHORT(io->i_cc);
		set_pointer((long)io->i_ma,(char *)tpb.pt_data);
		cyblock += 1;
		break;
	case SPACE:
		if ((j = io->i_bn - cyblock) < 0) {
			j = -j;
			tpb.control[1] |= CW_R;
			cyblock -= j;
		}
		else
			cyblock += j;
		tpb.rec_over = TM_SHORT(j);
		break;
	case REWD_TA:
		cyblock = 0;
		break;
	}
	TM_ATTENTION(ctlradr, 0xff);	/* execute! */
	if (func == REWD_TA || func == SPACE) {
		cywait(60*5*1000);
	}
	else cywait(10*1000);
	/*
	 * First we clear the interrupt and close the gate.
	 */
	mtpr(PADC, 0);
	ccb.gate[0] = GATE_CLOSED;
	set_pointer((int)&cycool,(char *)ccb.pt_tpb);
	cycool.cmd = NO_OP;	/* no operation */
	cycool.control[0] = CW_BL;	/* No INTERRUPTS */
	cycool.control[1] = 0;
	TM_ATTENTION(ctlradr,0xff);	/* cool it ! */
	cywait(20000); 
	uncache (&tpb.status[1] ) ; 
	if (tpb.status[1] & CS_ERm) {
		cy_decode_error(tpb.status[1]&CS_ERm);
		return -1;
	}
	uncache (&tpb.count);
	return((long)TM_SHORT(tpb.count));
}
	


cyprint_error(message)
register char	*message;
{
	printf("cy0: %s.\n", message);
}

/*
*/

cy_decode_error(status)
register int	status;
{
	switch(status) {
	case ER_TO1:
	case ER_TO2:
	case ER_TO3:
	case ER_TO4:
	case ER_TO5:
		cyprint_error("Drive timed out during transfer");
		break;
	case ER_TO6:	
		cyprint_error("Non-existant system memory reference");
		break;
	case ER_DIAG:
	case ER_JUMP:
		cyprint_error("Controller micro diagnostics failed");
		break;
	case ER_HARD:
		cyprint_error("Unrecoverble media error");
		break;
	case ER_TOF:
		if (tpb.cmd == WRIT_TA)
			cyprint_error("Unsatisfactory media");
		break;
	case ER_FIFO:
		cyprint_error("Data transfer over run");
		break;
	case ER_TRN:
		cyprint_error("Drive is not ready");
		break;
	case ER_PRO:
		cyprint_error("Tape is write protected");
		break;
	case ER_PSUM:
		cyprint_error("Checksum error in controller proms");
		break;
	case ER_PARI:
		cyprint_error("Unrecoverable tape parity error");
		break;
	case ER_BLAN:
		cyprint_error("Blank tape found where data was expected");
		break;
	case ER_ER:
		cyprint_error("Unrecoverble hardware error");
	default:
		break;
	}
}


long 
cywait(timeout)
	long timeout;
{
	long dummy;

	uncache (&ccb.gate[0]) ;
	while (ccb.gate[0] != GATE_OPEN) {
		uncache (&ccb.gate[0]) ;
		DELAY(1000);
		if (--timeout == 0) {
			cyprint_error("Transfer timeout");
			_stop("");
		}
	}
}

/*
 *  Set a TAPEMASTER pointer (first parameter), into the
 *  4 bytes array pointed by the second parameter.
 */
set_pointer(pointer,dest)
long pointer;
char * dest;
{
	*dest++ = pointer & 0xff;		/* low byte - offset */
	*dest++ = (pointer >> 8) & 0xff;	/* high byte - offset */
	*dest++ = 0; 
	*dest   = (pointer & 0xf0000) >> 12;	/* base */
}
