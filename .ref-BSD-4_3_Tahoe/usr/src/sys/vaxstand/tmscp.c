
/*	@(#)tmscp.c	7.3 (Berkeley) 3/4/88 */

/****************************************************************
 *                                                              *
 *        Licensed from Digital Equipment Corporation           *
 *                       Copyright (c)                          *
 *               Digital Equipment Corporation                  *
 *                   Maynard, Massachusetts                     *
 *                         1985, 1986                           *
 *                    All rights reserved.                      *
 *                                                              *
 *        The Information in this software is subject to change *
 *   without notice and should not be construed as a commitment *
 *   by  Digital  Equipment  Corporation.   Digital   makes  no *
 *   representations about the suitability of this software for *
 *   any purpose.  It is supplied "As Is" without expressed  or *
 *   implied  warranty.                                         *
 *                                                              *
 *        If the Regents of the University of California or its *
 *   licensees modify the software in a manner creating         *
 *   diriviative copyright rights, appropriate copyright        *
 *   legends may be placed on  the drivative work in addition   *
 *   to that set forth above.                                   *
 ***************************************************************/
/*
 * tmscp.c - TMSCP (TK50/TU81) standalone driver
 */
 
# ifndef lint
static char *sccsid = "@(#)tmscp.c	1.5	(ULTRIX)	4/18/86";
# endif not lint
 
/* ------------------------------------------------------------------------
 * Modification History: /sys/stand/tmscp.c
 *
 * 3-15-85  afd
 *	Don't ask for an interrupt when commands are issued and
 *	check ownership bit in the response descriptor to detect when a
 *	command is complete.  Necessary due to the TU81's failure to set
 *	the response interrupt field in the communications area.
 *
 * ------------------------------------------------------------------------
 */
 
#include "param.h"
#include "inode.h"
#include "fs.h"

#include "../vax/pte.h"

#include "savax.h"
#include "saio.h"
 
/*
 * Parameters for the communications area
 * (Only 1 cmd & 1 rsp packet)
 */
#define	NRSPL2	0
#define	NCMDL2	0
#define	NRSP	(1<<NRSPL2)
#define	NCMD	(1<<NCMDL2)
 
#include "../vaxuba/tmscpreg.h"
#include "../vaxuba/ubareg.h"
#include "../vax/tmscp.h"
 
#define	MAXCTLR		1		/* all addresses must be specified */
u_short tmscpstd[MAXCTLR] = { 0174500 };
 
struct iob	ctmscpbuf;
 
struct tmscpdevice *tmscpaddr = 0;
 
struct tmscp {
	struct tmscpca	tmscp_ca;
	struct mscp	tmscp_rsp;
	struct mscp	tmscp_cmd;
} tmscp;
 
struct tmscp *tmscp_ubaddr;		/* Unibus address of tmscp structure */
 
struct mscp *tmscpcmd();
 
int tmscp_offline = 1;		/* Flag to prevent multiple STCON */
int tms_offline[4] = {1,1,1,1}; /* Flag to prevent multiple ONLIN */
 
/*
 * Open a tmscp device. Initialize the controller and set the unit online.
 */
tmscpopen(io)
	register struct iob *io;
{
	register struct mscp *mp;
	int i;
 
	if ((u_int)io->i_ctlr >= MAXCTLR)
		return (ECTLR);
	/*
	 * Have the tmscp controller characteristics already been set up
	 * (STCON)?
	 */
	if (tmscp_offline) {
		if (tmscpaddr == 0)
			tmscpaddr = (struct tmscpdevice *)ubamem(io->i_adapt, tmscpstd[0]);
		if (tmscp_ubaddr == 0) {
			ctmscpbuf.i_unit = io->i_unit;
			ctmscpbuf.i_ma = (caddr_t)&tmscp;
			ctmscpbuf.i_cc = sizeof(tmscp);
			tmscp_ubaddr = (struct tmscp *)ubasetup(&ctmscpbuf, 2);
		}
		/*
		 * Initialize the tmscp device and wait for the 4 steps
		 * to complete.
		 */
		tmscpaddr->tmscpip = 0;
		while ((tmscpaddr->tmscpsa & TMSCP_STEP1) == 0);
		tmscpaddr->tmscpsa =TMSCP_ERR|(NCMDL2<<11)|(NRSPL2<<8);
 
		while ((tmscpaddr->tmscpsa & TMSCP_STEP2) == 0);
#		define STEP1MASK 0174377
#		define STEP1GOOD (TMSCP_STEP2|TMSCP_IE|(NCMDL2<<3)|NRSPL2)
		if ((tmscpaddr->tmscpsa&STEP1MASK) != STEP1GOOD)
			printf("tmscpopen: step 1 not successful sa=%o\n",tmscpaddr->tmscpsa&0xffff);
		tmscpaddr->tmscpsa = (short)&tmscp_ubaddr->tmscp_ca.ca_ringbase;
 
		while ((tmscpaddr->tmscpsa & TMSCP_STEP3) == 0);
#		define STEP2MASK 0174377
#		define STEP2GOOD (TMSCP_STEP3)
		if ((tmscpaddr->tmscpsa&STEP2MASK) != STEP2GOOD)
			printf("tmscpopen: step 2 not successful sa=%o\n",tmscpaddr->tmscpsa&0xffff);
		tmscpaddr->tmscpsa = (short)(((int)&tmscp_ubaddr->tmscp_ca.ca_ringbase) >> 16);
 
		while ((tmscpaddr->tmscpsa & TMSCP_STEP4) == 0);
#		define STEP3MASK 0174000
#		define STEP3GOOD TMSCP_STEP4
		if ((tmscpaddr->tmscpsa&STEP3MASK) != STEP3GOOD)
			printf("tmscpopen: step 3 not successful sa=%o\n",tmscpaddr->tmscpsa&0xffff);
		tmscpaddr->tmscpsa = TMSCP_GO;
 
		/*
		 * Init cmd & rsp area
		 */
		tmscp.tmscp_ca.ca_cmddsc[0] = (long)&tmscp_ubaddr->tmscp_cmd.mscp_cmdref;
		tmscp.tmscp_cmd.mscp_dscptr = tmscp.tmscp_ca.ca_cmddsc;
		tmscp.tmscp_cmd.mscp_header.tmscp_vcid = 1;	/* for tape */
 
		tmscp.tmscp_ca.ca_rspdsc[0] = (long)&tmscp_ubaddr->tmscp_rsp.mscp_cmdref;
		tmscp.tmscp_rsp.mscp_dscptr = tmscp.tmscp_ca.ca_rspdsc;
		tmscp.tmscp_cmd.mscp_cntflgs = 0;
		if (tmscpcmd(M_OP_STCON, 0) == 0) {
			printf("tms: open error, STCON\n");
			return (EIO);
		}
		tmscp_offline = 0;
	}
	tmscp.tmscp_cmd.mscp_unit = io->i_unit&03;
	/* 
	 * Has this unit been issued an ONLIN?
	 */
	if (tms_offline[tmscp.tmscp_cmd.mscp_unit]) {
		if ((mp = tmscpcmd(M_OP_ONLIN, 0)) == 0) {
			_stop("tms: open error, ONLIN\n");
			return (EIO);
		}
		tms_offline[tmscp.tmscp_cmd.mscp_unit] = 0;
	}
	/*
	 * This makes no sense, but I could be wrong... KB
	 *
	 *	if ((u_int)io->i_part > 3)
	 *		return (EPART);
	 */
	if (io->i_part) {
		/*
		 * Skip forward the appropriate number of files on the tape.
		 */
		tmscp.tmscp_cmd.mscp_tmkcnt = io->i_part;
		(void)tmscpcmd(M_OP_REPOS, 0);
		tmscp.tmscp_cmd.mscp_tmkcnt = 0;
	}
	return (0);
}
 
/*
 * Close the device (rewind it to BOT)
 */
tmscpclose(io)
	register struct iob *io;
{
	(void)tmscpcmd(M_OP_REPOS, M_MD_REWND);
}
 
/*
 * Set up tmscp command packet.  Cause the controller to poll to pick up
 * the command.
 */
struct mscp *
tmscpcmd(op,mod)
	int op, mod;			/* opcode and modifier (usu 0) */
{
	struct mscp *mp;		/* ptr to cmd packet */
	int i;				/* read into to init polling */
 
	tmscp.tmscp_cmd.mscp_opcode = op;
	tmscp.tmscp_cmd.mscp_modifier = mod;
	tmscp.tmscp_cmd.mscp_header.tmscp_msglen = mscp_msglen;
	tmscp.tmscp_ca.ca_cmddsc[0] |= TMSCP_OWN;	/* | TMSCP_INT */
	tmscp.tmscp_rsp.mscp_header.tmscp_msglen = mscp_msglen;
	tmscp.tmscp_ca.ca_rspdsc[0] |= TMSCP_OWN;	/* | TMSCP_INT */
 
	i = tmscpaddr->tmscpip;
	for (;;) {
		if (tmscpaddr->tmscpsa & TMSCP_ERR) {
			printf("tmscpcmd: Fatal error sa=%o\n", tmscpaddr->tmscpsa & 0xffff);
			return(0);
		}
 
		if (tmscp.tmscp_ca.ca_cmdint)
			tmscp.tmscp_ca.ca_cmdint = 0;
		/*
		 * This is to handle the case of devices not setting the
		 * interrupt field in the communications area. Some
		 * devices (early TU81's) only clear the ownership field
		 * in the Response Descriptor.
		 *
		 *
		 *	if (tmscp.tmscp_ca.ca_rspint)
		 *		break;
		 */
		if (!(tmscp.tmscp_ca.ca_rspdsc[0] & (TMSCP_OWN)))
			break;
	}
	tmscp.tmscp_ca.ca_rspint = 0;
	mp = &tmscp.tmscp_rsp;
	if (mp->mscp_opcode != (op|M_OP_END) ||
	   (mp->mscp_status&M_ST_MASK) != M_ST_SUCC) {
		/*
		 * Detect hitting tape mark.  This signifies the end of the
		 * tape mini-root file.  We don't want to return an error
		 * condition to the strategy routine.
		 */
		if ((mp->mscp_status & M_ST_MASK) != M_ST_TAPEM)
			return(0);
	}
	return(mp);
}
 
/*
 * Set up to do reads and writes; call tmscpcmd to issue the cmd.
 */
tmscpstrategy(io, func)
	register struct iob *io;
	int func;
{
	register struct mscp *mp;
	int ubinfo;
 
	ubinfo = ubasetup(io, 1);
	mp = &tmscp.tmscp_cmd;
	mp->mscp_lbn = io->i_bn;
	mp->mscp_unit = io->i_unit&03;
	mp->mscp_bytecnt = io->i_cc;
	mp->mscp_buffer = (ubinfo & 0x3fffff) | (((ubinfo>>28)&0xf)<<24);
	if ((mp = tmscpcmd(func == READ ? M_OP_READ : M_OP_WRITE, 0)) == 0) {
		ubafree(io, ubinfo);
		printf("tms: I/O error\n");
		return(-1);
	}
	ubafree(io, ubinfo);
	/*
	 * Detect hitting tape mark so we do it gracefully and return a
	 * character count of 0 to signify end of copy.  Rewind the tape
	 * before returning.
	 */
	if ((mp->mscp_status & M_ST_MASK) == M_ST_TAPEM)
		return(0);
	return(io->i_cc);
}
