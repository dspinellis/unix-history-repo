#ifndef lint
static char sccsid[] = "@(#)io.c	1.4 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"
#include	"cmd.h"


/*
**
*/

static cmd_text_element	nul_table[] = {
	{ 0,	"",	"" }
};

int wait_for_char;
int vdtimeout;
char	*clean_up = "Cleaning up...  Please wait.\n";


/*
**
*/

poll(wait)
int	wait;
{
	int	tokens[10];
	int	didmsg = 0;

	wait_for_char = 0;
	vdtimeout = wait*1000*1000;
	uncache(&(dcb.operrsta));
	while (!((dcb.operrsta) & (DCBS_DONE | DCBS_ABORT))) {
		if (input()) {
			get_text_cmd(nul_table, tokens);
			if (didmsg == 0 && kill_processes == true) {
				didmsg = 1;
				indent();
				print(clean_up);
				exdent(1);
			}
		}
		vdtimeout--;
		uncache(&(dcb.operrsta));
		if (vdtimeout <= 0) {
			if(C_INFO.type == VDTYPE_VDDC)
				printf("\nVDDC");
			else
				printf("\nSMD-E");
			printf(": Controller timeout");
			VDABORT(C_INFO.addr, C_INFO.type);
			DELAY(30000);
			break;
		}
	}
	if((vdtimeout > 0)) {
		if(C_INFO.type == VDTYPE_SMDE) {
			uncache(&(C_INFO.addr->vdcsr));
			while(C_INFO.addr->vdcsr & CS_GO) {
				DELAY(50);
				uncache(&(C_INFO.addr->vdcsr));
			}
		}
		DELAY(500);
	}
	if((dcb.opcode == VDOP_RD) || (dcb.opcode == VDOP_RDRAW))
		mtpr(PADC, 0);
	uncache(&(dcb.operrsta));
	uncache(&(dcb.err_code));
	wait_for_char = 1;
}


/*
**	Access_with_no_trailer is used to perform controller functions which
** require no data movement.
*/

access_with_no_trailer(function, wait_time)
int	function, wait_time;
{
	dcb.opcode = function;		/* command */
	dcb.intflg = DCBINT_NONE;
	dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = (function == VDOP_START) ? 0 : (char)cur.drive;
	dcb.trailcnt = (char)0;
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	VDGO(C_INFO.addr, (u_long)&mdcb, C_INFO.type);	
	poll(wait_time);
	if(vdtimeout <= 0) {
		printf(" during startup operation.\n");
		_longjmp(abort_environ, 1);
	}
	return dcb.operrsta;
}


/*
**	access_dsk is used by other routines to do reads and writes to the disk.
** The status of the read / write is returned to the caller for processing.
*/

access_dsk(buf, dskaddr, func, count, wait)
char	*buf;
dskadr	*dskaddr;
int	func, count, wait;
{
	cur.daddr.cylinder = dskaddr->cylinder;
	cur.daddr.track = dskaddr->track;
	wait_for_char = 0;
	dcb.opcode = func;		/* format sector command */
	dcb.intflg = DCBINT_NONE;
	dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = (char)cur.drive;
	if(func == VDOP_SEEK) {
		dcb.trailcnt = (char)(sizeof(struct trseek) / sizeof(long));
		dcb.trail.sktrail.skaddr.cylinder = dskaddr->cylinder;
		dcb.trail.sktrail.skaddr.track = dskaddr->track;
		dcb.trail.sktrail.skaddr.sector = dskaddr->sector;
	}
	else {
		dcb.trailcnt = (char)(sizeof(struct trrw) / sizeof(long));
		dcb.trail.rwtrail.memadr = (u_long)buf; 
		dcb.trail.rwtrail.wcount=count*(SECSIZ/sizeof(short));
		dcb.trail.rwtrail.disk.cylinder = dskaddr->cylinder;
		dcb.trail.rwtrail.disk.track = dskaddr->track;
		dcb.trail.rwtrail.disk.sector = dskaddr->sector;
	}
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	VDGO(C_INFO.addr, (u_long)&mdcb, C_INFO.type);
	if(wait) {
		poll(2*60);
		if(vdtimeout <= 0) {
			printf(" in access_dsk.\n");
			_longjmp(abort_environ, 1);
		}
	}
	wait_for_char = 1;
	return dcb.operrsta;
}


/*
**	Spin_up_drive starts the drives on a controller and waits around for
** the drive to spin up if it is not already spinning.
*/

spin_up_drive()
{
	VDRESET(C_INFO.addr, C_INFO.type);
	if(C_INFO.type == VDTYPE_SMDE) {
		C_INFO.addr->vdcsr =  0;
		C_INFO.addr->vdtcf_mdcb =  AM_ENPDA;
		C_INFO.addr->vdtcf_dcb =  AM_ENPDA;
		C_INFO.addr->vdtcf_trail =  AM_ENPDA;
		C_INFO.addr->vdtcf_data =  AM_ENPDA;
		C_INFO.addr->vdccf = CCF_SEN | 0x8 | CCF_STS |
		    XMD_32BIT | BSZ_16WRD | CCF_ERR |
		    CCF_ENP | CCF_EPE | CCF_EDE | CCF_ECE;
	}
	access_with_no_trailer(VDOP_INIT, 10);
	access_with_no_trailer(VDOP_DIAG, 20);
	configure_drive(0);
}

/*
**	Configure_drive tells the controller what kind of drive is attached
** on a particular line.
*/

configure_drive(pass)
int	pass;
{
	dcb.opcode = VDOP_CONFIG;		/* command */
	dcb.intflg = DCBINT_NONE;
	dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = (char)cur.drive;
	dcb.trail.rstrail.ncyl = CURRENT->vc_ncyl;
	dcb.trail.rstrail.nsurfaces = CURRENT->vc_ntrak;
	if(C_INFO.type == VDTYPE_VDDC)
		dcb.trailcnt = (char)2;
	else {
		dcb.trailcnt = (char)4;
		dcb.trail.rstrail.nsectors = CURRENT->vc_nsec;
		dcb.trail.rstrail.slip_sec = CURRENT->vc_nslip;
		dcb.trail.rstrail.recovery = 0x00;
		C_INFO.addr->vdcylskew = (*C_INFO.cylinder_skew)();
		C_INFO.addr->vdtrackskew = (*C_INFO.track_skew)();
	}
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	VDGO(C_INFO.addr, (u_long)&mdcb, C_INFO.type);
	poll(5);
	if(vdtimeout <= 0) {
		printf(" during drive configuration.\n");
		_longjmp(abort_environ, 1);
	}
	if(dcb.operrsta & (DCBS_OCYL | DCBS_NRDY)) {
		if(pass) {
			printf("\nDrive failed to start!\n\n");
			_longjmp(abort_environ, -1);
		}
		printf("\ndrive not ready, attempting to spin up...");
		access_with_no_trailer(VDOP_START, (cur.drive * 6) + 62);
		DELAY((cur.drive * 5500000) + 62000000);
		printf(" retrying drive configuration\n");
		configure_drive(1);
	}
}


/*
** 	data_ok checks an error status word for bit patterns
**  associated with error conditions from the VDDC controller.  If a hardware
**  error is present then the problem is reported on the console and the program
**  is halted.  If a data error is present the a zero is returned.
**  If everything is OK then a 1 is returned.
*/

data_ok()
{
	register int	status = dcb.operrsta;

	if(status & HARD_ERROR){
		if(status & DCBS_NRDY)
			printf("\nDrive is not ready!");
		else if(status & DCBS_IVA)
			printf("\nInvalid disk address issued!");
		else if(status & DCBS_NEM)
			printf("\nNon-existent memory error!");
		else if(status & DCBS_DPE)
			printf("\nMain memory parity error!");
		else if(status & DCBS_OAB) 
			printf("\nCPU aborted operation!");
		else if(status & DCBS_WPT)
			printf("\nDrive is write protected!");
		else if(status & DCBS_SKI)
			printf("\nDisk seek error!");
		else if(status & DCBS_CHE)
			printf("\nController hardware error!");
		else
			printf("\nNot on cylinder error!");
		printf("   Status = 0x%lx", status);
		if(C_INFO.type == VDTYPE_SMDE)
			printf("  Error code =  0x%x", dcb.err_code & 0xff);
		printf("\n");
		printf("cylinder = %d, track = %d,", dcb.err_cyl, dcb.err_trk);
		printf(" sector = %d, op = 0x%x\n", dcb.err_sec, dcb.opcode);
		reset_controller();
		dcb.operrsta &= HEADER_ERROR;
	}
	return (int)(!(status & (DATA_ERROR | HEADER_ERROR)));
}


/*
**
*/

reset_controller()
{
	printf("Resetting controller.  Please wait...\n");
	spin_up_drive();
	printf("Controller was reset successfully.\n");
}

/*
**
*/

static	int	indent_count;


/*
**
*/

indent()
{
	indent_count += 2;
}


/*
**
*/

exdent(count)
int	count;
{
	if(count == -1)
		indent_count = 0;
	else
		indent_count -= count * 2;
	if(indent_count < 0)
			indent_count = 0;
}


/*
**
*/
/*VARARGS1*/
print(par0, par1, par2, par3, par4, par5, par6)
char	*par0, *par1, *par2, *par3, *par4, *par5, *par6;
{
	register int	count = indent_count;

	while(count--)
		printf(" ");
	printf(par0, par1, par2, par3, par4, par5, par6);
	DELAY((strlen(par0) + 20) * 9000);
}
