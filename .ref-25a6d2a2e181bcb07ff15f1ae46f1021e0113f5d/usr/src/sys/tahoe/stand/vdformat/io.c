#ifndef lint
static char sccsid[] = "@(#)io.c	1.7 (Berkeley/CCI) %G%";
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
	register struct vddevice *addr = C_INFO->addr;
	int	tokens[10];

	if (kill_processes == false)
		wait_for_char = 0;
	vdtimeout = wait*1000;
	for (;;) {
		uncache(&(dcb.operrsta));
		if (dcb.operrsta & (DCBS_DONE | DCBS_ABORT))
			break;
		if (kill_processes == false && input()) {
			get_text_cmd(nul_table, tokens);
			if (kill_processes == true) {
				indent();
				print(clean_up);
				exdent(1);
			}
		}
		if (vdtimeout-- <= 0) {
			if(C_INFO->type == VDTYPE_VDDC)
				printf("\nVDDC");
			else
				printf("\nSMD-E");
			printf(": Controller timeout");
abort:
			VDABORT(addr, C_INFO->type);
			DELAY(30000);
			break;
		}
		DELAY(1000);
	}
	if ((vdtimeout > 0)) {
		if (C_INFO->type == VDTYPE_SMDE) {
			for (;;) {
				uncache(&(addr->vdcsr));
				if ((addr->vdcsr & CS_GO) == 0)
					break;
				DELAY(1000);
				if (vdtimeout-- <= 0) {
					printf("\nSMD-E timed out clearing GO");
					goto abort;
				}
			}
			DELAY(300);
		}
		DELAY(500);
	}
	DELAY(200);
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
	dcb.devselect = (function == VDOP_START) ? 0 :
	    ((char)cur.drive | lab->d_devflags);
	dcb.trailcnt = (char)0;
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	VDGO(C_INFO->addr, (u_long)&mdcb, C_INFO->type);	
	poll(wait_time);
	if(vdtimeout <= 0) {
		printf(" during startup operation.\n");
		_longjmp(abort_environ, 1);
	}
	return dcb.operrsta;
}

vread(sn, buf, seccnt)
int sn, seccnt;
char *buf;
{
	int ret;

	ret = vrdwr(sn, buf, seccnt, VDOP_RD);
	if (ret == 0)
		vd_error("read");
	return (ret);
}

vwrite(sn, buf, seccnt)
int sn, seccnt;
char *buf;
{
	int ret;

	ret = vrdwr(sn, buf, seccnt, VDOP_WD);
	if (ret == 0)
		vd_error("write");
	return (ret);
};

vrdwr(sn, buf, seccnt, op)
int sn, seccnt, op;
char *buf;
{
	dskadr	dskaddr;

	dskaddr.cylinder = sn / lab->d_secpercyl;
	sn %= lab->d_secpercyl;
	dskaddr.track = sn / lab->d_nsectors;
	dskaddr.sector = sn % lab->d_nsectors;
	if (access_dsk(buf, &dskaddr, op, seccnt, 1) & DCBS_HARD)
		return (0);
	return (seccnt);
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
	dcb.devselect = (char)cur.drive | lab->d_devflags;
	if(func == VDOP_SEEK) {
		dcb.trailcnt = (char)(sizeof(struct trseek) / sizeof(long));
		dcb.trail.sktrail.skaddr.cylinder = dskaddr->cylinder;
		dcb.trail.sktrail.skaddr.track = dskaddr->track;
		dcb.trail.sktrail.skaddr.sector = dskaddr->sector;
	} else {
		dcb.trailcnt = (char)(sizeof(struct trrw) / sizeof(long));
		dcb.trail.rwtrail.memadr = (u_long)buf; 
		dcb.trail.rwtrail.wcount=count*(lab->d_secsize/sizeof(short));
		dcb.trail.rwtrail.disk.cylinder = dskaddr->cylinder;
		dcb.trail.rwtrail.disk.track = dskaddr->track;
		dcb.trail.rwtrail.disk.sector = dskaddr->sector;
	}
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	VDGO(C_INFO->addr, (u_long)&mdcb, C_INFO->type);
	if(wait) {
		poll(10);
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
	register struct vddevice *addr = C_INFO->addr;

	VDRESET(addr, C_INFO->type);
	if(C_INFO->type == VDTYPE_SMDE) {
		addr->vdcsr =  0;
		addr->vdtcf_mdcb =  AM_ENPDA;
		addr->vdtcf_dcb =  AM_ENPDA;
		addr->vdtcf_trail =  AM_ENPDA;
		addr->vdtcf_data =  AM_ENPDA;
		addr->vdccf = CCF_SEN | 0x8 | CCF_STS |
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
	register struct vddevice *addr = C_INFO->addr;
	register i;

top:
	dcb.opcode = VDOP_CONFIG;		/* command */
	dcb.intflg = DCBINT_NONE;
	dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	dcb.operrsta = 0;
	dcb.devselect = cur.drive | lab->d_devflags;
	dcb.trail.rstrail.ncyl = lab->d_ncylinders;
	dcb.trail.rstrail.nsurfaces = lab->d_ntracks;
	if(C_INFO->type == VDTYPE_VDDC)
		dcb.trailcnt = (char)2;
	else {
		dcb.trailcnt = sizeof (struct treset)/sizeof (long);
		dcb.trail.rstrail.nsectors = lab->d_nsectors;
		dcb.trail.rstrail.slip_sec = lab->d_sparespertrack;
		dcb.trail.rstrail.recovery = VDRF_NONE;
		addr->vdcylskew = lab->d_cylskew;
		addr->vdtrackskew = lab->d_trackskew;
		addr->vdsecsize = lab->d_secsize/sizeof(short);
	}
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	VDGO(addr, (u_long)&mdcb, C_INFO->type);
	poll(5);
	if(vdtimeout <= 0) {
		printf(" during drive configuration.\n");
		goto bad;
	}
	if(dcb.operrsta & VDERR_HARD) {
		if (C_INFO->type == VDTYPE_SMDE) {
			if (lab->d_devflags == 0) {
				lab->d_devflags = VD_ESDI;
				goto top;
			}
#ifdef notdef
printf("vdstatus %x\n", addr->vdstatus[cur.drive]);
			if ((addr->vdstatus[cur.drive] & STA_US) == 0) {
				printf("Drive not present\n\n");
				goto bad;
			}
#endif
		}
		if ((dcb.operrsta & (DCBS_OCYL|DCBS_NRDY)) == 0) {
			printf("drive config error\n");
			goto bad;
		}
		if(pass) {
			printf("\nDrive failed to start!\n\n");
			goto bad;
		}
		printf("\ndrive not ready, attempting to spin up...");
		access_with_no_trailer(VDOP_START, 62);
		for (i = 0; i < 620; i++) {
			if (C_INFO->type == VDTYPE_SMDE &&
			    addr->vdstatus[cur.drive] & STA_UR)
				break;
			DELAY(100000);
		}
		printf(" retrying drive configuration\n");
		pass++;
		lab->d_devflags = 0;
		goto top;
	}
	D_INFO->alive = u_true;
	return;
bad:
	D_INFO->alive = u_false;
	_longjmp(abort_environ, -1);
}


/*
** 	data_ok checks an error status word for bit patterns
**  associated with error conditions from the VDDC controller.  If a hardware
**  error is present then the problem is reported on the console.
**  If a data error is present the a zero is returned.
**  If everything is OK then a 1 is returned.
*/

data_ok()
{
	register int	status = dcb.operrsta;

	if(status & HARD_ERROR){
		vd_error("data transfer");
		printf("  op = 0x%x\n", dcb.opcode);
		reset_controller();
		dcb.operrsta &= HEADER_ERROR;
	}
	return (int)(!(status & (DATA_ERROR | HEADER_ERROR)));
}

vd_error(s)
	char *s;
{
	register int	status = dcb.operrsta;

	print("%s error at sector %d (cyl %d trk %d sect %d),\n",
	    s, to_sector(cur.daddr), dcb.err_cyl & 0xfff, dcb.err_trk,
	    dcb.err_sec);
	print("  status=%b", dcb.operrsta, VDERRBITS);
	if (C_INFO->type == VDTYPE_SMDE)
		printf(", ecode=0x%x", dcb.err_code);
	printf(".\n");
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
