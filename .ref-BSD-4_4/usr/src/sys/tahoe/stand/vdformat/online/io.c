#ifndef lint
static char sccsid[] = "%W% (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"
#include	"cmd.h"


/*
**
*/

static cmd_text_element	nul_table[] = {
	{ 0,	"",	"" }
};

int	wait_for_char = 1;		/* synchronous by default */
int vdtimeout;
char	*clean_up = "Cleaning up...  Please wait.\n";


vread(sn, buf, seccnt)
int sn, seccnt;
char *buf;
{
	return (vrdwr(sn, buf, seccnt, VDOP_RD));
}

vwrite(sn, buf, seccnt)
int sn, seccnt;
char *buf;
{
	return (vrdwr(sn, buf, seccnt, VDOP_WD));
}

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
	struct format_op fop;

	cur.daddr.cylinder = dskaddr->cylinder;
	cur.daddr.track = dskaddr->track;
	wait_for_char = 0;

	bzero((char *)&fop, sizeof(fop));
	fop.df_buf = buf;
	fop.df_count = count * lab->d_secsize;
	fop.df_startblk = ((dskaddr->cylinder & 0xfff) * lab->d_ntracks +
	    dskaddr->track) * lab->d_nsectors + dskaddr->sector;
	fop.df_reg[0] = func;
	(void) ioctl(diskfd, DIOCWFORMAT, &fop);
	wait_for_char = 1;
	dcb.operrsta = fop.df_reg[0];		/* XXX */
	dcb.err_code = fop.df_reg[1];		/* XXX */
	return (fop.df_reg[0]);
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
		dcb.operrsta &= HEADER_ERROR;
	}
	return (int)(!(status & (DATA_ERROR | HEADER_ERROR)));
}

vd_error(s)
	char *s;
{
	register int	status = dcb.operrsta;

	print("%s error at sector %d (cyl %d trk %d sect %d),\n",
	    to_sector(cur.daddr), dcb.err_cyl, dcb.err_trk, dcb.err_sec);
	print("  status=%b", dcb.operrsta, VDERRBITS);
	if (C_INFO->type == VDTYPE_SMDE)
		printf(", ecode=0x%x", dcb.err_code);
	printf("\n");
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
/*	DELAY((strlen(par0) + 20) * 9000); */
}
