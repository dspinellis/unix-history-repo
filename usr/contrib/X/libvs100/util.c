/* $Header: util.c,v 10.3 86/02/01 15:47:51 tony Rel $ */
/* util.c		Various utilities
 *
 *	ErrorString	Interprets an error string
 *	SoundBell	Generate audible bell
 *	SetKeyClick	Control key click
 *	SetAutoRepeat	Control auto repeat
 *	SetLockLED	Control Lock LED
 *	QueryShape	Determine shapes
 *	SetVideo	Set video blanking
 *	ResolveColors	does nothing
 *	StoreColors	does nothing
 *
 */

/****************************************************************************
 *									    *
 *  Copyright (c) 1983, 1984 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished on an as-is basis and may be used and copied *
 *  only with inclusion of the above copyright notice. This software or any *
 *  other copies thereof may be provided or otherwise made available to     *
 *  others only for non-commercial purposes.  No title to or ownership of   *
 *  the software is hereby transferred.					    *
 * 									    *
 *  The information in this software is  subject to change without notice   *
 *  and  should  not  be  construed as  a commitment by DIGITAL EQUIPMENT   *
 *  CORPORATION.							    *
 * 									    *
 *  DIGITAL assumes no responsibility for the use  or  reliability of its   *
 *  software on equipment which is not supplied by DIGITAL.		    *
 * 									    *
 *									    *
 ****************************************************************************/

#include "vs100.h"
#include "reason.h"

extern int sys_nerr;
extern char *sys_errlist[];

char *AllocateSpace();

struct _ErrEntry {
	int code;
	char *reason;
} errorList[NUM_ERR_CODES] = {
	{INT_ID		, INT_ID_MSG},
	{INT_CD		, INT_CD_MSG},
	{INT_SE		, INT_SE_MSG},
	{INT_BE		, INT_BE_MSG},
	{INT_CM		, INT_CM_MSG},
	{INT_TM		, INT_TM_MSG},
	{INT_MM		, INT_MM_MSG},
	{INT_PD		, INT_PD_MSG},
	{ERR_NYI	, ERR_NYI_MSG},
	{ERR_IFC	, ERR_IFC_MSG},
	{ERR_ICC	, ERR_ICC_MSG},
	{ERR_RN		, ERR_RN_MSG},
	{ERR_RO		, ERR_RO_MSG},
	{ERR_LD		, ERR_LD_MSG},
	{ERR_BE		, ERR_BE_MSG},
	{ERR_AE		, ERR_AE_MSG},
	{ERR_SI		, ERR_SI_MSG},
	{ERR_II		, ERR_II_MSG},
	{ERR_BN		, ERR_BN_MSG},
	{ERR_BNI	, ERR_BNI_MSG},
	{ERR_KBO	, ERR_KBO_MSG},
	{ERR_TBO	, ERR_TBO_MSG},
	{ERR_BBO	, ERR_BBO_MSG},
	{ERR_ITP	, ERR_ITP_MSG},
	{ERR_ISRCMB	, ERR_ISRCMB_MSG},
	{ERR_ISRCBW	, ERR_ISRCBW_MSG},
	{ERR_ISRCBH	, ERR_ISRCBH_MSG},
	{ERR_ISRCC	, ERR_ISRCC_MSG},
	{ERR_ISRCBD	, ERR_ISRCBD_MSG},
	{ERR_ISRCD	, ERR_ISRCD_MSG},
	{ERR_IMSKMB	, ERR_IMSKMB_MSG},
	{ERR_IMSKBW	, ERR_IMSKBW_MSG},
	{ERR_IMSKBH	, ERR_IMSKBH_MSG},
	{ERR_IMSKBD	, ERR_IMSKBD_MSG},
	{ERR_IDSTMB	, ERR_IDSTMB_MSG},
	{ERR_IDSTBW	, ERR_IDSTBW_MSG},
	{ERR_IDSTBH	, ERR_IDSTBH_MSG},
	{ERR_IDSTBD	, ERR_IDSTBD_MSG},
	{ERR_NOAREA	, ERR_NOAREA_MSG},
	{ERR_IMAPMB	, ERR_IMAPMB_MSG},
	{ERR_IMAPFC	, ERR_IMAPFC_MSG},
	{ERR_ZIMAP	, ERR_ZIMAP_MSG},
	{ERR_ZCIMAP	, ERR_ZCIMAP_MSG},
	{ERR_ICLPMB	, ERR_ICLPMB_MSG},
	{ERR_ICLPRC	, ERR_ICLPRC_MSG},
	{ERR_SMC_ITC	, ERR_SMC_ITC_MSG},
	{ERR_ITC_MULT	, ERR_ITC_MULT_MSG},
	{ERR_ITC_DIV	, ERR_ITC_DIV_MSG},
	{ERR_ICD	, ERR_ICD_MSG},
	{ERR_MO_IBC	, ERR_MO_IBC_MSG},
	{ERR_MO_IOT	, ERR_MO_IOT_MSG},
	{ERR_MO_IDT	, ERR_MO_IDT_MSG},
	{ERR_IPC	, ERR_IPC_MSG},
	{ERR_DC_IPC	, ERR_DC_IPC_MSG},
	{ERR_DC_IPSL	, ERR_DC_IPSL_MSG},
	{ERR_DC_IPSM	, ERR_DC_IPSM_MSG},
	{ERR_DC_ICF	, ERR_DC_ICF_MSG},
	{ERR_DC_IPSP	, ERR_DC_IPSP_MSG},
	{ERR_DC_IPSMB	, ERR_DC_IPSMB_MSG},
	{ERR_DC_IPMMB	, ERR_DC_IPMMB_MSG},
	{ERR_DC_IPSC	, ERR_DC_IPSC_MSG},
	{ERR_DC_ISSRCBW	, ERR_DC_ISSRCBW_MSG},
	{ERR_DC_ISSRCBH	, ERR_DC_ISSRCBH_MSG},
	{ERR_DC_ISSRCBD	, ERR_DC_ISSRCBD_MSG},
	{ERR_DC_ISSRCC	, ERR_DC_ISSRCC_MSG},
	{ERR_DC_IDPM	, ERR_DC_IDPM_MSG},
	{ERR_PT_ICSL	, ERR_PT_ICSL_MSG},
	{ERR_PT_ICSO	, ERR_PT_ICSO_MSG},
	{ERR_PT_ICSP	, ERR_PT_ICSP_MSG},
	{ERR_PT_ITSL	, ERR_PT_ITSL_MSG},
	{ERR_PT_ICI	, ERR_PT_ICI_MSG},
	{ERR_PT_TSE	, ERR_PT_TSE_MSG},
	{ERR_PT_NFP	, ERR_PT_NFP_MSG},
	{ERR_PT_ISRCFW	, ERR_PT_ISRCFW_MSG},
	{ERR_PT_ISRCFH	, ERR_PT_ISRCFH_MSG},
	{ERR_PT_ISRCFD	, ERR_PT_ISRCFD_MSG},
	{ERR_PT_IMSKFW	, ERR_PT_IMSKFW_MSG},
	{ERR_PT_IMSKFH	, ERR_PT_IMSKFH_MSG},
	{ERR_PT_IMSKFD	, ERR_PT_IMSKFD_MSG},
	{ERR_PT_CSMF	, ERR_PT_CSMF_MSG},
	{ERR_FA_ISRCB	, ERR_FA_ISRCB_MSG},
	{ERR_FA_SPIOB	, ERR_FA_SPIOB_MSG},
	{ERR_FA_SO	, ERR_FA_SO_MSG},
	{ERR_FA_IBMMB	, ERR_FA_IBMMB_MSG},
	{ERR_FP_ISRCB	, ERR_FP_ISRCB_MSG},
	{ERR_FP_SO	, ERR_FP_SO_MSG},
	{ERR_FP_IPC	, ERR_FP_IPC_MSG},
	{ERR_FP_ICF	, ERR_FP_ICF_MSG},
	{ERR_68K	, ERR_68K_MSG},
	{ERR_RC		, ERR_RC_MSG},
	{ERR_PR		, ERR_PR_MSG},
	{ERR_CRT	, ERR_CRT_MSG},
	{ERR_TU		, ERR_TU_MSG},
	{ERR_KU		, ERR_KU_MSG},
	{ERR_FOE	, ERR_FOE_MSG},
	{ERR_VTO	, ERR_VTO_MSG},
	{ERR_SB		, ERR_SB_MSG},
	{ERR_BS		, ERR_BS_MSG},
	{ERR_BC		, ERR_BC_MSG},
	{ERR_TTO	, ERR_TTO_MSG},
	{ERR_FOO	, ERR_FOO_MSG},
	{ERR_KTO	, ERR_KTO_MSG},
	{ERR_KST	, ERR_KST_MSG},
	{ERR_LDC_IATRV	, ERR_LDC_IATRV_MSG},
	{ERR_LDC_ICH	, ERR_LDC_ICH_MSG},
	{ERR_LDC_ICW	, ERR_LDC_ICW_MSG},
	{ERR_NOVALCUR	, ERR_NOVALCUR_MSG}
};

/* Interpret a string corresponding to an error code.  This doesn't
 * work very well since the driver can't return enough bits for the
 * code, but we do our best */

char *ErrorString (error)
	int error;
{
	register int i;

	for (i = 0; i < NUM_ERR_CODES; i++) {
	    if (errorList[i].code == error)
		return (errorList[i].reason);
	}
	if (error > 0 && error < sys_nerr)
	    return (sys_errlist[error]);
	return ("Unknown error");
}

SoundBell (volume)
	int volume;
{
	char *buf;

	if ((buf = (char *) AllocateSpace(4)) == NULL)
	    return;
	buf[0] = 3;
	buf[1] = 0x23;
	buf[2] = 0x87 - volume;
	buf[3] = 0xa7;
	SendToPeripheral(buf, 4, VSE_DKB);
}

SetKeyClick (volume)
	int volume;
{
	char *buf;

	if ((buf = (char *) AllocateSpace(3)) == NULL)
	    return;
	if (volume) {
	    buf[0] = 2;
	    buf[1] = 0x1b;
	    buf[2] = 0x88 - volume;
	} else {
	    buf[0] = 1;
	    buf[1] = 0x99;
	    buf[2] = 0x99;
	}
	SendToPeripheral(buf, 3, VSE_DKB);
}

QueryShape (shape, width, height)
	int shape;
	short *width, *height;
{
	switch (shape) {
	case CursorShape:
	    if (*width > 64)
		*width = 64;
	    if (*height > 64)
		*height = 64;
	    break;
	case TileShape:
	    *width = *height = 16;
	    break;
	}
}

SetAutoRepeat (onoff)
	int onoff;
{
	char *buf;

	if ((buf = (char *) AllocateSpace(2)) == NULL)
	    return;
	buf[0] = 1;
	buf[1] = onoff ? 0xe3 : 0xe1;
	SendToPeripheral(buf, 2, VSE_DKB);
}

/*ARGSUSED*/
SetLockLED (onoff)
	int onoff;
{
}

SetVideo (onoff)
	int onoff;
{
	return (onoff - 1);
}

/*ARGSUSED*/
ResolveColor (red, green, blue)
	unsigned short *red, *green, *blue;
{
}

/*ARGSUSED*/
StoreColors (count, entries)
	int count;
	ColorDef *entries;

{
}
