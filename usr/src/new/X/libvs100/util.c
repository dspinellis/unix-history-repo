/* $Header: util.c,v 10.4 86/11/30 16:45:52 jg Rel $ */
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
	{IR_ID		, IR_ID_MSG },
	{IR_CD		, IR_CD_MSG },
	{IR_DCI		, IR_DCI_MSG },
	{IR_DE		, IR_DE_MSG },
	{IR_CM		, IR_CM_MSG },
	{IR_DPM		, IR_DPM_MSG },
	{IR_MM		, IR_MM_MSG },
	{IR_PD		, IR_PD_MSG },
	{IR_AB_ACK	, IR_AB_ACK_MSG },
	{IR_STAT_ACK	, IR_STAT_ACK_MSG },
	{IR_SAVE_ACK	, IR_SAVE_ACK_MSG },
	{ERR_NYI	, ERR_NYI_MSG },
	{ERR_IFC	, ERR_IFC_MSG},
	{ERR_ICC	, ERR_ICC_MSG},
	{ERR_BE_NXM_68K	, ERR_BE_NXM_68K_MSG},
	{ERR_BE_RO	, ERR_BE_RO_MSG},
	{ERR_BE_LD	, ERR_BE_LD_MSG},
	{ERR_BE_S	, ERR_BE_S_MSG},
	{ERR_AE		, ERR_AE_MSG},
	{ERR_ZD		, ERR_ZD_MSG},
	{ERR_II		, ERR_II_MSG},
	{ERR_NXM_BBA	, ERR_NXM_BBA_MSG},
	{ERR_BNI	, ERR_BNI_MSG},
	{ERR_KCQO	, ERR_KCQO_MSG},
	{ERR_DPCQO	, ERR_DPCQO_MSG},
	{ERR_MEQO	, ERR_MEQO_MSG},
	{ERR_DPEQO	, ERR_DPEQO_MSG},
	{ERR_AB_BBA	, ERR_AB_BBA_MSG},
	{ERR_KEQO	, ERR_KEQO_MSG},
	{ERR_KOR	, ERR_KOR_MSG},
	{ERR_DPOR	, ERR_DPOR_MSG},
	{ERR_KFE	, ERR_KFE_MSG},
	{ERR_DPFE	, ERR_DPFE_MSG},

	{ERR_ISRCM	, ERR_ISRCM_MSG},
	{ERR_ISRCBW	, ERR_ISRCBW_MSG},
	{ERR_ISRCBH	, ERR_ISRCBH_MSG},
	{ERR_ISRCC	, ERR_ISRCC_MSG},
	{ERR_ISRCBD	, ERR_ISRCBD_MSG},
	{ERR_IMSKM	, ERR_IMSKM_MSG},
	{ERR_IMSKBW	, ERR_IMSKBW_MSG},
	{ERR_IMSKBH	, ERR_IMSKBH_MSG},
	{ERR_IMSKBD	, ERR_IMSKBD_MSG},
	{ERR_IDSTM	, ERR_IDSTM_MSG},
	{ERR_IDSTBW	, ERR_IDSTBW_MSG},
	{ERR_IDSTBH	, ERR_IDSTBH_MSG},
	{ERR_IDSTBD	, ERR_IDSTBD_MSG},
	{ERR_IMAPM	, ERR_IMAPM_MSG},
	{ERR_ICLPM	, ERR_ICLPM_MSG},
	{ERR_ICLPC	, ERR_ICLPC_MSG},

	{ERR_SMC_ITC	, ERR_SMC_ITC_MSG},
	{ERR_SCL_CD	, ERR_SCL_CD_MSG},
	{ERR_AC_ICD	, ERR_AC_ICD_MSG},

	{ERR_MO_IBC	, ERR_MO_IBC_MSG},
	{ERR_MO_IOT	, ERR_MO_IOT_MSG},
	{ERR_MO_IDT	, ERR_MO_IDT_MSG},

	{ERR_IPC	, ERR_IPC_MSG},

	{ERR_DC_IPL	, ERR_DC_IPL_MSG},
	{ERR_DC_IPM	, ERR_DC_IPM_MSG},
	{ERR_DC_ICF	, ERR_DC_ICF_MSG},
	{ERR_DC_IPP	, ERR_DC_IPP_MSG},
	{ERR_DC_IPSM	, ERR_DC_IPSM_MSG},
	{ERR_DC_IPMM	, ERR_DC_IPMM_MSG},
	{ERR_DC_IPC	, ERR_DC_IPC_MSG},
	{ERR_DC_ISSRCBW	, ERR_DC_ISSRCBW_MSG},
	{ERR_DC_ISSRCBH	, ERR_DC_ISSRCBH_MSG},
	{ERR_DC_ISSRCBD	, ERR_DC_ISSRCBD_MSG},
	{ERR_DC_ISSRCC	, ERR_DC_ISSRCC_MSG},
	{ERR_DC_IDPM	, ERR_DC_IDPM_MSG},
	{ERR_DC_DXO	, ERR_DC_DXO_MSG},
	{ERR_DC_DYO	, ERR_DC_DYO_MSG},
	{ERR_DC_CRSO	, ERR_DC_CRSO_MSG},

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
	{ERR_PT_ILB	, ERR_PT_ILB_MSG},
	{ERR_PT_ILE	, ERR_PT_ILE_MSG},
	{ERR_FA_ISRCB	, ERR_FA_ISRCB_MSG},
	{ERR_FA_SO	, ERR_FA_SO_MSG},
	{ERR_FA_IBMM	, ERR_FA_IBMM_MSG},

	{ERR_FP_ISRCB	, ERR_FP_ISRCB_MSG},
	{ERR_FP_ICF	, ERR_FP_ICF_MSG},
	{ERR_FP_DXO	, ERR_FP_DXO_MSG},
	{ERR_FP_DYO	, ERR_FP_DYO_MSG},

	{ERR_CPU		, ERR_CPU_MSG},
	{ERR_RHB		, ERR_RHB_MSG},
	{ERR_PR		, ERR_PR_MSG},
	{ERR_CRTC	, ERR_CRTC_MSG},
	{ERR_DPE		, ERR_DPE_MSG},
	{ERR_KE		, ERR_KE_MSG},
	{ERR_FOE		, ERR_FOE_MSG},
	{ERR_F0		, ERR_F0_MSG},
	{ERR_BSR		, ERR_BSR_MSG},
	{ERR_BCC		, ERR_BCC_MSG},
	{ERR_DPTO	, ERR_DPTO_MSG},
	{ERR_FOO		, ERR_FOO_MSG},
	{ERR_KTO		, ERR_KTO_MSG},
	{ERR_KST		, ERR_KST_MSG},
	{ERR_VR		, ERR_VR_MSG},
	{ERR_F1		, ERR_F1_MSG},
	{ERR_F2		, ERR_F2_MSG},
	{ERR_F3		, ERR_F3_MSG},
	{ERR_DPP		, ERR_DPP_MSG},
	{ERR_KP		, ERR_KP_MSG},
	{ERR_FOED	, ERR_FOED_MSG},
	{ERR_FOOD	, ERR_FOOD_MSG},
	{ERR_BCTO	, ERR_BCTO_MSG},
	{ERR_RLB		, ERR_RLB_MSG},
	{ERR_KD		, ERR_KD_MSG},
	{ERR_BVC		, ERR_BVC_MSG},

	{ERR_ISRCHW	, ERR_ISRCHW_MSG},
	{ERR_ISRCHH	, ERR_ISRCHH_MSG},
	{ERR_ISRCHD	, ERR_ISRCHD_MSG},
	{ERR_DC_ISSRCHW	, ERR_DC_ISSRCHW_MSG},
	{ERR_DC_ISSRCHH	, ERR_DC_ISSRCHH_MSG},
	{ERR_DC_ISSRCHD	, ERR_DC_ISSRCHD_MSG},

	{ERR_LC_ICTX	, ERR_LC_ICTX_MSG},
	{ERR_LC_ICTY	, ERR_LC_ICTY_MSG},
	{ERR_LC_ICCX	, ERR_LC_ICCX_MSG},
	{ERR_LC_ICCY	, ERR_LC_ICCY_MSG},
	{ERR_LC_ICW	, ERR_LC_ICW_MSG},
	{ERR_LC_ICH	, ERR_LC_ICH_MSG},
	{ERR_PT_DXO	, ERR_PT_DXO_MSG},
	{ERR_PT_CRSO	, ERR_PT_CRSO_MSG},
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
