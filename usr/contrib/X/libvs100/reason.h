/* $Header: reason.h,v 10.3 86/02/01 15:47:29 tony Rel $ */
/* reason.h	Interrupt reason values
 *
 * Author:	Paul J. Asente
 * 		Digital Equipment Corporation
 * 		Western Reseach Lab
 * Date:	June 1983
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

#ifndef VS_REASONS
#define VS_REASONS

#define INT_ID		0x0001
#define INT_ID_MSG	"initialization done"
#define INT_CD		0x0002
#define INT_CD_MSG	"command done"
#define INT_SE		0x0004
#define INT_SE_MSG	"started executing"
#define INT_BE		0x0008
#define INT_BE_MSG	"button event"
#define INT_CM		0x0010
#define INT_CM_MSG	"cursor moved"
#define INT_TM		0x0020
#define INT_TM_MSG	"tablet moved"
#define INT_MM		0x0040
#define INT_MM_MSG	"mouse moved"
#define INT_PD		0x0080
#define INT_PD_MSG	"powerup done"
#define INT_ERR		-128
#define INT_ERR_MSG	"error encountered"

#define ERR_NYI		INT_ERR+0
#define ERR_NYI_MSG	"not yet implemented"
#define ERR_IFC		INT_ERR+1
#define ERR_IFC_MSG	"invalid function code"
#define ERR_ICC		INT_ERR+2
#define ERR_ICC_MSG	"invalid command code"
#define ERR_RN		INT_ERR+3
#define ERR_RN_MSG	"bus error: non-existant memory"
#define ERR_RO		INT_ERR+4
#define ERR_RO_MSG	"bus error: retry overflow"
#define ERR_LD		INT_ERR+5
#define ERR_LD_MSG	"bus error: link down"
#define ERR_BE		INT_ERR+6
#define ERR_BE_MSG	"bus error: unexplained"
#define ERR_AE		INT_ERR+7
#define ERR_AE_MSG	"address error"
#define ERR_SI		INT_ERR+8
#define ERR_SI_MSG	"spurious interrupt"
#define ERR_II		INT_ERR+9
#define ERR_II_MSG	"illegal instruction"
#define ERR_BN		INT_ERR+10
#define ERR_BN_MSG	"bba: non-existant memory"
#define ERR_BNI		INT_ERR+11
#define ERR_BNI_MSG	"bba not installed"
#define ERR_KBO		INT_ERR+12
#define ERR_KBO_MSG	"keyboard buffer overflow"
#define ERR_TBO		INT_ERR+13
#define ERR_TBO_MSG	"tablet buffer overflow"
#define ERR_BBO		INT_ERR+14
#define ERR_BBO_MSG	"button buffer overflow"
#define ERR_ITP		INT_ERR+15
#define ERR_ITP_MSG	"invalid tablet packet"

#define ERR_ISRCMB	INT_ERR+32
#define ERR_ISRCMB_MSG	"invalid src modifier bits"
#define ERR_ISRCBW	INT_ERR+33
#define ERR_ISRCBW_MSG	"invalid src bitmap width"
#define ERR_ISRCBH	INT_ERR+34
#define ERR_ISRCBH_MSG	"invalid src bitmap height"
#define ERR_ISRCC	INT_ERR+35
#define ERR_ISRCC_MSG	"invalid src constant"
#define ERR_ISRCBD	INT_ERR+36
#define ERR_ISRCBD_MSG	"invalid src bitmap depth"
#define ERR_ISRCD	INT_ERR+37
#define ERR_ISRCD_MSG	"invalid src bitmap dimension"

#define ERR_IMSKMB	INT_ERR+38
#define ERR_IMSKMB_MSG	"invalid msk modifier bits"
#define ERR_IMSKBW	INT_ERR+39
#define ERR_IMSKBW_MSG	"invalid msk bitmap width"
#define ERR_IMSKBH	INT_ERR+40
#define ERR_IMSKBH_MSG	"invalid msk bitmap height"
#define ERR_IMSKBD	INT_ERR+41
#define ERR_IMSKBD_MSG	"invalid msk bitmap depth"

#define ERR_IDSTMB	INT_ERR+44
#define ERR_IDSTMB_MSG	"invalid dst-offset modifier"
#define ERR_IDSTBW	INT_ERR+45
#define ERR_IDSTBW_MSG	"invalid dst bitmap width"
#define ERR_IDSTBH	INT_ERR+46
#define ERR_IDSTBH_MSG	"invalid dst bitmap height"
#define ERR_IDSTBD	INT_ERR+47
#define ERR_IDSTBD_MSG	"invalid dst bitmap depth"

#define ERR_NOAREA	INT_ERR+48
#define ERR_NOAREA_MSG	"no resultant area"

#define ERR_IMAPMB	INT_ERR+50
#define ERR_IMAPMB_MSG	"invalid map modifier bits"
#define ERR_IMAPFC	INT_ERR+51
#define ERR_IMAPFC_MSG	"invalid map function code"
#define ERR_ZIMAP	INT_ERR+52
#define ERR_ZIMAP_MSG	"depth incompatible with map"
#define ERR_ZCIMAP	INT_ERR+53
#define ERR_ZCIMAP_MSG	"depth combination incompatible with map"

#define ERR_ICLPMB	INT_ERR+54
#define ERR_ICLPMB_MSG	"invalid clipr modifier bits"
#define ERR_ICLPRC	INT_ERR+55
#define ERR_ICLPRC_MSG	"invalid clipr count"

#define ERR_SMC_ITC	INT_ERR+56
#define ERR_SMC_ITC_MSG	"invalid tracking ratio"
#define ERR_ITC_MULT	INT_ERR+57
#define ERR_ITC_MULT_MSG "invalid tracking multiplier"
#define ERR_ITC_DIV	INT_ERR+58
#define ERR_ITC_DIV_MSG	"invalid tracking divisor"

#define ERR_ICD		INT_ERR+59
#define ERR_ICD_MSG	"invalid cursor device"
#define ERR_MO_IBC	INT_ERR+60
#define ERR_MO_IBC_MSG	"invalid byte count"
#define ERR_MO_IOT	INT_ERR+61
#define ERR_MO_IOT_MSG	"invalid object type"
#define ERR_MO_IDT	INT_ERR+62
#define ERR_MO_IDT_MSG	"invalid device type"
#define ERR_IPC		INT_ERR+63
#define ERR_IPC_MSG	"invalid path count"

#define ERR_DC_IPC	INT_ERR+64
#define ERR_DC_IPC_MSG	"invalid path count"
#define ERR_DC_IPSL	INT_ERR+65
#define ERR_DC_IPSL_MSG	"invalid pattern string length"
#define ERR_DC_IPSM	INT_ERR+66
#define ERR_DC_IPSM_MSG	"invalid pattern string multiplier"
#define ERR_DC_ICF	INT_ERR+67
#define ERR_DC_ICF_MSG	"invalid closed figure"
#define ERR_DC_IPSP	INT_ERR+68
#define ERR_DC_IPSP_MSG	"invalid pattern position"
#define ERR_DC_IPSMB	INT_ERR+69
#define ERR_DC_IPSMB_MSG "invalid pattern string modifier bits"
#define ERR_DC_IPMMB	INT_ERR+70
#define ERR_DC_IPMMB_MSG "invalid pattern mode modifier bits"
#define ERR_DC_IPSC	INT_ERR+71
#define ERR_DC_IPSC_MSG	"invalid pattern count"
#define ERR_DC_ISSRCBW	INT_ERR+72
#define ERR_DC_ISSRCBW_MSG "invalid second src bitmap width"
#define ERR_DC_ISSRCBH	INT_ERR+73
#define ERR_DC_ISSRCBH_MSG "invalid second src bitmap height"
#define ERR_DC_ISSRCBD	INT_ERR+74
#define ERR_DC_ISSRCBD_MSG "invalid second src bitmap depth"
#define ERR_DC_ISSRCC	INT_ERR+75
#define ERR_DC_ISSRCC_MSG "invalid second src constant"
#define ERR_DC_IDPM	INT_ERR+76
#define ERR_DC_IDPM_MSG	"incompatible drawing/pattern mode"

#define ERR_PT_ICSL	INT_ERR+80
#define ERR_PT_ICSL_MSG	"invalid control string length"
#define ERR_PT_ICSO	INT_ERR+81
#define ERR_PT_ICSO_MSG	"invalid control string opcode"
#define ERR_PT_ICSP	INT_ERR+82
#define ERR_PT_ICSP_MSG	"invalid control string parameter"
#define ERR_PT_ITSL	INT_ERR+83
#define ERR_PT_ITSL_MSG	"invalid text string length"
#define ERR_PT_ICI	INT_ERR+84
#define ERR_PT_ICI_MSG	"invalid character index"
#define ERR_PT_TSE	INT_ERR+85
#define ERR_PT_TSE_MSG	"test string exhausted"
#define ERR_PT_NFP	INT_ERR+86
#define ERR_PT_NFP_MSG	"no font present"
#define ERR_PT_ISRCFW	INT_ERR+87
#define ERR_PT_ISRCFW_MSG "invalid src font width"
#define ERR_PT_ISRCFH	INT_ERR+88
#define ERR_PT_ISRCFH_MSG "invalid src font height"
#define ERR_PT_ISRCFD	INT_ERR+89
#define ERR_PT_ISRCFD_MSG "invalid src font depth"
#define ERR_PT_IMSKFW	INT_ERR+90
#define ERR_PT_IMSKFW_MSG "invalid msk font width"
#define ERR_PT_IMSKFH	INT_ERR+91
#define ERR_PT_IMSKFH_MSG "invalid msk font height"
#define ERR_PT_IMSKFD	INT_ERR+92
#define ERR_PT_IMSKFD_MSG "invalid msk font depth"
#define ERR_PT_CSMF	INT_ERR+93
#define ERR_PT_CSMF_MSG	"conflicting src/msk fonts"

#define ERR_FA_ISRCB	INT_ERR+96
#define ERR_FA_ISRCB_MSG "invalid src bitmap"
#define ERR_FA_SPIOB	INT_ERR+98
#define ERR_FA_SPIOB_MSG "seed point is on boundary"
#define ERR_FA_SO	INT_ERR+99
#define ERR_FA_SO_MSG	"stack overflow"
#define ERR_FA_IBMMB	INT_ERR+100
#define ERR_FA_IBMMB_MSG "invalid boundary map modifier bits"

#define ERR_FP_ISRCB	INT_ERR+112
#define ERR_FP_ISRCB_MSG "invalid src bitmap"
#define ERR_FP_SO	INT_ERR+113
#define ERR_FP_SO_MSG	"stack overflow"
#define ERR_FP_IPC	INT_ERR+114
#define ERR_FP_IPC_MSG	"invalid point count"
#define ERR_FP_ICF	INT_ERR+115
#define ERR_FP_ICF_MSG	"invalid closed figure"

#define ERR_68K		INT_ERR+129
#define ERR_68K_MSG	"68000 cpu"
#define ERR_RC		INT_ERR+130
#define ERR_RC_MSG	"rom checksum"
#define ERR_PR		INT_ERR+131
#define ERR_PR_MSG	"program ram"
#define ERR_CRT		INT_ERR+132
#define ERR_CRT_MSG	"crtc register"
#define ERR_TU		INT_ERR+133
#define ERR_TU_MSG	"tablet usart"
#define ERR_KU		INT_ERR+134
#define ERR_KU_MSG	"keyboard usart"
#define ERR_FOE		INT_ERR+135
#define ERR_FOE_MSG	"fotr electrical loop back"
#define ERR_VTO		INT_ERR+136
#define ERR_VTO_MSG	"vsync time out"
#define ERR_SB		INT_ERR+137
#define ERR_SB_MSG	"screen buffer"
#define ERR_BS		INT_ERR+138
#define ERR_BS_MSG	"bba scratchpad ram"
#define ERR_BC		INT_ERR+139
#define ERR_BC_MSG	"bba copyarea command"
#define ERR_TTO		INT_ERR+140
#define ERR_TTO_MSG	"tablet time out"
#define ERR_FOO		INT_ERR+141
#define ERR_FOO_MSG	"fotr optical loop back"
#define ERR_KTO		INT_ERR+142
#define ERR_KTO_MSG	"keyboard time out"
#define ERR_KST		INT_ERR+143
#define ERR_KST_MSG	"keyboard self-test"

#define ERR_LDC_IATRV	INT_ERR+160
#define ERR_LDC_IATRV_MSG "invalid cursor attribute value"
#define ERR_LDC_ICH	INT_ERR+161
#define ERR_LDC_ICH_MSG	"invalid cursor height"
#define ERR_LDC_ICW	INT_ERR+162
#define ERR_LDC_ICW_MSG	"invalid cursor width"
#define ERR_NOVALCUR	INT_ERR+163
#define ERR_NOVALCUR_MSG "no valid cursor defined"

#define NUM_ERR_CODES	108

#endif
