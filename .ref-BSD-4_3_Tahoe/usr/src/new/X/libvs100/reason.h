/* reason.h	Interrupt reason values for Nov 84 firmware
 *
 * Author:	Paul J. Asente
 * 		Digital Equipment Corporation
 * 		Western Reseach Lab
 * Date:	Sept 1985
 */

/****************************************************************************
 *									    *
 *  Copyright (c) 1985 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished under a license and may be used and copied   *
 *  only in  accordance with  the  terms  of  such  license  and with the   *
 *  inclusion of the above copyright notice. This software or  any  other   *
 *  copies thereof may not be provided or otherwise made available to any   *
 *  other person.  No title to and ownership of  the  software is  hereby   *
 *  transferred.							    *
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

#define IR_ID		1<<0
#define IR_ID_MSG	"initialization done"
#define IR_CD		1<<1
#define IR_CD_MSG	"command done"
#define IR_DCI		1<<2
#define IR_DCI_MSG	"display code initialized"
#define IR_DE		1<<3
#define IR_DE_MSG	"device event"
#define IR_CM		1<<4
#define IR_CM_MSG	"cursor moved"
#define IR_DPM		1<<5
#define IR_DPM_MSG	"data pad moved"
#define IR_MM		1<<6
#define IR_MM_MSG	"mouse moved"
#define IR_PD		1<<7
#define IR_PD_MSG	"powerup done"
#define IR_AB_ACK	1<<8
#define IR_AB_ACK_MSG	"abort acknowledge"
#define IR_STAT_ACK	1<<9
#define IR_STAT_ACK_MSG	"status acknowledge"
#define IR_SAVE_ACK	1<<10
#define IR_SAVE_ACK_MSG	"save_screen acknowledge"

#define IR_ERROR	(1<<15)

#define ERR_NYI		IR_ERROR+0
#define ERR_NYI_MSG	"function not yet implemented"
#define ERR_IFC		IR_ERROR+1
#define ERR_IFC_MSG	"invalid function code"
#define ERR_ICC		IR_ERROR+2
#define ERR_ICC_MSG	"invalid command code"
#define ERR_BE_NXM_68K	IR_ERROR+3
#define ERR_BE_NXM_68K_MSG	"bus error:  receive nxm -- 68K unibus access"
#define ERR_BE_RO	IR_ERROR+4
#define ERR_BE_RO_MSG	"bus error:  retry overflow"
#define ERR_BE_LD	IR_ERROR+5
#define ERR_BE_LD_MSG	"bus error:  link down"
#define ERR_BE_S	IR_ERROR+6
#define ERR_BE_S_MSG	"bus error:  spurious"
#define ERR_AE		IR_ERROR+7
#define ERR_AE_MSG	"address error"
#define ERR_ZD		IR_ERROR+8
#define ERR_ZD_MSG	"zero divide"
#define ERR_II		IR_ERROR+9
#define ERR_II_MSG	"illegal instruction"
#define ERR_NXM_BBA	IR_ERROR+10
#define ERR_NXM_BBA_MSG	"receive nxm -- BBA unibus access"
#define ERR_BNI		IR_ERROR+11
#define ERR_BNI_MSG	"BBA not installed"
#define ERR_KCQO	IR_ERROR+12
#define ERR_KCQO_MSG	"keyboard command queue overflow"
#define ERR_DPCQO	IR_ERROR+13
#define ERR_DPCQO_MSG	"data pad command queue overflow"
#define ERR_MEQO	IR_ERROR+14
#define ERR_MEQO_MSG	"mouse event queue overflow"
#define ERR_DPEQO	IR_ERROR+15
#define ERR_DPEQO_MSG	"data pad event queue overflow"
#define ERR_AB_BBA	IR_ERROR+16
#define ERR_AB_BBA_MSG	"unable to abort the BBA"
#define ERR_KEQO	IR_ERROR+17
#define ERR_KEQO_MSG	"keyboard event queue overflow"
#define ERR_KOR		IR_ERROR+18
#define ERR_KOR_MSG	"keyboard overrun"
#define ERR_DPOR	IR_ERROR+19
#define ERR_DPOR_MSG	"data pad overrun"
#define ERR_KFE		IR_ERROR+20
#define ERR_KFE_MSG	"keyboard framing error"
#define ERR_DPFE	IR_ERROR+21
#define ERR_DPFE_MSG	"data pad framing error"

#define ERR_ISRCM	IR_ERROR+32
#define ERR_ISRCM_MSG	"invalid source modifiers"
#define ERR_ISRCBW	IR_ERROR+33
#define ERR_ISRCBW_MSG	"invalid source bitmap width"
#define ERR_ISRCBH	IR_ERROR+34
#define ERR_ISRCBH_MSG	"invalid source bitmap height"
#define ERR_ISRCC	IR_ERROR+35
#define ERR_ISRCC_MSG	"invalid source constant"
#define ERR_ISRCBD	IR_ERROR+36
#define ERR_ISRCBD_MSG	"invalid source bitmap depth"
#define ERR_IMSKM	IR_ERROR+38
#define ERR_IMSKM_MSG	"invalid source mask modifiers"
#define ERR_IMSKBW	IR_ERROR+39
#define ERR_IMSKBW_MSG	"invalid source mask bitmap width"
#define ERR_IMSKBH	IR_ERROR+40
#define ERR_IMSKBH_MSG	"invalid source mask bitmap height"
#define ERR_IMSKBD	IR_ERROR+41
#define ERR_IMSKBD_MSG	"invalid source mask bitmap depth"
#define ERR_IDSTM	IR_ERROR+44
#define ERR_IDSTM_MSG	"invalid destination offset modifiers"
#define ERR_IDSTBW	IR_ERROR+45
#define ERR_IDSTBW_MSG	"invalid destination bitmap width"
#define ERR_IDSTBH	IR_ERROR+46
#define ERR_IDSTBH_MSG	"invalid destination bitmap height"
#define ERR_IDSTBD	IR_ERROR+47
#define ERR_IDSTBD_MSG	"invalid destination bitmap depth"
#define ERR_IMAPM	IR_ERROR+50
#define ERR_IMAPM_MSG	"invalid mapping function modifiers"
#define ERR_ICLPM	IR_ERROR+54
#define ERR_ICLPM_MSG	"invalid clipping rectangle modifiers"
#define ERR_ICLPC	IR_ERROR+55
#define ERR_ICLPC_MSG	"invalid clipping rectangle count"

#define ERR_SMC_ITC	IR_ERROR+56
#define ERR_SMC_ITC_MSG	"set_mouse_characteristics:  invalid tracking constant"
#define ERR_SCL_CD	IR_ERROR+58
#define ERR_SCL_CD_MSG	"set_cursor_location:  cursor is detached"
#define ERR_AC_ICD	IR_ERROR+59
#define ERR_AC_ICD_MSG	"attach_cursor:  invalid cursor device"

#define ERR_MO_IBC	IR_ERROR+60
#define ERR_MO_IBC_MSG	"move_object:  invalid byte count"
#define ERR_MO_IOT	IR_ERROR+61
#define ERR_MO_IOT_MSG	"move_object:  invalid object type"
#define ERR_MO_IDT	IR_ERROR+62
#define ERR_MO_IDT_MSG	"move_object:  invalid device type"

#define ERR_IPC		IR_ERROR+63
#define ERR_IPC_MSG	"invalid path count (draw_curve or fill_polygon)"

#define ERR_DC_IPL	IR_ERROR+65
#define ERR_DC_IPL_MSG	"invalid pattern length"
#define ERR_DC_IPM	IR_ERROR+66
#define ERR_DC_IPM_MSG	"invalid pattern multiplier"
#define ERR_DC_ICF	IR_ERROR+67
#define ERR_DC_ICF_MSG	"invalid closed figure"
#define ERR_DC_IPP	IR_ERROR+68
#define ERR_DC_IPP_MSG	"invalid pattern position"
#define ERR_DC_IPSM	IR_ERROR+69
#define ERR_DC_IPSM_MSG	"invalid pattern string modifiers"
#define ERR_DC_IPMM	IR_ERROR+70
#define ERR_DC_IPMM_MSG	"invalid pattern mode modifiers"
#define ERR_DC_IPC	IR_ERROR+71
#define ERR_DC_IPC_MSG	"invalid pattern count"
#define ERR_DC_ISSRCBW	IR_ERROR+72
#define ERR_DC_ISSRCBW_MSG	"invalid second source bitmap width"
#define ERR_DC_ISSRCBH	IR_ERROR+73
#define ERR_DC_ISSRCBH_MSG	"invalid second source bitmap height"
#define ERR_DC_ISSRCBD	IR_ERROR+74
#define ERR_DC_ISSRCBD_MSG	"invalid second source bitmap depth"
#define ERR_DC_ISSRCC	IR_ERROR+75
#define ERR_DC_ISSRCC_MSG	"invalid second source constant"
#define ERR_DC_IDPM	IR_ERROR+76
#define ERR_DC_IDPM_MSG	"incompatible drawing/pattern modes"
#define ERR_DC_DXO	IR_ERROR+77
#define ERR_DC_DXO_MSG	"delta_x overflow"
#define ERR_DC_DYO	IR_ERROR+78
#define ERR_DC_DYO_MSG	"delta_y overflow"
#define ERR_DC_CRSO	IR_ERROR+79
#define ERR_DC_CRSO_MSG	"clipping rectangle stack overflow"

#define ERR_PT_ICSL	IR_ERROR+80
#define ERR_PT_ICSL_MSG	"invalid control string length"
#define ERR_PT_ICSO	IR_ERROR+81
#define ERR_PT_ICSO_MSG	"invalid control string opcode"
#define ERR_PT_ICSP	IR_ERROR+82
#define ERR_PT_ICSP_MSG	"invalid control string parameter"
#define ERR_PT_ITSL	IR_ERROR+83
#define ERR_PT_ITSL_MSG	"invalid text string length"
#define ERR_PT_ICI	IR_ERROR+84
#define ERR_PT_ICI_MSG	"invalid character index"
#define ERR_PT_TSE	IR_ERROR+85
#define ERR_PT_TSE_MSG	"text string exhausted"
#define ERR_PT_NFP	IR_ERROR+86
#define ERR_PT_NFP_MSG	"no font present"
#define ERR_PT_ISRCFW	IR_ERROR+87
#define ERR_PT_ISRCFW_MSG	"invalid source font width"
#define ERR_PT_ISRCFH	IR_ERROR+88
#define ERR_PT_ISRCFH_MSG	"invalid source font height"
#define ERR_PT_ISRCFD	IR_ERROR+89
#define ERR_PT_ISRCFD_MSG	"invalid source font depth"
#define ERR_PT_IMSKFW	IR_ERROR+90
#define ERR_PT_IMSKFW_MSG	"invalid source mask font width"
#define ERR_PT_IMSKFH	IR_ERROR+91
#define ERR_PT_IMSKFH_MSG	"invalid source mask font height"
#define ERR_PT_IMSKFD	IR_ERROR+92
#define ERR_PT_IMSKFD_MSG	"invalid source mask font depth"
#define ERR_PT_CSMF	IR_ERROR+93
#define ERR_PT_CSMF_MSG	"conflicting source/source mask fonts"
#define ERR_PT_ILB	IR_ERROR+94
#define ERR_PT_ILB_MSG	"invalid left_array bounds"
#define ERR_PT_ILE	IR_ERROR+95
#define ERR_PT_ILE_MSG	"invalid left_array element"

#define ERR_FA_ISRCB	IR_ERROR+96
#define ERR_FA_ISRCB_MSG	"invalid source bitmap"
#define ERR_FA_SO	IR_ERROR+99
#define ERR_FA_SO_MSG	"stack overflow"
#define ERR_FA_IBMM	IR_ERROR+100
#define ERR_FA_IBMM_MSG	"invalid boundary map modifiers"

#define ERR_FP_ISRCB	IR_ERROR+112
#define ERR_FP_ISRCB_MSG	"invalid source bitmap"
#define ERR_FP_ICF	IR_ERROR+115
#define ERR_FP_ICF_MSG	"invalid closed figure"
#define ERR_FP_DXO	IR_ERROR+116
#define ERR_FP_DXO_MSG	"delta_x overflow"
#define ERR_FP_DYO	IR_ERROR+117
#define ERR_FP_DYO_MSG	"delta_y overflow"

#define ERR_CPU		IR_ERROR+129
#define ERR_CPU_MSG	"CPU or supervisor stack error"
#define ERR_RHB		IR_ERROR+130
#define ERR_RHB_MSG	"ROM high byte checksum error"
#define ERR_PR		IR_ERROR+131
#define ERR_PR_MSG	"program RAM data error"
#define ERR_CRTC	IR_ERROR+132
#define ERR_CRTC_MSG	"CRT controller data error"
#define ERR_DPE		IR_ERROR+133
#define ERR_DPE_MSG	"data pad EPCI data error or time out"
#define ERR_KE		IR_ERROR+134
#define ERR_KE_MSG	"keyboard EPCI data error or time out"
#define ERR_FOE		IR_ERROR+135
#define ERR_FOE_MSG	"fiber optics electrical loop back"
#define ERR_F0		IR_ERROR+137
#define ERR_F0_MSG	"frame #0 data error"
#define ERR_BSR		IR_ERROR+138
#define ERR_BSR_MSG	"BBA scratchpad RAM data error"
#define ERR_BCC		IR_ERROR+139
#define ERR_BCC_MSG	"BBA copyarea command data error"
#define ERR_DPTO	IR_ERROR+140
#define ERR_DPTO_MSG	"data pad self test time out"
#define ERR_FOO		IR_ERROR+141
#define ERR_FOO_MSG	"fiber optics optical loop back"
#define ERR_KTO		IR_ERROR+142
#define ERR_KTO_MSG	"keyboard self test time out"
#define ERR_KST		IR_ERROR+143
#define ERR_KST_MSG	"keyboard self test error"
#define ERR_VR		IR_ERROR+144
#define ERR_VR_MSG	"vector RAM data error"
#define ERR_F1		IR_ERROR+145
#define ERR_F1_MSG	"frame #1 data error"
#define ERR_F2		IR_ERROR+146
#define ERR_F2_MSG	"frame #2 data error"
#define ERR_F3		IR_ERROR+147
#define ERR_F3_MSG	"frame #3 data error"
#define ERR_DPP		IR_ERROR+148
#define ERR_DPP_MSG	"data pad port loop back data error"
#define ERR_KP		IR_ERROR+149
#define ERR_KP_MSG	"keyboard port loop back data error"
#define ERR_FOED	IR_ERROR+150
#define ERR_FOED_MSG	"fiber optics electrical loop back"
#define ERR_FOOD	IR_ERROR+151
#define ERR_FOOD_MSG	"fiber optics optical loop back"
#define ERR_BCTO	IR_ERROR+152
#define ERR_BCTO_MSG	"BBA copyarea command time out"
#define ERR_RLB		IR_ERROR+153
#define ERR_RLB_MSG	"ROM low byte checksum error"
#define ERR_KD		IR_ERROR+154
#define ERR_KD_MSG	"key down on keyboard self test"
#define ERR_BVC		IR_ERROR+155
#define ERR_BVC_MSG	"BBA vector command data error"

#define ERR_ISRCHW	IR_ERROR+161
#define ERR_ISRCHW_MSG	"invalid source halftone width"
#define ERR_ISRCHH	IR_ERROR+162
#define ERR_ISRCHH_MSG	"invalid source halftone height"
#define ERR_ISRCHD	IR_ERROR+163
#define ERR_ISRCHD_MSG	"invalid source halftone depth"
#define ERR_DC_ISSRCHW	IR_ERROR+164
#define ERR_DC_ISSRCHW_MSG	"invalid second source halftone width"
#define ERR_DC_ISSRCHH	IR_ERROR+165
#define ERR_DC_ISSRCHH_MSG	"invalid second source halftone height"
#define ERR_DC_ISSRCHD	IR_ERROR+166
#define ERR_DC_ISSRCHD_MSG	"invalid second source halftone depth"

#define ERR_LC_ICTX	IR_ERROR+167
#define ERR_LC_ICTX_MSG	"load_cursor:  invalid cursor tip_x"
#define ERR_LC_ICTY	IR_ERROR+168
#define ERR_LC_ICTY_MSG	"load_cursor:  invalid cursor tip_y"
#define ERR_LC_ICCX	IR_ERROR+169
#define ERR_LC_ICCX_MSG	"load_cursor:  invalid cursor centre_x"
#define ERR_LC_ICCY	IR_ERROR+170
#define ERR_LC_ICCY_MSG	"load_cursor:  invalid cursor centre_y"
#define ERR_LC_ICW	IR_ERROR+171
#define ERR_LC_ICW_MSG	"load_cursor:  invalid cursor width"
#define ERR_LC_ICH	IR_ERROR+172
#define ERR_LC_ICH_MSG	"load_cursor:  invalid cursor height"
#define ERR_PT_DXO	IR_ERROR+173
#define ERR_PT_DXO_MSG	"print_text:  destination offset x overflow"
#define ERR_PT_CRSO	IR_ERROR+174
#define ERR_PT_CRSO_MSG	"print_text:  clipping rectangle stack overflow"

#define NUM_ERR_CODES 134

#endif
