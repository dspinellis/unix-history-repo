/* $Header: packet.h,v 10.3 86/02/01 15:47:13 tony Rel $ */
/* packet.h	Packet descriptions for Workstation Graphics Architecture
 *		command packets
 *
 *	Each object has two definitions.  The more human-readable one
 *	has "reasonable" definitions, the one beginning with a_ expresses
 *	the structure as an array of shorts so that the C compiler doesn't
 *	move it around for silly alignment reasons.
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

#ifndef WGA_PACKET
#define WGA_PACKET

typedef struct _CopyAreaMod {
	unsigned : 8;
	unsigned m_source : 3;
	unsigned m_mask : 3;
	unsigned : 3;
	unsigned m_map : 3;
	unsigned m_clipping : 3;
} CopyAreaMod;

typedef struct _DrawCurveMod {
	unsigned : 8;
	unsigned m_source : 3;
	unsigned m_mask : 3;
	unsigned : 3;
	unsigned m_map : 3;
	unsigned m_clipping : 3;
	unsigned m_drawMode : 1;
	unsigned m_patState : 2;
	unsigned m_patMode : 2;
} DrawCurveMod;

typedef struct _PrintTextMod {
	unsigned : 8;
	unsigned m_source : 3;
	unsigned m_mask: 3;
	unsigned m_dest : 3;
	unsigned m_map : 3;
	unsigned m_clipping : 3;
	unsigned m_textSize : 1;
	unsigned m_control : 1;
} PrintTextMod;

typedef struct _FillAreaMod {
	unsigned : 8;
	unsigned m_source : 3;
	unsigned : 6;
	unsigned m_map : 3;
	unsigned m_clipping : 3;
} FillAreaMod;

typedef struct _FloodAreaMod {
	unsigned : 8;
	unsigned m_source : 3;
	unsigned : 9;
	unsigned m_clipping : 3;
	unsigned m_boundary : 1;
} FloodAreaMod;

typedef struct _LoadCursorMod {
	unsigned : 8;
	unsigned m_source : 3;
	unsigned m_mask : 3;
	unsigned : 3;
	unsigned m_map : 3;
} LoadCursorMod;

typedef struct _SetMouseCharacteristicsMod {
	unsigned : 8;
	unsigned m_tracking : 3;
} SetMouseCharacteristicsMod;

typedef struct _SetTabletCharacteristicsMod {
	unsigned : 8;
	unsigned m_tracking : 3;
} SetTabletCharacteristicsMod;

typedef struct _PacketHeader {
	union {
	    struct {
		unsigned _opcode : 8;
		unsigned _modifier : 24;
	    } ph;
	    long emptymod;
	    CopyAreaMod copy;
	    DrawCurveMod draw;
	    PrintTextMod text;
	    FillAreaMod fill;
	    FloodAreaMod flood;
	    LoadCursorMod cursor;
	    SetMouseCharacteristicsMod mouse;
	    SetTabletCharacteristicsMod tablet;
	} ph_modifier;
	a_CharPtr ph_next;
} PacketHeader;

#define ph_copyMod ph_modifier.copy
#define ph_drawMod ph_modifier.draw
#define ph_textMod ph_modifier.text
#define ph_fillMod ph_modifier.fill
#define ph_floodMod ph_modifier.flood
#define ph_cursorMod ph_modifier.cursor
#define ph_mouseMod ph_modifier.mouse
#define ph_tabletMod ph_modifier.tablet

typedef short a_PacketHeader[4];

#define ph_opcode ph_modifier.ph._opcode

#define COPY_AREA	1

typedef struct _CopyAreaPacket {
	a_PacketHeader cap_head;
	union {
	    a_SubBitmap image;
	    Constant const;
	    a_Halftone pattern;
	} cap_source;
	a_SubBitmap cap_sourceMask;
	a_Extent cap_maskSize;
	a_Bitmap cap_destImage;
	a_Point	cap_destOffset;
	union {
	    a_MapPtr ptr;
	    a_Map literal;
	} cap_map;
	union {
	    a_RectangleList rectList;
	    a_Rectangle litRect;
	} cap_clipping;
} CopyAreaPacket;

typedef short a_CopyAreaPacket[33];

#define DRAW_CURVE	2

typedef struct _DrawCurvePacket {
	a_PacketHeader dcp_head;
	union {
	    a_SubBitmap image;
	    Constant const;
	    a_Halftone pattern;
	} dcp_source;
	a_SubBitmap dcp_sourceMask;
	a_Extent dcp_maskSize;
	a_Bitmap dcp_destImage;
	a_Point dcp_destOffset;
	union {
	    a_MapPtr ptr;
	    a_Map literal;
	} dcp_map;
	union {
	    a_RectangleList rectList;
	    a_Rectangle litRect;
	} dcp_clipping;
	a_SegmentList dcp_path;
	a_PatternString dcp_pattern;
	union {
	    a_PatternState literal;
	    a_PatternStatePtr ptr;
	} dcp_patState;
 	union {
	    a_SubBitmap image;
	    Constant const;
	    a_Halftone pattern;
	} dcp_secondSource;
} DrawCurvePacket;

typedef short a_DrawCurvePacket[49];

#define PRINT_TEXT	3

typedef struct _PrintTextPacket {
	a_PacketHeader ptp_head;
	union {
	    a_FontPtr font;
	    Constant const;
	    a_Halftone pattern;
	} ptp_source;
	a_FontPtr ptp_mask;
	short filler[7];
	a_Bitmap ptp_destImage;
	union {
	    a_Point literal;
	    a_PointPtr ptr;
	} ptp_initialOffset;
	union {
	    a_MapPtr ptr;
	    a_Map literal;
	} ptp_map;
	union {
	    a_RectangleList rectList;
	    a_Rectangle litRect;
	} ptp_clipping;
	a_TextString ptp_text;
	a_ControlString ptp_control;
	short ptp_charPad;
	short ptp_spacePad;
} PrintTextPacket;

typedef short a_PrintTextPacket[41];

#define FILL_AREA	11

typedef struct _FillAreaPacket {
	a_PacketHeader fap_head;
 	union {
	    Constant const;
	    a_Halftone pattern;
	} fap_source;
	a_Bitmap fap_destImage;
	a_Point fap_destOffset;
	union {
	    a_MapPtr ptr;
	    a_Map literal;
	} fap_map;
	a_Rectangle fap_clippingRec;
	a_SegmentList fap_path;
} FillAreaPacket;

typedef short a_FillAreaPacket[28];

#define FLOOD_AREA	4

typedef struct _FloodAreaPacket {
	a_PacketHeader flp_head;
	union {
	    Constant const;
	    a_Halftone pattern;
	} flp_source;
	a_Bitmap flp_destImage;
	a_Point flp_seed;
	Rectangle flp_clippingRec;
	union {
	    a_MapPtr ptr;
	    a_Map literal;
	} flp_boundary;
} FloodAreaPacket;

typedef short a_FloodAreaPacket[24];

#define LOAD_CURSOR	5

typedef struct _LoadCursorPacket {
	a_PacketHeader lcp_head;
	union {
	    a_SubBitmap image;
	    Constant const;
	    a_Halftone pattern;
	} lcp_source;
	a_SubBitmap lcp_sourceMask;
	a_Extent lcp_maskSize;
	union {
	    a_MapPtr ptr;
	    a_Map literal;
	} lcp_map;
	short lcp_blink;
	short lcp_tip_x;
	short lcp_tip_y;
	short lcp_center_x;
	short lcp_center_y;
} LoadCursorPacket;

typedef short a_LoadCursorPacket[29];

#define SET_CURSOR_POSITION	6

typedef struct _SetCursorPositionPacket {
	a_PacketHeader scp_head;
	a_Point scp_position;
} SetCursorPositionPacket;

typedef short a_SetCursorPositionPacket[6];

#define ATTACH_CURSOR	7

typedef struct _AttachCursorPacket {
	a_PacketHeader acp_head;
	short acp_device;
} AttachCursorPacket;

typedef short a_AttachCursorPacket[5];

#define GET_CURSOR_POSITION	8

typedef struct _GetCursorPositionPacket {
	a_PacketHeader gcp_head;
	a_Point gcp_position;
} GetCursorPositionPacket;

typedef short a_GetCursorPositionPacket[6];

#define MOVE_OBJECT	9
#define MOVE_OBJECT_ROM	128

typedef struct _MoveObjectPacket {
	a_PacketHeader mop_head;
	short mop_objectType;
	a_MemSize mop_objectSize;
	a_CharPtr mop_source;
	a_CharPtr mop_dest;
} MoveObjectPacket;

typedef short a_MoveObjectPacket[11];

#define REPORT_STATUS	10
#define REPORT_STATUS_ROM	129

typedef struct _ReportStatusPacket {
	a_PacketHeader rsp_head;
	char rsp_deviceType[4];
	short rsp_deviceVersion;
	short rsp_ucodeVersion;
	a_Bitmap rsp_visibleFramebuffer;
	a_MemArea rsp_freeFramebuffer;
	a_MemArea rsp_freeProgramMemory;
	a_MemArea rsp_hostMemory;
} ReportStatusPacket;

typedef short a_ReportStatusPacket[25];

#define NO_OPERATION	0

typedef struct _NoopPacket {
	a_PacketHeader nop_head;
} NoopPacket;

typedef short a_NoopPacket[4];
 
#define GET_MOUSE_POSITION	12

typedef struct _GetMousePositionPacket {
	a_PacketHeader gmp_head;
	a_Point gmp_position;
} GetMousePositionPacket;

typedef short a_GetMousePositionPacket[6];

#define SET_MOUSE_CHARACTERISTICS	13

typedef struct _SetMouseCharacteristicsPacket {
	a_PacketHeader smc_head;
	union {
		struct {
			short multiplier;
			short divisor;
		} linear;
		struct {
			short scale;
			short threshold;
		} exponential;
	} tracking;
} SetMouseCharacteristicsPacket;


#define smc_multiplier tracking.linear.multiplier
#define smc_divisor tracking.linear.divisor
#define smc_threshold tracking.exponential.threshold
#define smc_scale tracking.exponential.scale

typedef short a_SetMouseCharacteristicsPacket[6];

#define GET_TABLET_POSITION	14

typedef struct _GetTabletPositionPacket {
	a_PacketHeader gtp_head;
	a_Point gtp_position;
} GetTabletPositionPacket;

typedef short a_GetTabletPositionPacket[6];

#define SET_TABLET_CHARACTERISTICS	16

typedef struct _SetTabletCharacteristicsPacket {
	a_PacketHeader stc_head;
	union {
		struct {
			short multiplier;
			short divisor;
		} linear;
		struct {
			a_Point input;
			a_Point output;
		} quantized;
	} tracking;
} SetTabletCharacteristicsPacket;

#define stc_multiplier tracking.linear.multiplier
#define stc_divisor tracking.linear.divisor
#define stc_input tracking.quantized.input
#define stc_output tracking.quantized.output

typedef short a_SetTabletCharacteristicsPacket[8];

#define SET_POINTING_DEVICE_REPORTING	15

typedef struct _SetPointingDeviceReportingPacket {
	a_PacketHeader spdp_head;
	short spdp_enable;
} SetPointingDeviceReportingPacket;

typedef short a_SetPointingDeviceReportingPacket[5];

typedef union _Packet {
	a_CopyAreaPacket copyArea;
	a_DrawCurvePacket drawCurve;
	a_PrintTextPacket writeText;
	a_FillAreaPacket fillArea;
	a_FloodAreaPacket floodArea;
	a_LoadCursorPacket loadCursor;
	a_SetCursorPositionPacket setCursor;
	a_AttachCursorPacket attachCursor;
	a_GetCursorPositionPacket getCursor;
	a_MoveObjectPacket moveObject;
	a_ReportStatusPacket reportStatus;
	a_NoopPacket noop;
	a_GetMousePositionPacket getMouse;
	a_SetMouseCharacteristicsPacket setMouse;
	a_GetTabletPositionPacket getTablet;
	a_SetTabletCharacteristicsPacket setTablet;
	a_SetPointingDeviceReportingPacket setPointingDevice;
} Packet;	

#endif
