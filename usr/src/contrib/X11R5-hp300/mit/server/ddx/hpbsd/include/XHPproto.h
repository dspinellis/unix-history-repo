
/*************************************************************************
 * 
 * (c)Copyright 1992 Hewlett-Packard Co.,  All Rights Reserved.
 * 
 *                          RESTRICTED RIGHTS LEGEND
 * Use, duplication, or disclosure by the U.S. Government is subject to
 * restrictions as set forth in sub-paragraph (c)(1)(ii) of the Rights in
 * Technical Data and Computer Software clause in DFARS 252.227-7013.
 * 
 *                          Hewlett-Packard Company
 *                          3000 Hanover Street
 *                          Palo Alto, CA 94304 U.S.A.
 * 
 * Rights for non-DOD U.S. Government Departments and Agencies are as set
 * forth in FAR 52.227-19(c)(1,2).
 *
 *************************************************************************/

#ifndef XHPPROTO_H
#define XHPPROTO_H

/* Definitions for HP extensions used by the server and C bindings*/

#ifndef XMD_H
#include "Xmd.h"
#endif

/* detailed device information */

#define NO_DEVICE	0x00
#define HP_ITF_KBD	0x01
#define PC101_KBD	0x02
#define IBM3270_KBD	0x03
#define LK201_KBD	0x04
#define LK401_KBD	0x05
#define UNIX_KBD	0x06
#define SUN3_KBD	0x07
#define SUN4_KBD	0x08

#define HP_HIL 		0x40
#define PS2    		0x80
#define SERIAL 		0xC0

#define ALLWINDOWS	-1

#define STDEVENTS	-1
#define EXTEVENTS	0
#define CORE_EVENTS     64

#define REPEAT_30	0
#define REPEAT_60	1

#define	OFF		(0L << 0)
#define	ON		(1L << 0)
#define	RELATIVE 	(0L << 1)
#define	ABSOLUTE 	(1L << 1)
#define	SYSTEM_EVENTS	(0L << 2)
#define	DEVICE_EVENTS	(1L << 2)

/* HP devices */

#define	XPOINTER		0
#define	XKEYBOARD		1
#define	XOTHER   		2

#define NUM_DEV_TYPES		19

#if defined(__hp9000s300) || defined(__hp9000s700) || defined(hp300)
#define	MAX_POSITIONS		7
#define MAX_LOGICAL_DEVS	9
#else
#define	MAX_POSITIONS		28
#define MAX_LOGICAL_DEVS	30
#endif

#define NULL_DEVICE		0
#define MOUSE			1
#define TABLET			2
#define KEYBOARD		3
#define QUADRATURE		4
#define TOUCHSCREEN		5
#define TOUCHPAD		6
#define BUTTONBOX		7
#define BARCODE			8
#define ONE_KNOB    		9
#define TRACKBALL		10
#define KEYPAD   		11
#define NINE_KNOB    		12
#define ID_MODULE 		13
#define	VCD_8_DIALBOX		14
#define	MMII_1812_TABLET	15
#define	SS_SPACEBALL		16
#define	APOLLO_LPFK		17
#define	MMII_1201_TABLET	18

#define DVKeyClickPercent	(1L<<0)
#define DVBellPercent		(1L<<1)
#define DVBellPitch		(1L<<2)
#define DVBellDuration		(1L<<3)
#define DVLed			(1L<<4)
#define DVLedMode		(1L<<5)
#define DVKey			(1L<<6)
#define DVAutoRepeatMode	(1L<<7)
#define DVAccelNum		(1L<<8)
#define DVAccelDenom		(1L<<9)
#define DVThreshold 		(1L<<10)

/*
 * Display modes, needed by both server and clients.
 * 
 * 
 */

#define OVERLAY_MODE			0x1
#define IMAGE_MODE			0x2
#define STACKED_MODE			0x3
#define STACKED_MODE_FIRST_SCREEN	0x3
#define STACKED_MODE_SECOND_SCREEN	0x33
#define COMBINED_MODE			0x4

#define XHPOVERLAY_MODE			OVERLAY_MODE
#define XHPIMAGE_MODE			IMAGE_MODE
#define XHPSTACKED_SCREENS_MODE		STACKED_MODE
#define XHPCOMBINED_MODE		COMBINED_MODE

/*
 * Server exit error codes
 *
 */
#define NORMAL_TERMINATION				0
#define UNABLE_TO_INITIALIZE_THE_DISPLAY		1
#define INCORRECT_SB_DISPLAY_ADDR_ENVIRONMENT_VAR	2
#define INCORRECT_GRM_SIZE_ENVIRONMENT_VAR		3
#define UNABLE_TO_STARTUP_OR_CONNECT_TO_THE_GRM		4
#define DISPLAY_TYPE_UNKNOWN_TO_SERVER         		5
#define DOUBLE_BUFFERING_NOT_SUPPORTED        		6
#define DOUBLE_BUFFERING_HARDWARE_NOT_PRESENT 		7
#define CANNOT_SUPPORT_DEPTHS_OPTION_ON_THIS_DISPLAY    8
#define UNSUPPORTED_DEPTH_SPECIFIED                     9
#define HARDWARE_AT_THIS_DEPTH_NOT_PRESENT              10
#define DUPLICATE_DEVICE_ENTRY_IN_FILE                  11
#define CANNOT_GET_DRIVER_INFO		                12
#define COMBINED_MODE_NOT_STACKED_SCREENS_MODE		13
#define TWO_TIMES_COMBINED_MODE                         14
#define SECONDARY_DIFFERENT_FROM_PRIMARY		15

#define NEXTTRACK_SERVER	0	/* HP-UX 6.2 */
#define MERGE_SERVER		1	/* HP-UX 6.5 */
#define REL_70_SERVER		2	/* HP-UX 7.0 */
#define REL_701_SERVER		3	/* HP-UX 7.05 */
#define REL_80_SERVER		4	/* HP-UX 8.0 */
#define REL_807_SERVER		5	/* HP-UX 8.07 (IF2) */
#define REL_90_SERVER		6	/* HP-UX 9.0 R5 */

#endif
