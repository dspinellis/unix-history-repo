
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

#ifndef HPEXT_H
#define HPEXT_H
/* $Header: /host/kaukau/disk2/X11R5/R5-hp300/mit/server/ddx/hpbsd/input/RCS/hpext.h,v 1.1 1992/09/30 03:14:10 root Exp $ */

#define  NEED_EVENTS
#define  NEED_REPLIES
#include "Xproto.h"
#include "X.h"
#include "XHPproto.h"			/* server version definitions */
#ifdef XINPUT
#include "XIproto.h"
#endif /* XINPUT */

/* Definitions for HP extensions used by the server and Xlib */

/*********************************************************
 *
 * number of hp events, errors, and extension name.
 *
 */

#define HPEVENTS		12
#define HPERRORS		3
#define CLIENT_REQ		1
#define HPNAME 			"HPExtension"
#define MIN_EVENT_REQUEST	1
#define MAX_EVENT_REQUEST	11

/*********************************************************
 *
 * Protocol request constants
 *
 */

#define X_GetHpKeyboardId		1	/* DO NOT CHANGE THIS LINE! */
#define X_HPListInputDevices		2
#define X_HPSetInputDevice		3
#define X_HPGetExtEventMask		4
#define X_HPGetDeviceFocus		5
#define X_HPGetClipList			6
#define X_HPGrabDevice 			7
#define X_HPSetDeviceFocus		8
#define X_HPUnGrabDevice  		9
#define X_HPSelectExtensionEvent  	10
#define X_HPGetCurrentDeviceMask  	11
#define X_HPEnableReset 		12
#define X_HPDisableReset 		13
#define X_HPGetDeviceMotionEvents 	14
#define X_HPGrabDeviceButton		15
#define X_HPUngrabDeviceButton		16
#define X_HPGrabDeviceKey		17
#define X_HPUngrabDeviceKey		18
#define X_HPDeviceAutoRepeatOn		19
#define X_HPDeviceAutoRepeatOff		20
#define X_HPPrompt			21
#define X_HPAcknowledge			22
#define X_HPRegisterWindow		23
#define X_HPUnRegisterWindow		24
#define X_HPSynchronizeColorRange	25
#define X_HPGetServerMode		26
#define X_HPGetDeviceKeyMapping		27
#define X_HPChangeDeviceKeyMapping	28
#define X_HPGetDeviceModifierMapping	29
#define X_HPSetDeviceModifierMapping	30
#define X_HPGetDeviceControl		31
#define X_HPChangeDeviceControl		32
#define X_HPGetWindowCursor		33
#define X_HPGrabReset			34
#define X_HPSendDdxDriverMsg		35
#define X_HPGetClipLists		36
#define X_HPSSChange			37	/* Screen saver change */

#define sz_xHPListInputDevicesReq		  4
#define sz_xHPListInputDevicesReply		 32
#define sz_xHPSetInputDeviceReq			 12
#define sz_xHPSetInputDeviceReply		 32
#define sz_xHPGetExtEventMaskReq		  8
#define sz_xHPGetExtEventMaskReply		 32
#define sz_xHPGetDeviceFocusReq			  8
#define sz_xHPGetDeviceFocusReply		 32
#define sz_xHPGetClipListReq			 16
#define sz_xHPGetClipListReply			 32
#define sz_xHPGrabDeviceReq			 24
#define sz_xHPGrabDeviceReply			 32
#define sz_xHPSetDeviceFocusReq			 20
#define sz_xHPUnGrabDeviceReq			 12
#define sz_xHPSelectExtensionEventReq   	 16
#define sz_xHPGetCurrentDeviceMaskReq		 12
#define sz_xHPGetCurrentDeviceMaskReply 	 32
#define sz_xHPEnableResetReq			  4
#define sz_xHPDisableResetReq			  4
#define sz_xHPGetDeviceMotionEventsReq		 20
#define sz_xHPGetDeviceMotionEventsReply 	 32
#define sz_xHPGrabDeviceButtonReq		 24
#define sz_xHPUngrabDeviceButtonReq		 16
#define sz_xHPGrabDeviceKeyReq			 20
#define sz_xHPUngrabDeviceKeyReq		 16
#define sz_xHPDeviceAutoRepeatOnReq		 12
#define sz_xHPDeviceAutoRepeatOffReq		  8
#define sz_xHPPromptReq				 12
#define sz_xHPAcknowledgeReq			 12
#define sz_xHPRegisterWindowReq			 16
#define sz_xHPRegisterWindowReply      		152
#define sz_xHPSynchronizeColorRangeReq  	 16
#define sz_xHPGetServerModeReq		 	  8
#define sz_xHPGetServerModeReply		 32
#define sz_xHPGetDeviceKeyMappingReq		 12
#define sz_xHPGetDeviceKeyMappingReply  	 32
#define sz_xHPChangeDeviceKeyMappingReq 	 12
#define sz_xHPGetDeviceModifierMappingReq 	  8
#define sz_xHPGetDeviceModifierMappingReply 	 32
#define sz_xHPSetDeviceModifierMappingReq 	 12
#define sz_xHPSetDeviceModifierMappingReply 	 32
#define sz_xHPGetDeviceControlReq 		  8
#define sz_xHPGetDeviceControlReply 		 64
#define sz_xHPChangeDeviceControlReq 		 12
#define sz_xHPGetWindowCursorReq		  8
#define sz_xHPGetWindowCursorReply		 32
#define sz_xHPGrabResetReq			  4
#define sz_xHPGrabResetReply			 32
#define sz_xHPSendDdxDriverMsgReq		 16
#define sz_xHPSendDdxDriverMsgReply		 32
#define sz_xHPGetClipListsReq			 16
#define sz_xHPGetClipListsReply			 32

#define GET_HPINPUTDEVICE(pDev) \
    ((HPInputDevice *) ((pDev)->public.devicePrivate))

struct dev_select_info
    {
    Mask	mask;
    long	type;
    };

/*********************************************************
 *
 * Protocol request and reply structures.
 *
 */

typedef struct {
    CARD8 reqType;          /* always HpReqCode */
    CARD8 hpReqType;        /* always X_HPListInputDevices */
    CARD16 length;
} xHPListInputDevicesReq;

typedef struct {
    CARD8 repType;  		/* X_Reply */
    CARD8 hpRepType;        	/* always X_HPListInputDevices  */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD32 ndevices B32;
    CARD32 t_axes B32;
    CARD32 data02 B32;
    CARD32 data03 B32;
    CARD32 data04 B32;
    CARD32 data05 B32;
    } xHPListInputDevicesReply;


typedef struct {
    CARD8 reqType;          /* always HpReqCode */
    CARD8 hpReqType;        /* always X_HPSetInputDevice */
    CARD16 length;
    XID    deviceid;
    CARD32 mode;
} xHPSetInputDeviceReq;

typedef struct {
    CARD8 repType;  		/* X_Reply */
    CARD8 hpRepType;        	/* always X_HPSetInputDevice  */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD32 status B32;
    CARD32 data01 B32;
    CARD32 data02 B32;
    CARD32 data03 B32;
    CARD32 data04 B32;
    CARD32 data05 B32;
    } xHPSetInputDeviceReply;


typedef struct {
    CARD8 reqType;          /* always HpReqCode */
    CARD8 hpReqType;        /* always X_HPGetExtEventMask */
    CARD16 length;
    CARD32 evconst;
} xHPGetExtEventMaskReq;

typedef struct {
    CARD8 repType;  		/* X_Reply */
    CARD8 hpRepType;        	/* always X_HPGetExtEventMask  */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD32 mask   B32;
    CARD32 evtype B32;
    CARD32 data01 B32;
    CARD32 data02 B32;
    CARD32 data03 B32;
    CARD32 data04 B32;
    } xHPGetExtEventMaskReply;

typedef struct {
    CARD8 reqType;          /* always HpeqCode */
    CARD8 hpReqType;        /* always X_HPGetCurrentDeviceMask */
    CARD16 length;
    Window window B32;
    XID    deviceid;
} xHPGetCurrentDeviceMaskReq;

typedef struct {
    CARD8 repType;  		/* X_Reply */
    CARD8 hpRepType;        	/* always X_HPGetCurrentDeviceMask  */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD32 mask   B32;
    CARD32 data01 B32;
    CARD32 data02 B32;
    CARD32 data03 B32;
    CARD32 data04 B32;
    CARD32 data05 B32;
    } xHPGetCurrentDeviceMaskReply;


typedef struct {
    CARD8 reqType;          /* always HpReqCode */
    CARD8 hpReqType;        /* always X_HPGetDeviceFocus */
    CARD16 length;
    XID  deviceid;
} xHPGetDeviceFocusReq;

typedef struct {
    CARD8 repType;  		/* X_Reply */
    CARD8 hpRepType;        	/* always X_HPGetDeviceFocus  */
    CARD16 sequenceNumber B16;
    CARD32 length 	B32;
    CARD32 status 	B32;
    CARD32 focus  	B32;
    Window revertTo 	B32;
    CARD32 data01 	B32;
    CARD32 data02 	B32;
    CARD32 data03 	B32;
    } xHPGetDeviceFocusReply;

typedef struct {
    CARD8 reqType;          /* always HpReqCode */
    CARD8 hpReqType;        /* always X_HPSetDeviceFocus */
    CARD16 length;
    Window focus  	B32;
    XID    deviceid;
    Time   time 	B32;
    CARD8  revertTo;
    CARD8  pad00;
    CARD16 pad01;
} xHPSetDeviceFocusReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPGrabDevice */
    CARD16 	length B16;
    Window 	grabWindow B32;
    Time 	time B32;
    XID   	deviceid;
    CARD32 	eventMask B32;
    BOOL 	ownerEvents;
    CARD8	pad00;
    CARD16 	pad01 B16;
} xHPGrabDeviceReq;

typedef struct {
    CARD8 repType;  		/* X_Reply */
    CARD8 hpRepType;        	/* always X_HPGrabDevice  */
    CARD16 sequenceNumber B16;
    CARD32 length B32;  /* 0 */
    CARD32 status;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
    CARD32 pad7 B32;
    } xHPGrabDeviceReply;


typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPUnGrabDevice */
    CARD16 	length B16;
    Time 	time B32;
    XID   	deviceid;
} xHPUnGrabDeviceReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPSelectExtensionEvent */
    CARD16 	length B16;
    Window 	window B32;
    CARD32	extensionMask B32;
    XID		deviceid;
} xHPSelectExtensionEventReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPEnableReset          */
    CARD16 	length B16;
} xHPEnableResetReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPDisableReset          */
    CARD16 	length B16;
} xHPDisableResetReq;

typedef struct {
    CARD8 	reqType;
    CARD8 	hpReqType;        /* always X_HPGetDeviceMotionEvents*/
    CARD16 	length B16;
    Window 	window B32;
    Time 	start B32;
    Time	stop B32;
    XID		deviceid;
} xHPGetDeviceMotionEventsReq;

typedef struct {
    CARD8 repType;  		/* X_Reply */
    CARD8 hpRepType;        	/* always X_HPGetDeviceMotionEvents  */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD32 nEvents B32;
    INT16  axes B16;
    CARD16 pad2 B16;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
} xHPGetDeviceMotionEventsReply;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPGrabDeviceButton */
    CARD16 	length B16;
    Window 	grabWindow B32;
    XID   	deviceid;
    CARD32	eventMask;
    CARD16 	modifiers B16;
    BOOL 	ownerEvents;
    CARD8 	button;
    BYTE 	pointerMode, keyboardMode;
    CARD8	pad1, pad2;
} xHPGrabDeviceButtonReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPUngrabDeviceButton */
    CARD16 	length B16;
    Window 	grabWindow B32;
    XID   	deviceid;
    CARD16 	modifiers B16;
    CARD8 	button;
    CARD8 	pad1;
} xHPUngrabDeviceButtonReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPGrabDeviceKey */
    CARD16 	length B16;
    Window 	grabWindow B32;
    XID   	deviceid;
    CARD16 	modifiers B16;
    BOOL 	ownerEvents;
    CARD8	key;
    BYTE 	pointerMode, keyboardMode;  
    BYTE 	pad1, pad2;
} xHPGrabDeviceKeyReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPUngrabDeviceKey */
    CARD16 	length B16;
    Window 	grabWindow B32;
    XID   	deviceid;
    CARD16	modifiers B16;
    CARD8	key;
    CARD8	pad1;
} xHPUngrabDeviceKeyReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPDeviceAutoRepeatOn */
    CARD16 	length B16;
    XID   	deviceid;
    INT32	rate;
} xHPDeviceAutoRepeatOnReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPDeviceAutoRepeatOff */
    CARD16 	length B16;
    XID   	deviceid;
} xHPDeviceAutoRepeatOffReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPPrompt */
    CARD16 	length B16;
    XID   	deviceid;
    CARD8	prompt;
    CARD8	pad1, pad2, pad3;
} xHPPromptReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPAcknowledge */
    CARD16 	length B16;
    XID   	deviceid;
    CARD8	ack;
    CARD8	pad1, pad2, pad3;
} xHPAcknowledgeReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPGetServerMode */
    CARD16 	length B16;
    CARD32 	screen B32;
} xHPGetServerModeReq;

typedef struct {
    CARD8 repType;  		/* X_Reply */
    CARD8 hpRepType;        	/* always X_HPGetServerMode */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD32 mode B32;
    CARD32 pad1 B32;
    CARD32 pad2 B32;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
} xHPGetServerModeReply;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPGetDeviceKeyMapping */
    CARD16 	length B16;
    XID   	deviceid;
    KeyCode 	firstKeyCode; 
    CARD8 	count;
    CARD16 	pad1 B16;
} xHPGetDeviceKeyMappingReq;

typedef struct {
    CARD8  repType;  		/* X_Reply */
    CARD8  hpRepType;        	/* always X_HPGetDeviceKeyMapping */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD8  keySymsPerKeyCode;
    CARD8  pad0;
    CARD16 pad1 B16;
    CARD32 pad2 B32;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
} xHPGetDeviceKeyMappingReply;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPChangeDeviceKeyMapping */
    CARD16 	length B16;
    XID   	deviceid;
    CARD8 	keyCodes;
    KeyCode 	firstKeyCode;
    CARD8 	keySymsPerKeyCode;
    CARD8 	pad1;
} xHPChangeDeviceKeyMappingReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPGetDeviceModifierMapping */
    CARD16 	length B16;
    XID   	deviceid;
} xHPGetDeviceModifierMappingReq;

typedef struct {
    CARD8  repType;  		/* X_Reply */
    CARD8  hpRepType;        	/* always X_HPGetDeviceModifierMapping */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD8  numKeyPerModifier;
    CARD8  pad0;
    CARD16 pad1 B16;
    CARD32 pad2 B32;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
} xHPGetDeviceModifierMappingReply;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPSetDeviceModifierMapping */
    CARD16 	length B16;
    CARD8   	numKeyPerModifier;
    CARD8   	pad1;
    CARD16  	pad2 B16;
    XID   	deviceid;
} xHPSetDeviceModifierMappingReq;

typedef struct {
    CARD8  repType;  		/* X_Reply */
    CARD8  hpRepType;        	/* always X_HPSetDeviceModifierMapping */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD8  success;
    CARD8  pad0;
    CARD16 pad1 B16;
    CARD32 pad2 B32;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
} xHPSetDeviceModifierMappingReply;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPGetDeviceControl */
    CARD16 	length B16;
    XID   	deviceid;
} xHPGetDeviceControlReq;

typedef struct {
    CARD8  repType;  		/* X_Reply */
    CARD8  hpRepType;        	/* always X_HPGetDeviceControl */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    BOOL   globalAutoRepeat;
    CARD8  keyClickPercent;
    CARD8  bellPercent;
    CARD8  pad0;
    CARD32 ledMask B32;
    CARD16 bellPitch B16;
    CARD16 bellDuration B16;
    CARD16 accelNumerator B16;
    CARD16 accelDenominator B16;
    CARD16 threshold B16;
    CARD16 pad1 B16;
    CARD32 pad2 B32;
    BYTE map[32];  /* bit masks start here */
} xHPGetDeviceControlReply;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_HPChangeDeviceControl */
    CARD16 	length B16;
    XID   	deviceid;
    CARD32 	mask B32;
} xHPChangeDeviceControlReq;

typedef struct {
    CARD8	reqType;
    CARD8 	hpReqType;        /* always X_GetWindowCursor */
    CARD16 	length B16;
    Window	window B32;
} xHPGetWindowCursorReq;

typedef struct {
    CARD8  repType;  		/* X_Reply */
    CARD8  hpRepType;        	/* always X_HPGetWindowCursor */
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    Cursor cursor B32;
    CARD32 pad1 B32;
    CARD32 pad2 B32;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
} xHPGetWindowCursorReply;

typedef struct {
    CARD8 reqType;          /* always HpReqCode */
    CARD8 hpReqType;        /* always X_HPGetClipList */
    CARD16 length;
    CARD32 wid;		    /* window id */
    CARD32 gcid;	    /* graphics context id */
    CARD32 format;	    /* clip list format */
} xHPGetClipListReq;

typedef struct {
    BYTE   type;  	/* X_Reply */
    BYTE   pad0;
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    INT16  x	  B16;	/* x origin of window */
    INT16  y	  B16;	/* y origin of window */
    CARD16 width  B16;	/* width of window */
    CARD16 height B16;	/* height of window */
    CARD32 count  B32;	/* number of clipping rectanges */
    CARD32 data03 B32;
    CARD32 data04 B32;
    CARD32 data05 B32;
    } xHPGetClipListReply;

/*
 *  X_HPRegisterWindow request/reply structures...
 */

typedef struct {
    CARD8 reqType;		/* always HpReqCode */
    CARD8 hpReqType;		/* X_HPRegisterWindow or X_HPUnReg... */
    CARD16 length;
    CARD32 wid;			/* window id */
    CARD32 accelid;		/* accelerated id */
    CARD32 flags;		/* flags */
} xHPRegisterWindowReq;

typedef struct {
    BYTE   type;		/* X_Reply */
    BYTE   pad0;
    CARD16 sequenceNumber B16;
    CARD32 length	B32;
    INT32  error	B32;	/* error status */
    CARD32 flags	B32;	/* flags */
    BYTE   path[80];		/* length must >= cfbPrivScreen->path */
    CARD32 depth	B32;	/* depth of window */
    INT32  w_class	B32;	/* window GRM_ID class */
    INT32  w_screen	B32;	/* window GRM_ID screen */
    INT32  w_window	B32;	/* window GRM_ID window */
    BYTE   w_name[40];		/* window GRM_ID name */
    } xHPRegisterWindowReply;

/*
 *  X_HPSynchronizeColorRange Request structure (no reply)
 */

typedef struct {
    CARD8 reqType;		/* always HpReqCode */
    CARD8 hpReqType;		/* X_HPRegisterWindow or X_HPUnReg... */
    CARD16 length;
    CARD32 cmap;		/* Colormap ID of interest */
    CARD32 start;		/* Starting pixel if changes to cmap */
    CARD32 ncolors;		/* number of pixels changed */
} xHPSynchronizeColorRangeReq;

/*
 *  X_HPSendDdxDriverMsg Request structure (possible reply)
 */

typedef struct {
    CARD8 reqType;		/* always HpReqCode */
    CARD8 hpReqType;		/* X_HPRegisterWindow or X_HPUnReg... */
    CARD16 length;
    Window window;		/* window of interest */
    INT32 nMsgBytes;		/* Number of bytes in message */
    INT32 needReply;		/* whether or not a reply is forthcoming */
} xHPSendDdxDriverMsgReq;

typedef struct {
    CARD8 type;		
    CARD8 pad0;		
    CARD16 sequenceNumber B16;
    CARD32 length	B32;
    INT32 nRepBytes;
    CARD32 pad1		B32;
    CARD32 pad2		B32;
    CARD32 pad3		B32;
    CARD32 pad4		B32;
    CARD32 pad5		B32;
    } xHPSendDdxDriverMsgReply;

/*
 * XHPGetClipLists request (not to be confused with XHPGetClipList.
 * This extension gets multiple clip lists)
 */

typedef struct {
    CARD8 reqType;          /* always HpReqCode */
    CARD8 hpReqType;        /* always X_HPGetClipLists */
    CARD16 length;
    CARD32 wid;             /* window id */
    CARD32 gcid;            /* graphics context id */
    CARD32 nClipLists;      /* number of clip lists requested */
/* To be followed by 'nClipLists' CARD32 format longwords: */
} xHPGetClipListsReq;


typedef struct {
    BYTE   type;        /* X_Reply */
    BYTE   pad0;
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    INT16  x      B16;  /* x origin of window */
    INT16  y      B16;  /* y origin of window */
    CARD16 width  B16;  /* width of window */
    CARD16 height B16;  /* height of window */
    CARD32 count  B32;  /* number of clip lists */
    CARD32 pad1		B32;
    CARD32 pad2		B32;
    CARD32 pad3		B32;
/* To be followed by 'count' xHPGetClipListsReplyData structures: */
} xHPGetClipListsReply;

typedef struct {
    CARD16 count  B16;  /* number of clip boxes */
    CARD16 size   B16;  /* size of each clip box in bytes */
/* To be followed by 'count * size' bytes: */
} xHPGetClipListsReplyData;


/*  X_HPGrabReset Request structure */

typedef struct {
    CARD8 reqType;		/* always HpReqCode */
    CARD8 hpReqType;		/* X_HPGrabReset */
    CARD16 length;
} xHPGrabResetReq;

typedef struct {
    CARD8 repType;		/* always HpReqCode */
    CARD8 hpRepType;		/* X_HPGrabReset */
    CARD16 sequenceNumber B16;
    CARD32 length	B32;
    Atom   evtype	B32;	/* event type to expect */
    CARD32 pad1		B32;
    CARD32 pad2		B32;
    CARD32 pad3		B32;
    CARD32 pad4		B32;
    CARD32 pad5		B32;
    } xHPGrabResetReply;

/*  X_HPSSChange Request structures
 *  Notes:
 *    The sz_x numbers are sizeof(struct) padded to the to 4 bytes.  This to
 *    make the X packets work correctly.
 */

typedef struct {
    CARD8 reqType;		/* always HpReqCode */
    CARD8 hpReqType;		/* X_HPSSChange */
    CARD16 length;
    CARD32 flags;		/* stuff */
    CARD32 wid;			/* just in case */
} xHPSSChangeReq;

#define sz_xHPSSChangeReq	12

typedef struct {
    CARD8 repType;		/* always HpReqCode */
    CARD8 hpRepType;		/* X_HPSSChange */
    CARD16 sequenceNumber B16;
    CARD32 length	B32;
    Atom   evtype	B32;	/* event type to expect */
    CARD32 flags	B32;	/*  */
    CARD32 pad2		B32;
    CARD32 pad3		B32;
    CARD32 pad4		B32;
    CARD32 pad5		B32;
    } xHPSSChangeReply;

#define sz_xHPSSChangeReply	32

/**********************************************************
 *
 * extension events.
 *
 */

typedef struct 
    {
    INT16	ax_num;
    INT16	ax_val;
    } XHPaxis_data;

typedef struct
    {
    BYTE 	type;
    BYTE 	count;
    CARD16 	sequencenumber B16;
    XID    	deviceid;
    CARD32	pad00 B32;
    CARD32 	pad01 B32;
    CARD32 	pad02 B32;
    CARD32 	pad03 B32;
    CARD32 	pad04 B32;
    CARD32 	pad05 B32;
    }  xAnyExtensionEvent;

typedef struct
    {
    BYTE 	type;
    BYTE 	ext_type;
    CARD16 	sequencenumber B16;
    XID    	deviceid;
    INT16  	axes_count B16;
    CARD16 	pad00 B16;
    XHPaxis_data data[4];
    CARD32 	pad01 B32;
    }  xHPExtensionEvent;

typedef struct
    {
    xEvent		b;
    xAnyExtensionEvent	x;
    } xAnyEvent;

typedef struct
    {
    xEvent		b;
#ifdef XINPUT
    deviceValuator	x;
#else
    xHPExtensionEvent	x;
#endif /* XINPUT */
    }  xHPEvent;

typedef	struct 
    {
    BYTE type;
    BYTE pad00;
    CARD16 sequencenumber B16;
    INT16 detail B16;
    BYTE mode; 			/* really XMode */
    BYTE pad1;
    XID   deviceid;
    Window window B32;
    CARD32 pad01 B32;
    CARD32 pad02 B32;
    CARD32 pad03 B32;
    CARD32 pad04 B32;
    } xHPdevicefocus;

typedef	struct 
    {
    BYTE type;
    BYTE pad00;
    CARD16 sequencenumber B16;
    CARD8 request;
    KeyCode firstKeyCode;
    CARD8 count;
    BYTE pad1;
    XID   deviceid;
    CARD32 pad01 B32;
    CARD32 pad02 B32;
    CARD32 pad03 B32;
    CARD32 pad04 B32;
    CARD32 pad05 B32;
    } xHPDeviceMappingEvent;

typedef	struct 
    {
    BYTE type;
    BYTE deviceid;
    BYTE map[30];
    } xHPDeviceKeymapEvent;

#endif
