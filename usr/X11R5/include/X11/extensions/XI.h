/* $Header: XI.h,v 1.13 91/07/24 15:32:36 rws Exp $ */

/************************************************************
Copyright (c) 1989 by Hewlett-Packard Company, Palo Alto, California, and the 
Massachusetts Institute of Technology, Cambridge, Massachusetts.

			All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Hewlett-Packard or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
HEWLETT-PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************/

/* Definitions used by the server, library and client */

#ifndef _XI_H_

#define _XI_H_

#define sz_xGetExtensionVersionReq		8
#define sz_xGetExtensionVersionReply		32
#define sz_xListInputDevicesReq			4
#define sz_xListInputDevicesReply		32
#define sz_xOpenDeviceReq			8
#define sz_xOpenDeviceReply			32
#define sz_xCloseDeviceReq			8
#define sz_xSetDeviceModeReq			8
#define sz_xSetDeviceModeReply			32
#define sz_xSelectExtensionEventReq		12
#define sz_xGetSelectedExtensionEventsReq	8
#define sz_xGetSelectedExtensionEventsReply	32
#define sz_xChangeDeviceDontPropagateListReq	12
#define sz_xGetDeviceDontPropagateListReq	8
#define sz_xGetDeviceDontPropagateListReply	32
#define sz_xGetDeviceMotionEventsReq		16
#define sz_xGetDeviceMotionEventsReply		32
#define sz_xChangeKeyboardDeviceReq		8
#define sz_xChangeKeyboardDeviceReply		32
#define sz_xChangePointerDeviceReq		8
#define sz_xChangePointerDeviceReply		32
#define sz_xGrabDeviceReq			20
#define sz_xGrabDeviceReply			32
#define sz_xUngrabDeviceReq			12
#define sz_xGrabDeviceKeyReq			20
#define sz_xGrabDeviceKeyReply			32
#define sz_xUngrabDeviceKeyReq			16
#define sz_xGrabDeviceButtonReq			20
#define sz_xGrabDeviceButtonReply		32
#define sz_xUngrabDeviceButtonReq		16
#define sz_xAllowDeviceEventsReq		12
#define sz_xGetDeviceFocusReq			8
#define sz_xGetDeviceFocusReply			32
#define sz_xSetDeviceFocusReq			16
#define sz_xGetFeedbackControlReq		8
#define sz_xGetFeedbackControlReply		32
#define sz_xChangeFeedbackControlReq		12
#define sz_xGetDeviceKeyMappingReq		8
#define sz_xGetDeviceKeyMappingReply		32
#define sz_xChangeDeviceKeyMappingReq		8
#define sz_xGetDeviceModifierMappingReq		8
#define sz_xSetDeviceModifierMappingReq		8
#define sz_xSetDeviceModifierMappingReply	32
#define sz_xGetDeviceButtonMappingReq		8
#define sz_xGetDeviceButtonMappingReply		32
#define sz_xSetDeviceButtonMappingReq		8
#define sz_xSetDeviceButtonMappingReply		32
#define sz_xQueryDeviceStateReq			8
#define sz_xQueryDeviceStateReply		32
#define sz_xSendExtensionEventReq		16
#define sz_xDeviceBellReq			8
#define sz_xSetDeviceValuatorsReq		8
#define sz_xSetDeviceValuatorsReply		32
#define sz_xGetDeviceControlReq			8
#define sz_xGetDeviceControlReply		32
#define sz_xChangeDeviceControlReq		8
#define sz_xChangeDeviceControlReply		32

#define INAME 			"XInputExtension"

#define XI_KEYBOARD	"KEYBOARD"
#define XI_MOUSE	"MOUSE"
#define XI_TABLET	"TABLET"
#define XI_TOUCHSCREEN	"TOUCHSCREEN"
#define XI_TOUCHPAD	"TOUCHPAD"
#define XI_BARCODE	"BARCODE"
#define XI_BUTTONBOX	"BUTTONBOX"
#define XI_KNOB_BOX	"KNOB_BOX"
#define XI_ONE_KNOB	"ONE_KNOB"
#define XI_NINE_KNOB	"NINE_KNOB"
#define XI_TRACKBALL	"TRACKBALL"
#define XI_QUADRATURE	"QUADRATURE"
#define XI_ID_MODULE	"ID_MODULE"
#define XI_SPACEBALL	"SPACEBALL"
#define XI_DATAGLOVE	"DATAGLOVE"
#define XI_EYETRACKER	"EYETRACKER"
#define XI_CURSORKEYS	"CURSORKEYS"
#define XI_FOOTMOUSE	"FOOTMOUSE"

#define Dont_Check			0
#define XInput_Initial_Release		1
#define XInput_Add_XDeviceBell		2
#define XInput_Add_XSetDeviceValuators	3
#define XInput_Add_XChangeDeviceControl	4

#define XI_Absent		0
#define XI_Present		1

#define XI_Initial_Release_Major		1
#define XI_Initial_Release_Minor		0

#define XI_Add_XDeviceBell_Major		1
#define XI_Add_XDeviceBell_Minor		1

#define XI_Add_XSetDeviceValuators_Major	1
#define XI_Add_XSetDeviceValuators_Minor	2

#define XI_Add_XChangeDeviceControl_Major	1
#define XI_Add_XChangeDeviceControl_Minor	3

#define DEVICE_RESOLUTION	1

#define NoSuchExtension		1

#define COUNT			0
#define CREATE			1

#define NewPointer		0
#define NewKeyboard		1

#define XPOINTER		0
#define XKEYBOARD		1

#define UseXKeyboard		0xFF

#define IsXPointer		0
#define IsXKeyboard		1
#define IsXExtensionDevice	2

#define AsyncThisDevice		0
#define SyncThisDevice		1
#define ReplayThisDevice	2
#define AsyncOtherDevices	3
#define AsyncAll		4
#define SyncAll			5

#define FollowKeyboard 		3
#ifndef RevertToFollowKeyboard
#define RevertToFollowKeyboard 	3
#endif

#define DvAccelNum              (1L << 0)
#define DvAccelDenom            (1L << 1)
#define DvThreshold             (1L << 2)

#define DvKeyClickPercent	(1L<<0)
#define DvPercent		(1L<<1)
#define DvPitch			(1L<<2)
#define DvDuration		(1L<<3)
#define DvLed			(1L<<4)
#define DvLedMode		(1L<<5)
#define DvKey			(1L<<6)
#define DvAutoRepeatMode	(1L<<7)

#define DvString                (1L << 0)

#define DvInteger               (1L << 0)

#define DeviceMode              (1L << 0)
#define Relative                0
#define Absolute                1

#define ProximityState          (1L << 1)
#define InProximity             (0L << 1)
#define OutOfProximity          (1L << 1)

#define AddToList               0
#define DeleteFromList          1

#define KeyClass  		0
#define ButtonClass  		1
#define ValuatorClass  		2
#define FeedbackClass  		3
#define ProximityClass  	4
#define FocusClass  		5
#define OtherClass  		6

#define KbdFeedbackClass  	0
#define PtrFeedbackClass  	1
#define StringFeedbackClass  	2
#define IntegerFeedbackClass  	3
#define LedFeedbackClass  	4
#define BellFeedbackClass  	5

#define _devicePointerMotionHint 0
#define _deviceButton1Motion	 1
#define _deviceButton2Motion	 2
#define _deviceButton3Motion	 3
#define _deviceButton4Motion	 4
#define _deviceButton5Motion	 5
#define _deviceButtonMotion	 6
#define _deviceButtonGrab	 7
#define _deviceOwnerGrabButton	 8
#define _noExtensionEvent	 9

#define XI_BadDevice	0
#define XI_BadEvent	1
#define XI_BadMode	2
#define XI_DeviceBusy	3
#define XI_BadClass	4

typedef	unsigned long	XEventClass;

/*******************************************************************
 *
 * Extension version structure.
 *
 */

typedef struct {
        int   	present;
        short	major_version;
        short	minor_version;
} XExtensionVersion;

#endif /* _XI_H_ */
