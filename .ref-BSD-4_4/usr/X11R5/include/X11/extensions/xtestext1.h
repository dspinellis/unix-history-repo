/*
 * xtestext1.h
 *
 * X11 Input Synthesis Extension include file
 */

/*

Copyright 1986, 1987, 1988 by Hewlett-Packard Corporation
Copyright 1986, 1987, 1988 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

Hewlett-Packard and M.I.T. make no representations about the 
suitability of this software for any purpose.  It is provided 
"as is" without express or implied warranty.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/

/*
 * the typedefs for CARD8, CARD16, and CARD32 are defined in Xmd.h
 */

/*
 * used in the XTestPressButton and XTestPressKey functions
 */
#define XTestPRESS                      1 << 0
#define XTestRELEASE                    1 << 1
#define XTestSTROKE                     1 << 2

/*
 * When doing a key or button stroke, the number of milliseconds
 * to delay between the press and the release of a key or button
 * in the XTestPressButton and XTestPressKey functions.
 */

#define XTestSTROKE_DELAY_TIME		10

/*
 * used in the XTestGetInput function
 */
#define XTestEXCLUSIVE                  1 << 0
#define XTestPACKED_ACTIONS             1 << 1
#define XTestPACKED_MOTION              1 << 2

/*
 * used in the XTestFakeInput function
 */
#define XTestFAKE_ACK_NOT_NEEDED        0
#define XTestFAKE_ACK_REQUEST           1

/*
 * used in the XTest extension initialization routine
 */
#define XTestEXTENSION_NAME             "XTestExtension1"
#define XTestEVENT_COUNT                2

/*
 * XTest request type values 
 *
 * used in the XTest extension protocol requests
 */
#define X_TestFakeInput                  1
#define X_TestGetInput                   2
#define X_TestStopInput                  3
#define X_TestReset                      4
#define X_TestQueryInputSize             5

/*
 * This defines the maximum size of a list of input actions
 * to be sent to the server.  It should always be a multiple of
 * 4 so that the entire xTestFakeInputReq structure size is a
 * multiple of 4.
 */
#define XTestMAX_ACTION_LIST_SIZE       64

typedef struct {
        CARD8   reqType;        /* always XTestReqCode             */
        CARD8   XTestReqType;   /* always X_TestFakeInput           */
        CARD16  length B16;     /* 2 + XTestMAX_ACTION_LIST_SIZE/4 */
        CARD32  ack B32;
        CARD8   action_list[XTestMAX_ACTION_LIST_SIZE];
} xTestFakeInputReq;
#define sz_xTestFakeInputReq (XTestMAX_ACTION_LIST_SIZE + 8)

typedef struct {
        CARD8   reqType;        /* always XTestReqCode  */
        CARD8   XTestReqType;   /* always X_TestGetInput */
        CARD16  length B16;     /* 2                    */
        CARD32  mode B32;
} xTestGetInputReq;
#define sz_xTestGetInputReq 8

typedef struct {
        CARD8   reqType;        /* always XTestReqCode   */
        CARD8   XTestReqType;   /* always X_TestStopInput */
        CARD16  length B32;     /* 1                     */
} xTestStopInputReq;
#define sz_xTestStopInputReq 4

typedef struct {
        CARD8   reqType;        /* always XTestReqCode */
        CARD8   XTestReqType;   /* always X_TestReset   */
        CARD16  length B16;     /* 1                   */
} xTestResetReq;
#define sz_xTestResetReq 4

typedef struct {
        CARD8   reqType;        /* always XTestReqCode        */
        CARD8   XTestReqType;   /* always X_TestQueryInputSize */
        CARD16  length B16;     /* 1                          */
} xTestQueryInputSizeReq;
#define sz_xTestQueryInputSizeReq 4

/*
 * This is the definition of the reply for the xTestQueryInputSize
 * request.  It should remain the same minimum size as other replies
 * (32 bytes).
 */
typedef struct {
        CARD8   type;           /* always X_Reply  */
        CARD8   pad1;
        CARD16  sequenceNumber B16;
        CARD32  length B32;     /* always 0 */
        CARD32  size_return B32;
        CARD32  pad2 B32;
        CARD32  pad3 B32;
        CARD32  pad4 B32;
        CARD32  pad5 B32;
        CARD32  pad6 B32;
} xTestQueryInputSizeReply;

/*
 * This is the definition for the input action wire event structure.
 * This event is sent to the client when the server has one or
 * more user input actions to report to the client.  It must
 * remain the same size as all other wire events (32 bytes).
 */
#define XTestACTIONS_SIZE	28

typedef struct {
        CARD8   type;           /* always XTestInputActionType */
        CARD8   pad00;
        CARD16  sequenceNumber B16;
        CARD8   actions[XTestACTIONS_SIZE];
} xTestInputActionEvent;

/*
 * This is the definition for the xTestFakeAck wire event structure.
 * This event is sent to the client when the server has completely
 * processed its input action buffer, and is ready for more.
 * It must remain the same size as all other wire events (32 bytes).
 */
typedef struct {
        CARD8   type;           /* always XTestFakeAckType */
        CARD8   pad00;
        CARD16  sequenceNumber B16;
        CARD32  pad02 B32;
        CARD32  pad03 B32;
        CARD32  pad04 B32;
        CARD32  pad05 B32;
        CARD32  pad06 B32;
        CARD32  pad07 B32;
        CARD32  pad08 B32;
} xTestFakeAckEvent;

/*
 * The server side of this extension does not (and should not) have
 * definitions for Display and Window.  The ifndef allows the server
 * side of the extension to ignore the following typedefs.
 */
#ifndef XTestSERVER_SIDE
/*
 * This is the definition for the input action host format event structure.
 * This is the form that a client using this extension will see when
 * it receives an input action event.
 */
typedef struct {
        int     type;           /* always XTestInputActionType */
	Display *display;
	Window  window;
        CARD8   actions[XTestACTIONS_SIZE];
} XTestInputActionEvent;

/*
 * This is the definition for the xTestFakeAck host format event structure.
 * This is the form that a client using this extension will see when
 * it receives an XTestFakeAck event.
 */
typedef struct {
        int     type;           /* always XTestFakeAckType */
	Display *display;
	Window  window;
} XTestFakeAckEvent;
#endif

/*
 * This is the definition for the format of the header byte
 * in the input action structures.
 */
#define XTestACTION_TYPE_MASK   0x03    /* bits 0 and 1          */
#define XTestKEY_STATE_MASK     0x04    /* bit 2 (key action)    */
#define XTestX_SIGN_BIT_MASK    0x04    /* bit 2 (motion action) */
#define XTestY_SIGN_BIT_MASK    0x08    /* bit 3 (motion action) */
#define XTestDEVICE_ID_MASK     0xf0    /* bits 4 through 7      */

#define XTestMAX_DEVICE_ID	0x0f
#define XTestPackDeviceID(x)	(((x) & XTestMAX_DEVICE_ID) << 4)
#define XTestUnpackDeviceID(x)	(((x) & XTestDEVICE_ID_MASK) >> 4)

/*
 * These are the possible action types.
 */
#define XTestDELAY_ACTION       0
#define XTestKEY_ACTION         1
#define XTestMOTION_ACTION      2
#define XTestJUMP_ACTION        3

/*
 * These are the definitions for key/button motion input actions.
 */
#define XTestKEY_UP             0x04
#define XTestKEY_DOWN           0x00

typedef struct {
        CARD8   header;         /* which device, key up/down */
        CARD8   keycode;        /* which key/button to move  */
        CARD16  delay_time B16; /* how long to delay (in ms) */
} XTestKeyInfo;

/*
 * This is the definition for pointer jump input actions.
 */
typedef struct {
        CARD8   header;         /* which pointer             */
        CARD8   pad1;           /* unused padding byte       */
        CARD16  jumpx B16;      /* x coord to jump to        */
        CARD16  jumpy B16;      /* y coord to jump to        */
        CARD16  delay_time B16; /* how long to delay (in ms) */
} XTestJumpInfo;

/*
 * These are the definitions for pointer relative motion input
 * actions.
 *
 * The sign bits for the x and y relative motions are contained
 * in the header byte.  The x and y relative motions are packed
 * into one byte to make things fit in 32 bits.  If the relative
 * motion range is larger than +/-15, use the pointer jump action.
 */
#define XTestMOTION_MAX            15
#define XTestMOTION_MIN            -15

#define XTestX_NEGATIVE            0x04
#define XTestY_NEGATIVE            0x08

#define XTestX_MOTION_MASK         0x0f
#define XTestY_MOTION_MASK         0xf0

#define XTestPackXMotionValue(x)   ((x) & XTestX_MOTION_MASK)
#define XTestPackYMotionValue(x)   (((x) << 4) & XTestY_MOTION_MASK)

#define XTestUnpackXMotionValue(x) ((x) & XTestX_MOTION_MASK)
#define XTestUnpackYMotionValue(x) (((x) & XTestY_MOTION_MASK) >> 4)

typedef struct {
        CARD8   header;         /* which pointer             */
        CARD8   motion_data;    /* x,y relative motion       */
        CARD16  delay_time B16; /* how long to delay (in ms) */
} XTestMotionInfo;

/*
 * These are the definitions for a long delay input action.  It is 
 * used when more than XTestSHORT_DELAY_TIME milliseconds of delay
 * (approximately one minute) is needed.
 *
 * The device ID for a delay is always set to XTestDELAY_DEVICE_ID.
 * This guarantees that a header byte with a value of 0 is not
 * a valid header, so it can be used as a flag to indicate that
 * there are no more input actions in an XTestInputAction event.
 */

#define XTestSHORT_DELAY_TIME	0xffff
#define XTestDELAY_DEVICE_ID    0x0f   

typedef struct {
        CARD8   header;         /* always XTestDELAY_DEVICE_ID */
        CARD8   pad1;           /* unused padding byte         */
        CARD16  pad2 B16;       /* unused padding word         */
        CARD32  delay_time B32; /* how long to delay (in ms)   */
} XTestDelayInfo;
