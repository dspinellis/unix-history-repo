#ifndef XHPLIB_H
#define XHPLIB_H
/* $Header: /host/kaukau/disk2/X11R5/R5-hp300/mit/server/ddx/hpbsd/input/X11/RCS/XHPlib.h,v 1.1 1992/09/30 03:15:29 root Exp $ */

/* Definitions used by Xlib and the client */

#include "XHPproto.h"
#ifndef _NO_PROTO
#if    !defined(__STDC__) && !defined(__cplusplus) && !defined(c_plusplus)
#define _NO_PROTO
#endif /* __STDC__ */
#endif /* _NO_PROTO */


#ifndef _XLIB_H_
#include <X11/Xlib.h>
#endif
#ifndef _XUTIL_H_
#include <X11/Xutil.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define GENERAL_PROMPT			0
#define PROMPT_1			1
#define PROMPT_2			2
#define PROMPT_3			3
#define PROMPT_4			4
#define PROMPT_5			5
#define PROMPT_6			6
#define PROMPT_7			7

#define GENERAL_ACKNOWLEDGE		0
#define ACKNOWLEDGE_1			1
#define ACKNOWLEDGE_2			2
#define ACKNOWLEDGE_3			3
#define ACKNOWLEDGE_4			4
#define ACKNOWLEDGE_5			5
#define ACKNOWLEDGE_6			6
#define ACKNOWLEDGE_7			7

#define HPDeviceKeyPressreq		1
#define HPDeviceKeyReleasereq		2
#define HPDeviceButtonPressreq		3
#define HPDeviceButtonReleasereq	4
#define HPDeviceMotionNotifyreq		5
#define HPDeviceFocusInreq		6
#define HPDeviceFocusOutreq		7
#define HPProximityInreq		8
#define HPProximityOutreq		9
#define HPDeviceKeymapNotifyreq		10
#define HPDeviceMappingNotifyreq	11

extern	int	HPDeviceKeyPress;
extern	int	HPDeviceKeyRelease;
extern	int	HPDeviceButtonPress;
extern	int	HPDeviceButtonRelease;
extern	int	HPDeviceMotionNotify;
extern	int	HPDeviceFocusIn;
extern	int	HPDeviceFocusOut;
extern	int	HPProximityIn;
extern	int	HPProximityOut;
extern	int	HPDeviceKeymapNotify;
extern	int	HPDeviceMappingNotify;

typedef	int	(*PtrFuncInt) ();

typedef unsigned long	XHPFilterId;

/* structure used to split events queue between drivers and client */

typedef struct    _XHProutines	*_XHPrtnptr;

typedef struct _XHProutines
    {
    Display	*display;
    XHPFilterId	id;
    Window	window;
    Mask	std_filtermask;
    Mask	std_clientmask;
    Mask	ext_filtermask[MAX_LOGICAL_DEVS];
    Mask	ext_clientmask[MAX_LOGICAL_DEVS];
    int		(*callback) ();
    int		state_info;
    _XHPrtnptr	next;
    } XHProutines;

typedef struct 
    {
    XKeyEvent	ev;
    XID		deviceid;
    } XHPDeviceKeyEvent;

/***************************************************************
 *
 * The location of the X pointer is reported in the coordinate
 * fields of the ev member.  The location of the device
 * is determined from the previous DeviceMotionNotify event.
 *
 */

typedef struct {
    XButtonEvent	ev;
    XID			deviceid;
    } XHPDeviceButtonEvent;

/***************************************************************
 *
 * The ax_num and ax_val fields contain the data reported by the
 * device.  The values may be absolute or relative.  Any axis
 * whose value changes will be reported.
 *
 */

typedef struct 
    {
    int           	ax_num;
    int			ax_val;
    } XHPAxis_data;

/****************************************************************************
 *
 * Bug fix for alignment problem on s700/ s800.
 *
 * XHPDeviceMotionEvent embeds an XMotionEvent struct.  The XMotionEvent
 * struct contains a char followed by an int.  68k CPUs add one byte of 
 * padding to align the int on a 16-bit boundary.  PA-RISC CPUs add three
 * bytes of padding to align the int on a 32-bit boudary.  The result is 
 * that XMotionEvent structs are 58 bytes on 68k CPUs and 60 bytes on
 * PA-RISC CPUs.  
 *
 * The size is critical because the XHPScreen_events routine assumes that all
 * HP input extension events contain a device id in bytes 60 - 63.
 *
 * The right way to fix this would be to define a 60-byte
 * array and make it a union with the ev field, but this would break existing
 * clients that reference this field.
 *
 * Instead we will ifdef the struct to make the padding come out right.
 * A side effect of this is that on machines with 32-bit alignment, there's
 * only room for 3 elements in the data array field, since the total XEvent
 * size is 96 bytes.  This is probably ok, since no HP input devices report more
 * than 3 axes of motion.  We will leave the s300 definition at 4 elements,
 * since it was originally defined that way.  We will also put in code to
 * cause a compiler error for undefined machines.
 *
 *  Mea culpa,   ---gms
 */

typedef struct 
    {
#if defined(__hp9000s300) || defined(__apollo)	|| defined(hp300) /* 68k aligns to 16 bits */
    XMotionEvent	ev;		
    char		pad;	
    unsigned char	axes_count;
    XID			deviceid;	
    XHPAxis_data	data[4];
#else 
#if defined(__hp9000s800) || defined(__hp9000s700)	/* 32-bit alignment */
    XMotionEvent	ev;
    XID			deviceid;
    XHPAxis_data	data[3];
    unsigned char	axes_count;
#else
#if defined(__hp_osf) && defined(__mc68000)
    XMotionEvent	ev;		
    char		pad;	
    unsigned char	axes_count;
    XID			deviceid;	
    XHPAxis_data	data[4];
#else 
#if defined(__hp_osf) && defined(__pa_risc)
    XMotionEvent	ev;
    XID			deviceid;
    XHPAxis_data	data[3];
    unsigned char	axes_count;
#else 
This is a bogus line to force a compiler error on undefined machines - gms.
#endif
#endif
#endif
#endif
    } XHPDeviceMotionEvent;

typedef struct 
    {
    XFocusChangeEvent	ev;
    char		pad[32];
    XID			deviceid;
    char		pad1[2];
    } XHPDeviceFocusChangeEvent;

typedef struct 
    {
    XMappingEvent	ev;
    char		pad[28];
    XID			deviceid;
    char		pad1[2];
    } XHPDeviceMappingEvent;

typedef struct 
    {
    int type;			/* ProximityIn or ProximityOut */
    unsigned long serial;	/* # of last request processed by server */
    Bool send_event;		/* true if this came from a SendEvent request */
    Display *display;		/* Display the event was read from */
    Window window;		/* window of event */
    Window root;	        /* root window that the event occured on */
    Window subwindow;		/* child window */
    Time time;			/* milliseconds */
    int x, y;			/* pointer x, y coordinates in event window */
    int x_root, y_root;		/* coordinates relative to root */
    unsigned int state;		/* key or button mask */
    Bool same_screen;		/* same screen flag */
    char pad[4];
    XID	deviceid;
    char pad1[2];
    } XHPProximityNotifyEvent;
typedef XHPProximityNotifyEvent XHPProximityInEvent;
typedef XHPProximityNotifyEvent XHPProximityOutEvent;

/* generated on EnterWindow and FocusIn  when KeyMapState selected */
typedef struct {
    int type;
    unsigned long serial;	/* # of last request processed by server */
    Bool send_event;		/* true if this came from a SendEvent request */
    Display *display;		/* Display the event was read from */
    Window window;
    char key_vector[32];
    char pad[8];
    XID	deviceid;
} XHPDeviceKeymapEvent;	

typedef struct 
    {
    char pad[60];
    XID	deviceid;
    } XHP_AnyEvent;

typedef struct 
    {
    unsigned int  	resolution;
    unsigned short	min_val;
    unsigned short	max_val;
    } XHPaxis_info;

typedef struct 
    {
    XID			x_id;
    char		*name;
    XHPaxis_info	*axes;
    unsigned short	type;
    unsigned short	min_keycode;
    unsigned short	max_keycode;
    unsigned char	hil_id;
    unsigned char	mode;
    unsigned char	num_axes;
    unsigned char	num_buttons;
    unsigned char	num_keys;
    unsigned char	io_byte;
    unsigned short	detailed_id;
    unsigned char	pad[6];
    } XHPDeviceList;

typedef struct {
        int key_click_percent;
        int bell_percent;
        int bell_pitch;
        int bell_duration;
        int led;
        int led_mode;
        int key;
        int auto_repeat_mode;   /* On, Off, Default */
        int accelNumerator;
        int accelDenominator;
        int threshold;
} XHPDeviceControl;

/* Data structure for XHPGetDeviceControl */

typedef struct {
        int key_click_percent;
	int bell_percent;
	unsigned int bell_pitch, bell_duration;
	unsigned long led_mask;
	int global_auto_repeat;
	int accelNumerator;
	int accelDenominator;
	int threshold;
	char auto_repeats[32];
} XHPDeviceState;

typedef struct {
        Time time;
	unsigned short *data;
} XHPTimeCoord;


/* This structure is used to pass Nlio ioctl style commands
 * to the Nlio server */

#define XNHPNlioctl "HPNlioctl"

typedef struct {
    int cmd;
    XPointer arg;
    int ret;
} XhpNlioCmd;

/* These are the structures used for the XHPNlioctl call */

typedef struct {
    int flags;
    KeySym invoke_nlio;
    KeySym terminate_nlio;
    KeySym set_alternate;
    KeySym unset_alternate;
} K16_state;

/* These are the commands for the XHPNlioctl call */
#include <sys/ioctl.h>
#define K16_FOCUS_IN    _IO('X', 100)
#define K16_FOCUS_OUT   _IO('X', 101)

#define K16_EXEC_PROC	1
#define K16_KILL_PROC	2
#define K16_GET_STATEKEYS 3
#define K16_SET_STATEKEYS 4
#define K16_ALT_ON 5
#define K16_NLIO_ON 6
#define K16_IS_PROC 7
#define K16_GET_ENCODING 8
#define K16_SET_ENCODING 9

#define K16_NLIOSTATE  1
#define K16_ALTSTATE   2

/* The following block of defines are required for XHPSetKeyboardMapping */

#define DEF_FILENAME	"XHPKeymaps"
#ifndef DEF_DIRECTORY
#define DEF_DIRECTORY	"/usr/lib/X11/"
#endif
#define VERIFY_MAGIC	"HPKeyMap Rev 1.0"

#define MAGIC_SIZE 20

struct XHP_keymap_header {
    int kbd;
    int offset;
    int size;
};

#define HPK_KEYDEVICE_NAME_TABLE_ID	19998
#define HPK_MODMAP_TABLE_ID		19999
#define HPK_FIRST_RESERVED_ID		20000

typedef struct
{
  int keydevice_id, min_keycode, max_keycode, columns;
  char *name, *modmap_name;
} HPKKeyDeviceInfo;

#define MODMAP_SIZE 256		/* aka MAP_LENGTH in server/include/input.h */

typedef struct
{
  char *modmap_name;
  CARD8 modmap[MODMAP_SIZE];
} HPKModMap;


/* Error values returned by XHPSetKeyboardMapping                            */

#define XHPKB_NOKEYFILE 	1
#define XHPKB_BADMAGIC 		2
#define XHPKB_BADKBID 		3
#define XHPKB_NONHPINPUTDEV 	4
#define XHPKB_NOMEM	 	5

/* In the following list, several of the constants have duplicate names.
   The duplicate names were added to provide a consistent name for the
   tokens (i.e. each name represents a language). The original version
   mixed country names and language names.                                    */

#define KB_US_English 0         /* For use with HP46021A */
#define KB_Latin_Spanish 1	/* For use with HP46021AM */
#define KB_Katakana 2		/* For use with HP46021AJ */
#define KB_Danish 3		/* For use with HP46021AY */
#define KB_French 4		/* For use with HP46021AF */
#define KB_Norwegian 5		/* For use with HP46021AN */
#define KB_Swiss_German 6	/* For use with HP46020AP
				  : HIL-ID(lower 5 bits)=19h */
#define KB_Canada_French 7	/* For use with HP46021AC */
#define KB_UK_English 8		/* For use with HP46021AU */
#define KB_Finnish 9		/* For use with HP46021AX */
#define KB_Belgian 10		/* For use with HP46021AW */
#define KB_Swiss_German2 11	/* For use with HP46021AP */
#define KB_Euro_Spanish 12	/* For use with HP46021AE */
#define KB_Swiss_French2 13	/* For use with HP46021AQ */
#define KB_T_Chinese 14  	/* Trad. Chinese (ROC): For HP46021W#ZAA */
#define KB_S_Chinese 15  	/* Simp. Chinese (PROC): For HP40621W#ZAC */
#define KB_German 16		/* For use with HP46021AD */
#define KB_Swedish 17		/* For use with HP46021AS */
#define KB_Dutch 18		/* For use with HP46021AH */
#define KB_Korean 19     	/* Korean: For HP40621W#ZAB */ 
#define KB_Italian 20		/* For use with HP46021AZ */
#define KB_Canada_English 21	/* For use with HP46021AL */
#define KB_Swiss_French 22	/* For use with HP46020AQ
				  : HIL-ID(lower 5 bits)=03h */
#define KB_Japanese 23		/* For use with HP46021W#ZAL */

	/* ITF ethereal keyboards */
#define KB_Hebrew		100	/* Hebrew Keymap - NO KEYBOARD */
#define KB_Cyrillic		101	/* Cyrillic Keymap - NO KEYBOARD */
#define KB_Czech		102	/* Czech Keymap - NO KEYBOARD */
#define KB_Hungarian		103	/* Hungarian Keymap - NO KEYBOARD */
#define KB_SerboCroatian	104	/* SerboCroatian Keymap - NO KEYBOARD */
#define KB_Polish		105	/* Polish Keymap - NO KEYBOARD */
#define KB_Romanian		106	/* Romanian Keymap - NO KEYBOARD */
#define KB_Rumanian		106	/* alternate spelling */
#define KB_Turkish		107	/* Turkey Keymap - NO KEYBOARD */
#define KB_Greek		108	/* Greek Keymap - NO KEYBOARD */
#define KB_Arabic		109	/* Greek Keymap - NO KEYBOARD */


	/* PS2 ethereal keyboards (which may never exist) */
#define KB_PS2_Hebrew		150	/* Hebrew Keymap - NO KEYBOARD */
#define KB_PS2_Cyrillic		151	/* Cyrillic Keymap - NO KEYBOARD */
#define KB_PS2_Czech		152	/* Czech Keymap - NO KEYBOARD */
#define KB_PS2_Hungarian	153	/* Hungarian Keymap - NO KEYBOARD */
#define KB_PS2_SerboCroatian	154	/* SerboCroatian Keymap - NO KEYBOARD */
#define KB_PS2_Polish		155	/* Polish Keymap - NO KEYBOARD */
#define KB_PS2_Romanian		156	/* Romanian Keymap - NO KEYBOARD */
#define KB_PS2_Rumanian 	156	/* alternate spelling */
#define KB_PS2_Turkish		157	/* Turkey Keymap - NO KEYBOARD */
#define KB_PS2_Greek		158	/* Greek Keymap - NO KEYBOARD */
#define KB_PS2_Arabic		159	/* Greek Keymap - NO KEYBOARD */

/* ******************** HP hil PS2 keyboards *************** */

#define KB_PS2_US_English	60
#define KB_PS2_Latin_Spanish	61
#define KB_PS2_Katakana		62
#define KB_PS2_Danish		63
#define KB_PS2_French		64
#define KB_PS2_Norwegian	65
#define KB_PS2_Swiss_German	66
#define KB_PS2_Canada_French	67
#define KB_PS2_UK_English	68
#define KB_PS2_Finnish		69
#define KB_PS2_Belgian		70
#define KB_PS2_Swiss_German2	71
#define KB_PS2_Euro_Spanish	72
#define KB_PS2_Swiss_French2	73
#define KB_PS2_T_Chinese	74
#define KB_PS2_S_Chinese	75
#define KB_PS2_German		76
#define KB_PS2_Swedish		77
#define KB_PS2_Dutch		78
#define KB_PS2_Korean		79
#define KB_PS2_Italian		80
#define KB_PS2_Canada_English	84
#define KB_PS2_Swiss_French	88
#define KB_PS2_Japanese		89

#define KB_NULL		201	/* Device that needs a null keymap, modmap */
#define KB_BUTTON_BOX	202	/* HP button box(es) */
#define KB_BARCODE_WAND	203	/* HP barcode reader */

#define KB_HPUNSUPP -1		/* For unsupported HP keyboards */
#define KB_NONHP -2		/* For non-HP keyboards (for internal use) */

typedef	int  KEYBOARD_ID;

/* Function definitions for clients. */

Status      XHPSetKeyboardMapping();
KEYBOARD_ID XHPGetKeyboardID();

/* End of entries required for XHPSetKeyboardMapping. */

typedef struct {
	unsigned char * iso7to8;
	unsigned char * iso8to7;
	char ** mute8to7;
	unsigned int mutekey;
} _XHP_transptrs;

#define _XHP_ISO7To8(status,index) \
	((_XHP_transptrs *) ((status)->compose_ptr)) -> iso7to8 [index]

#define _XHP_ISO8To7(status,index) \
	((_XHP_transptrs *) ((status)->compose_ptr)) -> iso8to7 [index]

#define	_XHP_MUTE8To7(status,index) \
	((_XHP_transptrs *) ((status)->compose_ptr)) -> mute8to7[index-128]

#define XHPInputInit(dpy,status) XHPNlioctl(dpy,status,K16_EXEC_PROC)

#define XHPSetKbdMapInit(dpy,kbd,frc,status) \
    { \
	XHPNlioctl(dpy,status,K16_KILL_PROC); \
	XHPSetKeyboardMapping(dpy,kbd,frc); \
	XHPNlioctl(dpy,status,K16_EXEC_PROC); \
    } 

/* These are the tags in the compose structure for the different convert
 * routines
 */

#define _XHP_INP_NLIO  0x81000000
#define _XHP_INP_ROM8  0x82000000
#define _XHP_INP_7SUB  0x84000000

/* Function definitions for client programs */

PtrFuncInt 	XHPSetErrorHandler();
XFontStruct     *XHPGet16bitMixedFontStruct();

#ifdef _NO_PROTO
extern PtrFuncInt XHPGetEurasianCvt() ;
extern int XGetHpKeyboardId() ;
extern int XHPAcknowledge() ;
extern int XHPDeviceAutoRepeatOff() ;
extern int XHPDeviceAutoRepeatOn() ;
extern int XHPChangeDeviceControl() ;
extern int XHPChangeDeviceKeyMapping() ;
extern int XHPDisableReset() ;
extern int XHPDisableReset() ;
extern int XHPFreeDeviceList() ;
extern XHPTimeCoord * XHPGetDeviceMotionEvents() ;
extern KeySym * XHPGetDeviceKeyMapping() ;
extern XModifierKeymap * XHPGetDeviceModifierMapping() ;
extern int XHPGetServerMode() ;
extern int XHPGrabDeviceButton() ;
extern int XHPGrabDeviceKey() ;
extern int XHPGrabReset() ;
extern int XHPGrabDevice() ;
extern int XHPGetCurrentDeviceMask() ;
extern int XHPGetExtEventMask() ;
extern int XHPGetDeviceFocus() ;
extern int XHPGetDeviceControl() ;
extern XHPDeviceList * XHPListInputDevices() ;
extern int XHPPrompt() ;
extern XHPFilterId XHPRegisterEventFilter();
extern int XHPSetInputDevice() ;
extern int XHPSelectExtensionEvent() ;
extern int XHPUngrabDevice() ;
extern int XHPUngrabDeviceButton() ;
extern int XHPUngrabDeviceKey() ;
extern int XHPSetDeviceFocus() ;
extern int XHPSetDeviceModifierMapping() ;
extern Cursor XHPGetWindowCursor() ;
extern int XHPConvertLookup() ;
extern int input_isolatin1() ;
extern KEYBOARD_ID XHPGetKeyboardID() ;
extern void XHPUpdateKIDList() ;
extern KEYBOARD_ID XHPGetCvtLang() ;
extern KEYBOARD_ID XHPGetHILandCvt() ;
extern int _XHPInitKbdState() ;
extern int _XHP_alt_on() ;
extern void _XHP_GetAltKeys() ;
extern void _XHP_SetAltKeys() ;
extern int _XHPIgnoreLang() ;
extern int XHPInputRoman8() ;
extern int XHPInputLatin1() ;
extern int XHPInputISO8859_8() ;
#else
extern PtrFuncInt XHPGetEurasianCvt( 
                        Display *dpy) ;
extern int XGetHpKeyboardId( 
                        register Display *dpy,
                        unsigned char *kbd_id) ;
extern int XHPAcknowledge( 
                        register Display *dpy,
                        XID device,
                        int ack) ;
extern int XHPDeviceAutoRepeatOff( 
                        register Display *dpy,
                        XID device) ;
extern int XHPDeviceAutoRepeatOn( 
                        register Display *dpy,
                        XID device,
                        int rate) ;
extern int XHPChangeDeviceControl( 
                        register Display *dpy,
                        XID deviceid,
                        unsigned long mask,
                        XHPDeviceControl *vlist) ;
extern int XHPChangeDeviceKeyMapping( 
                        register Display *dpy,
                        XID deviceid,
                        int first,
                        int syms_per_code,
                        KeySym *keysyms,
                        int count) ;
extern int XHPDisableReset( 
        register Display *dpy );
extern int XHPDisableReset( 
        register Display *dpy );
extern int XHPFreeDeviceList( 
                        XHPDeviceList *list) ;
extern XHPTimeCoord * XHPGetDeviceMotionEvents( 
                        register Display *dpy,
                        XID deviceid,
                        Window window,
                        Time start,
                        Time stop,
                        int *nEvents) ;
extern KeySym * XHPGetDeviceKeyMapping( 
                        register Display *dpy,
                        XID deviceid,
                        KeyCode first,
                        int keycount,
                        int *syms_per_code) ;
extern XModifierKeymap * XHPGetDeviceModifierMapping( 
                        register Display *dpy,
                        XID deviceid) ;
extern int XHPGetServerMode( 
                        register Display *dpy,
                        register int screen) ;
extern int XHPGrabDeviceButton( 
                        register Display *dpy,
                        XID device,
                        unsigned int button,
                        unsigned int modifiers,
                        Window grab_window,
                        Bool owner_events,
                        unsigned int event_mask,
                        int pointer_mode,
                        int keyboard_mode) ;
extern int XHPGrabDeviceKey( 
                        register Display *dpy,
                        XID device,
                        unsigned int key,
                        unsigned int modifiers,
                        Window grab_window,
                        Bool owner_events,
                        int pointer_mode,
                        int keyboard_mode) ;
extern int XHPGrabReset( 
                        register Display *dpy,
                        Atom *type) ;
extern int XHPGrabDevice( 
                        register Display *dpy,
                        XID id,
                        Window window,
                        Bool ownerEvents,
                        int pointer_mode,
                        int device_mode,
                        Time time) ;
extern int XHPGetCurrentDeviceMask( 
                        register Display *dpy,
                        Window w,
                        XID device,
                        Mask *mask) ;
extern int XHPGetExtEventMask( 
                        register Display *dpy,
                        long evconst,
                        int *type,
                        Mask *mask) ;
extern int XHPGetDeviceFocus( 
                        register Display *dpy,
                        XID deviceid,
                        Window *focus,
                        int *revert_to) ;
extern int XHPGetDeviceControl( 
                        register Display *dpy,
                        XID deviceid,
                        XHPDeviceState *values) ;
extern XHPDeviceList * XHPListInputDevices( 
                        register Display *dpy,
                        int *ndevices) ;
extern int XHPPrompt( 
                        register Display *dpy,
                        XID device,
                        int prompt) ;
extern XHPFilterId XHPRegisterEventFilter(
			register Display *dpy,
			Window window,
			int device,
			Mask mask,
			int (*routine)(),
			int state_info) ;
extern int XHPSetInputDevice( 
                        register Display *dpy,
                        register XID id,
                        register int mode) ;
extern int XHPSelectExtensionEvent( 
                        register Display *dpy,
                        Window w,
                        XID device,
                        Mask mask) ;
extern int XHPUngrabDevice( 
                        register Display *dpy,
                        XID device,
                        Time time) ;
extern int XHPUngrabDeviceButton( 
                        register Display *dpy,
                        XID device,
                        unsigned int button,
                        unsigned int modifiers,
                        Window grab_window) ;
extern int XHPUngrabDeviceKey( 
                        register Display *dpy,
                        XID device,
                        unsigned int key,
                        unsigned int modifiers,
                        Window grab_window) ;
extern int XHPSetDeviceFocus( 
                        register Display *dpy,
                        XID deviceid,
                        Window focus,
                        int revert_to,
                        Time time) ;
extern int XHPSetDeviceModifierMapping( 
                        register Display *dpy,
                        XID deviceid,
                        XModifierKeymap *modmap) ;
extern Cursor XHPGetWindowCursor( 
                        register Display *dpy,
                        Window window) ;
extern int XHPConvertLookup( 
                        register XKeyEvent *event,
                        char *buffer,
                        int nbytes,
                        KeySym *keysym,
                        XComposeStatus *status,
                        int (*convert_routine)()) ;
extern int input_isolatin1( 
                        KeySym keysym,
                        int modifiers,
                        char *buffer_return,
                        int bytes_buffer,
                        XComposeStatus *status_in_out) ;
extern KEYBOARD_ID XHPGetKeyboardID( 
                        Display *dpy) ;
extern void XHPUpdateKIDList( 
                        Display *dpy,
                        KEYBOARD_ID kbd_id) ;
extern KEYBOARD_ID XHPGetCvtLang( 
                        Display *dpy) ;
extern KEYBOARD_ID XHPGetHILandCvt( 
                        Display *dpy) ;
extern int _XHPInitKbdState( 
                        Display *dpy) ;
extern int _XHP_alt_on( 
                        Display *dpy) ;
extern void _XHP_GetAltKeys( 
                        Display *dpy,
                        K16_state *state) ;
extern void _XHP_SetAltKeys( 
                        Display *dpy,
                        K16_state *state) ;
extern int _XHPIgnoreLang( 
                        int value) ;
extern int XHPInputRoman8( 
                        Display *display,
                        KeySym *keysym,
                        int modifiers,
                        unsigned char *buffer_return,
                        int bytes_buffer,
                        register XComposeStatus *status_in_out) ;
extern int XHPInputLatin1( 
                        Display *display,
                        register KeySym *keysym,
                        int modifiers,
                        unsigned char *buffer_return,
                        int bytes_buffer,
                        register XComposeStatus *status_in_out) ;
extern int XHPInputISO8859_8( 
                        Display *display,
                        KeySym *keysym,
                        unsigned int modifiers,
                        char *buffer_return,
                        int bytes_buffer,
                        XComposeStatus *status_in_out) ;
#endif /* _NO_PROTO */
#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration that encloses file */
#endif /* __cplusplus */
#endif /* XHPLIB_H */
