/*
 * $XConsortium: EditresP.h,v 1.11 91/07/30 15:28:28 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Chris D. Peterson, MIT X Consortium
 */

/************************************************************

		The Editres Protocol


  The Client message sent to the application is:

  ATOM = "ResEditor" 		--- RES_EDITOR_NAME

  FORMAT = 32                   --- RES_EDIT_SEND_EVENT_FORMAT

  l[0] = timestamp
  l[1] = command atom name
  l[2] = ident of command.
  l[3] = protocol version number to use.



  The binary protocol has the following format:

	Card8:		8-bit unsingned integer
	Card16:		16-bit unsingned integer
	Card32:		32-bit unsingned integer
	Int16:		16-bit signed integer
	Window:		32-bit value
	Widget:		32-bit value
	String8:        ListOfCard8
	
	[a][b][c] represent an exclusive list of choices.

	All widgets are passed as a list of widgets, containing the 
	full instance heirarch of this widget.  The hierarchy is ordered
	from parent to child.  Thus the first element of each list is
	the root of the widget tree (this makes verifying that the widget
	still exists, MUCH faster).

	ListOfFoo comprises a list of things in the following format:
	
	number:			Card16
	<number> things:	????
	
  This is a synchronous protocol, every request MUST be followed by a
  reply.  

  Request:

	Serial Number:	Card8
	Op Code:	Card8 -	{ SendWidgetTree = 0,
				  SetValues = 1,
				  GetResources = 2,
				  GetGeometry = 3,
				  FindChild = 4 }
	Length:		Card32
	Data:		

   Reply:

	Serial Number:	Card8
	Type:		Card8 - { Formatted = 0,
	                          Unformatted = 1,
				  ProtocolMismatch = 2
				}
	Length:		Card32


   Byte Order:

	All Fields are MSB -> LSB

    Data:

    	Formatted:

        	The data contains the reply information for the request as
		specified below if the reply type is "Formatted".  The return
		values for the other reply types are shown below.

        Unformatted:

		Message:	String8

	ProtocolMismatch:

		RequestedVersion:   	Card8

------------------------------------------------------------

   SendWidgetTree:

	--->

	Number of Entries:	Card16
	Entry:
		widget:		ListOfWidgets
		name:		String8
		class:		String8
		window:		Card32

	Send Widget Tree returns the fully specified list of widgets
	for each widget in the tree.  This is enough information to completely
	reconstruct the entire widget heirarchy.

	The window return value contains the Xid of the window currently 
	used by this widget.  If the widget is unrealized then 0 is returned,
	and if widget is a non-windowed object a value of 2 is returned.

   SetValues:

	name:	String8
	type:	String8
	value:  String8
	Number of Entries:	Card16
	Entry:
		widget:		ListOfWidgets

	--->

	Number of Entries:	Card16
	Entry:
		widget:		ListOfWidgets
		message:	String8

	SetValues will allow the same resource to be set on a number of 
	widgets.  This function will return an error message if the SetValues
	request caused an Xt error.
	
   GetResources:

	Number of Entries:	Card16
	Entry
		widget:		ListOfWidgets:

	---->

	Number of Entries:	Card16
	Entry
		Widget:			ListOfWidgets:
		Error:			Bool

		[ Message:		String 8 ]
		[ Number of Resources:	Card16
		Resource:
			Kind:	{normal, constraint}
			Name:	String8
			Class:	String8	
			Type:	String8 ]

	GetResource retrieves the kind, name, class and type for every 
	widget passed to it.  If an error occured with the resource fetch
	Error will be set to True for the given widget and a message
	is returned rather than the resource info.

  GetGeometry:

	Number of Entries:	Card16
	Entry
		Widget:		ListOfWidgets:

	---->

	Number of Entries:	Card16
	Entry
		Widget:			ListOfWidgets:
		Error:			Bool

		[ message:		String 8 ]
		[ mapped:       Boolean
		  X: 		Int16
		  Y:  		Int16
		  Width: 	Card16
	      	  Height:	Card16
		  BorderWidth:	Card16 ]

	GetGeometry retreives the mapping state, x, y, width, height
	and border width for each widget specified.  If an error occured 
	with the geometry fetch "Error" will be set to True for the given 
	widget and a message is returned rather than the geometry info.  
	X an Y corrospond to the root coordinates of the upper left corner
	of the widget (outside the window border).
	
  FindChild:

	Widget:		ListOfWidgets
	X:		Int16
	Y:		Int16
	
	--->

	Widget:		ListOfWidgets

	Find Child returns a descendent of the widget specified that 
	is at the root coordinates specified.

	NOTE:

	The returned widget is undefined if the point is contained in
	two or more mapped widgets, or in two overlapping Rect objs.

************************************************************/

#include <X11/Intrinsic.h>
#include <X11/Xfuncproto.h>

#define XER_NBBY 8		/* number of bits in a byte */
#define BYTE_MASK 255

#define HEADER_SIZE 6

#define EDITRES_IS_OBJECT 2
#define EDITRES_IS_UNREALIZED 0

/*
 * Format for atoms.
 */

#define EDITRES_FORMAT             8
#define EDITRES_SEND_EVENT_FORMAT 32

/*
 * Atoms
 */

#define EDITRES_NAME         "Editres"
#define EDITRES_COMMAND_ATOM "EditresCommand"
#define EDITRES_COMM_ATOM    "EditresComm"
#define EDITRES_CLIENT_VALUE "EditresClientVal"
#define EDITRES_PROTOCOL_ATOM "EditresProtocol"

typedef enum { SendWidgetTree = 0, SetValues = 1, GetResources = 2,
	       GetGeometry = 3, FindChild = 4 } EditresCommand;

typedef enum {NormalResource = 0, ConstraintResource = 1} ResourceType;

/*
 * The type of a resource identifier.
 */

typedef unsigned char ResIdent;

typedef enum {PartialSuccess= 0, Failure= 1, ProtocolMismatch= 2} EditResError;

typedef struct _WidgetInfo {
    unsigned short num_widgets;
    unsigned long * ids;
    Widget real_widget;
} WidgetInfo;

typedef struct _ProtocolStream {
    unsigned long size, alloc;
    unsigned char *real_top, *top, *current;
} ProtocolStream;

/************************************************************
 *
 * Function definitions for reading and writing protocol requests.
 *
 ************************************************************/

_XFUNCPROTOBEGIN

void _XEditResPutString8(), _XEditResPut8(), _XEditResPut16();
void _XEditResPut32(), _XEditResPutWidgetInfo(), _XEditResPutWidget();
void _XEditResResetStream();

Boolean _XEditResGet8(), _XEditResGet16(), _XEditResGetSigned16();
Boolean _XEditResGet32(), _XEditResGetString8(), _XEditResGetWidgetInfo();

_XFUNCPROTOEND
