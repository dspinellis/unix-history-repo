#ifndef lint
static char Xrcsid[] = "$XConsortium: Dvi.c,v 1.9 89/12/10 16:12:25 rws Exp $";
#endif /* lint */

/*
 * Dvi.c - Dvi display widget
 *
 */

#define XtStrlen(s)	((s) ? strlen(s) : 0)

  /* The following are defined for the reader's convenience.  Any
     Xt..Field macro in this code just refers to some field in
     one of the substructures of the WidgetRec.  */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Converters.h>
#include <stdio.h>
#include <ctype.h>
#include "DviP.h"

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */

static char default_font_map[] =  "\
TR	-*-times-medium-r-normal--*-100-*-*-*-*-iso8859-1\n\
TI	-*-times-medium-i-normal--*-100-*-*-*-*-iso8859-1\n\
TB	-*-times-bold-r-normal--*-100-*-*-*-*-iso8859-1\n\
TBI	-*-times-bold-i-normal--*-100-*-*-*-*-iso8859-1\n\
CR	-*-courier-medium-r-normal--*-100-*-*-*-*-iso8859-1\n\
CI	-*-courier-medium-o-normal--*-100-*-*-*-*-iso8859-1\n\
CB	-*-courier-bold-r-normal--*-100-*-*-*-*-iso8859-1\n\
CBI	-*-courier-bold-o-normal--*-100-*-*-*-*-iso8859-1\n\
HR	-*-helvetica-medium-r-normal--*-100-*-*-*-*-iso8859-1\n\
HI	-*-helvetica-medium-o-normal--*-100-*-*-*-*-iso8859-1\n\
HB	-*-helvetica-bold-r-normal--*-100-*-*-*-*-iso8859-1\n\
HBI	-*-helvetica-bold-o-normal--*-100-*-*-*-*-iso8859-1\n\
NR	-*-new century schoolbook-medium-r-normal--*-100-*-*-*-*-iso8859-1\n\
NI	-*-new century schoolbook-medium-i-normal--*-100-*-*-*-*-iso8859-1\n\
NB	-*-new century schoolbook-bold-r-normal--*-100-*-*-*-*-iso8859-1\n\
NBI	-*-new century schoolbook-bold-i-normal--*-100-*-*-*-*-iso8859-1\n\
S	-*-symbol-medium-r-normal--*-100-*-*-*-*-adobe-fontspecific\n\
";

#define offset(field) XtOffset(DviWidget, field)

# define MY_WIDTH(dw)	(dw->dvi.device_resolution * 9)
# define MY_HEIGHT(dw)	(dw->dvi.device_resolution * 11)

static XtResource resources[] = { 
	{XtNfontMap, XtCFontMap, XtRString, sizeof (char *),
	 offset(dvi.font_map_string), XtRString, default_font_map},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (unsigned long),
	 offset(dvi.foreground), XtRString, "black"},
	{XtNbackground, XtCBackground, XtRPixel, sizeof (unsigned long),
	 offset(dvi.background), XtRString, "white"},
	{XtNpageNumber, XtCPageNumber, XtRInt, sizeof (int),
	 offset(dvi.requested_page), XtRString, "1"},
	{XtNlastPageNumber, XtCLastPageNumber, XtRInt, sizeof (int),
	 offset (dvi.last_page), XtRString, "0"},
	{XtNfile, XtCFile, XtRFile, sizeof (FILE *),
	 offset (dvi.file), XtRFile, (char *) 0},
	{XtNseek, XtCSeek, XtRBoolean, sizeof (Boolean),
	 offset(dvi.seek), XtRString, "false"},
	{XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	 offset(dvi.default_font), XtRString, "xtdefaultfont"},
	{XtNbackingStore, XtCBackingStore, XtRBackingStore, sizeof (int),
	 offset(dvi.backing_store), XtRString, "default"},
	{XtNnoPolyText, XtCNoPolyText, XtRBoolean, sizeof (Boolean),
	 offset(dvi.noPolyText), XtRString, "false"},
};

#undef offset

static void		ClassInitialize ();
static void		Initialize(), Realize (), Destroy (), Redisplay ();
static Boolean		SetValues (), SetValuesHook ();
static XtGeometryResult	QueryGeometry ();
static void		ShowDvi ();
static void		CloseFile (), OpenFile ();
static void		FindPage ();

DviClassRec dviClassRec = {
{
	&widgetClassRec,		/* superclass		  */	
	"Dvi",				/* class_name		  */
	sizeof(DviRec),			/* size			  */
	ClassInitialize,		/* class_initialize	  */
	NULL,				/* class_part_initialize  */
	FALSE,				/* class_inited		  */
	Initialize,			/* initialize		  */
	NULL,				/* initialize_hook	  */
	Realize,			/* realize		  */
	NULL,				/* actions		  */
	0,				/* num_actions		  */
	resources,			/* resources		  */
	XtNumber(resources),		/* resource_count	  */
	NULLQUARK,			/* xrm_class		  */
	FALSE,				/* compress_motion	  */
	TRUE,				/* compress_exposure	  */
	TRUE,				/* compress_enterleave    */
	FALSE,				/* visible_interest	  */
	Destroy,			/* destroy		  */
	NULL,				/* resize		  */
	Redisplay,			/* expose		  */
	SetValues,			/* set_values		  */
	SetValuesHook,			/* set_values_hook	  */
	NULL,				/* set_values_almost	  */
	NULL,				/* get_values_hook	  */
	NULL,				/* accept_focus		  */
	XtVersion,			/* version		  */
	NULL,				/* callback_private	  */
	0,				/* tm_table		  */
	QueryGeometry,			/* query_geometry	  */
	NULL,				/* display_accelerator	  */
	NULL				/* extension		  */
},{
	0,				/* field not used    */
},
};

WidgetClass dviWidgetClass = (WidgetClass) &dviClassRec;

static void ClassInitialize ()
{
	XtAddConverter( XtRString, XtRBackingStore, XmuCvtStringToBackingStore,
			NULL, 0 );
}

/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

/* ARGSUSED */
static void Initialize(request, new)
	Widget request, new;
{
	DviWidget	dw = (DviWidget) new;

	dw->dvi.current_page = 0;
	dw->dvi.font_map = 0;
	dw->dvi.cache.index = 0;
	dw->dvi.file = 0;
	dw->dvi.seek = False;
	dw->dvi.device_resolution = 75;
	dw->dvi.line_thickness = -1;
	dw->dvi.line_width = 1;
	dw->dvi.fill = DVI_FILL_MAX;
}

#include <X11/bitmaps/gray>

static void
Realize (w, valueMask, attrs)
	Widget			w;
	XtValueMask		*valueMask;
	XSetWindowAttributes	*attrs;
{
	DviWidget	dw = (DviWidget) w;
	XGCValues	values;

	if (dw->dvi.backing_store != Always + WhenMapped + NotUseful) {
		attrs->backing_store = dw->dvi.backing_store;
		*valueMask |= CWBackingStore;
	}
	XtCreateWindow (w, (unsigned)InputOutput, (Visual *) CopyFromParent,
			*valueMask, attrs);
	values.foreground = dw->dvi.foreground;
	values.cap_style = CapRound;
	values.join_style = JoinRound;
	values.line_width = dw->dvi.line_width;
	dw->dvi.normal_GC = XCreateGC (XtDisplay (w), XtWindow (w),
				       GCForeground|GCCapStyle|GCJoinStyle
				       |GCLineWidth,
				       &values);
	dw->dvi.gray = XCreateBitmapFromData(XtDisplay (w), XtWindow (w),
					     gray_bits,
					     gray_width, gray_height);
	values.background = dw->dvi.background;
	values.stipple = dw->dvi.gray;
	dw->dvi.fill_GC = XCreateGC (XtDisplay (w), XtWindow (w),
				     GCForeground|GCBackground|GCStipple,
				     &values);

	dw->dvi.fill_type = DVI_FILL_BLACK;

	if (dw->dvi.file)
		OpenFile (dw);
	ParseFontMap (dw);
}

static void
Destroy(w)
	Widget w;
{
	DviWidget	dw = (DviWidget) w;

	XFreeGC (XtDisplay (w), dw->dvi.normal_GC);
	XFreeGC (XtDisplay (w), dw->dvi.fill_GC);
	XFreePixmap (XtDisplay (w), dw->dvi.gray);
	DestroyFontMap (dw->dvi.font_map);
	DestroyFileMap (dw->dvi.file_map);
}

/*
 * Repaint the widget window
 */

/* ARGSUSED */
static void
Redisplay(w, event, region)
	Widget w;
	XEvent *event;
	Region region;
{
	DviWidget	dw = (DviWidget) w;
	XRectangle	extents;
	
	XClipBox (region, &extents);
	dw->dvi.extents.x1 = extents.x;
	dw->dvi.extents.y1 = extents.y;
	dw->dvi.extents.x2 = extents.x + extents.width;
	dw->dvi.extents.y2 = extents.y + extents.height;
	ShowDvi (dw);
}

/*
 * Set specified arguments into widget
 */
/* ARGSUSED */
static Boolean
SetValues (current, request, new)
	DviWidget current, request, new;
{
	Boolean		redisplay = FALSE;
	extern char	*malloc ();
	char		*new_map;
	int		cur, req;

	if (current->dvi.font_map_string != request->dvi.font_map_string) {
		new_map = malloc (strlen (request->dvi.font_map_string) + 1);
		if (new_map) {
			redisplay = TRUE;
			strcpy (new_map, request->dvi.font_map_string);
			new->dvi.font_map_string = new_map;
			if (current->dvi.font_map_string)
				free (current->dvi.font_map_string);
			current->dvi.font_map_string = 0;
			ParseFontMap (new);
		}
	}

	req = request->dvi.requested_page;
	cur = current->dvi.requested_page;
	if (cur != req) {
		if (!request->dvi.file)
		    req = 0;
		else {
		    if (req < 1)
			    req = 1;
		    if (current->dvi.last_page != 0 &&
			req > current->dvi.last_page)
			    req = current->dvi.last_page;
		}
		if (cur != req)
	    	    redisplay = TRUE;
		new->dvi.requested_page = req;
		if (current->dvi.last_page == 0 && req > cur)
			FindPage (new);
	}

	return redisplay;
}

/*
 * use the set_values_hook entry to check when
 * the file is set
 */

static Boolean
SetValuesHook (dw, args, num_argsp)
	DviWidget	dw;
	ArgList		args;
	Cardinal	*num_argsp;
{
	Cardinal	i;

	for (i = 0; i < *num_argsp; i++) {
		if (!strcmp (args[i].name, XtNfile)) {
			CloseFile (dw);
			OpenFile (dw);
			return TRUE;
		}
	}
	return FALSE;
}

static void CloseFile (dw)
	DviWidget	dw;
{
	if (dw->dvi.tmpFile)
		fclose (dw->dvi.tmpFile);
	ForgetPagePositions (dw);
}

static void OpenFile (dw)
	DviWidget	dw;
{
	char	tmpName[sizeof ("/tmp/dviXXXXXX")];

	dw->dvi.tmpFile = 0;
	if (!dw->dvi.seek) {
		strcpy (tmpName, "/tmp/dviXXXXXX");
		mktemp (tmpName);
		dw->dvi.tmpFile = fopen (tmpName, "w+");
		unlink (tmpName);
	}
	dw->dvi.requested_page = 1;
	dw->dvi.last_page = 0;
}

static XtGeometryResult
QueryGeometry (w, request, geometry_return)
	Widget			w;
	XtWidgetGeometry	*request, *geometry_return;
{
	XtGeometryResult	ret;
	DviWidget		dw = (DviWidget) w;

	ret = XtGeometryYes;
	if (request->width < MY_WIDTH(dw) || request->height < MY_HEIGHT(dw))
		ret = XtGeometryAlmost;
	geometry_return->width = MY_WIDTH(dw);
	geometry_return->height = MY_HEIGHT(dw);
	geometry_return->request_mode = CWWidth|CWHeight;
	return ret;
}

SetDeviceResolution (dw, resolution)
	DviWidget   dw;
	int	    resolution;
{
	XtWidgetGeometry	request, reply;

	if (resolution != dw->dvi.device_resolution) {
		dw->dvi.device_resolution = resolution;
		request.request_mode = CWWidth|CWHeight;
		request.width = MY_WIDTH(dw);
		request.height = MY_HEIGHT(dw);
		XtMakeGeometryRequest (dw, &request, &reply);
	}
}

static void
ShowDvi (dw)
	DviWidget	dw;
{
	if (!dw->dvi.file) {
		static char Error[] = "No file selected";

		XSetFont (XtDisplay(dw), dw->dvi.normal_GC,
			  dw->dvi.default_font->fid);
		XDrawString (XtDisplay (dw), XtWindow (dw), dw->dvi.normal_GC,
			     20, 20, Error, strlen (Error));
		return;
	}

	FindPage (dw);
	
	dw->dvi.display_enable = 1;
	ParseInput (dw);
	if (dw->dvi.last_page && dw->dvi.requested_page > dw->dvi.last_page)
		dw->dvi.requested_page = dw->dvi.last_page;
}

static void
FindPage (dw)
	DviWidget	dw;
{
	int	i;
	long	file_position;

	if (dw->dvi.requested_page < 1)
		dw->dvi.requested_page = 1;

	if (dw->dvi.last_page != 0 && dw->dvi.requested_page > dw->dvi.last_page)
		dw->dvi.requested_page = dw->dvi.last_page;

	file_position = SearchPagePosition (dw, dw->dvi.requested_page);
	if (file_position != -1) {
		FileSeek(dw, file_position);
		dw->dvi.current_page = dw->dvi.requested_page;
	} else {
		for (i=dw->dvi.requested_page; i > 0; i--) {
			file_position = SearchPagePosition (dw, i);
			if (file_position != -1)
				break;
		}
		if (file_position == -1)
			file_position = 0;
		FileSeek (dw, file_position);

		dw->dvi.current_page = i;
		
		dw->dvi.display_enable = 0;
		while (dw->dvi.current_page != dw->dvi.requested_page) {
			dw->dvi.current_page = ParseInput (dw);
			/*
			 * at EOF, seek back to the begining of this page.
			 */
			if (feof (dw->dvi.file)) {
				file_position = SearchPagePosition (dw,
						dw->dvi.current_page);
				if (file_position != -1)
					FileSeek (dw, file_position);
				dw->dvi.requested_page = dw->dvi.current_page;
				break;
			}
		}
	}
}

/*
Local Variables:
c-indent-level: 8
c-continued-statement-offset: 8
c-brace-offset: -8
c-argdecl-indent: 8
c-label-offset: -8
c-tab-always-indent: nil
End:
*/
