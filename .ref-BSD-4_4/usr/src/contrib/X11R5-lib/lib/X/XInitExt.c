/* $XConsortium: XInitExt.c,v 11.28 91/01/08 14:41:05 gildea Exp $ */
/* Copyright  Massachusetts Institute of Technology 1987 */

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#include <X11/Xlibint.h>
#include <X11/Xos.h>
#include <stdio.h>

extern Bool _XUnknownWireEvent();
extern Status _XUnknownNativeEvent();
extern Bool _XDefaultWireError();

/*
 * This routine is used to link a extension in so it will be called
 * at appropriate times.
 */

#if NeedFunctionPrototypes
XExtCodes *XInitExtension (
	Display *dpy,
	_Xconst char *name)
#else
XExtCodes *XInitExtension (dpy, name)
	Display *dpy;
	char *name;
#endif
{
	XExtCodes codes;	/* temp. place for extension information. */
	register _XExtension *ext;/* need a place to build it all */
	if (!XQueryExtension(dpy, name, 
		&codes.major_opcode, &codes.first_event,
		&codes.first_error)) return (NULL);

	LockDisplay (dpy);
	if (! (ext = (_XExtension *) Xcalloc (1, sizeof (_XExtension))) ||
	    ! (ext->name = Xmalloc((unsigned) strlen(name) + 1))) {
	    if (ext) Xfree((char *) ext);
	    UnlockDisplay(dpy);
	    return (XExtCodes *) NULL;
	}
	codes.extension = dpy->ext_number++;
	ext->codes = codes;
	(void) strcpy(ext->name, name);

	/* chain it onto the display list */	
	ext->next = dpy->ext_procs;
	dpy->ext_procs = ext;
	UnlockDisplay (dpy);

	return (&ext->codes);		/* tell him which extension */
}

XExtCodes *XAddExtension (dpy)
    Display *dpy;
{
    register _XExtension *ext;

    LockDisplay (dpy);
    if (! (ext = (_XExtension *) Xcalloc (1, sizeof (_XExtension)))) {
	UnlockDisplay(dpy);
	return (XExtCodes *) NULL;
    }
    ext->codes.extension = dpy->ext_number++;

    /* chain it onto the display list */
    ext->next = dpy->ext_procs;
    dpy->ext_procs = ext;
    UnlockDisplay (dpy);

    return (&ext->codes);		/* tell him which extension */
}

static _XExtension *XLookupExtension (dpy, extension)
	register Display *dpy;	/* display */
	register int extension;	/* extension number */
{
	register _XExtension *ext = dpy->ext_procs;
	while (ext != NULL) {
		if (ext->codes.extension == extension) return (ext);
		ext = ext->next;
	}
	return (NULL);
}

XExtData **XEHeadOfExtensionList(object)
    XEDataObject object;
{
    return *(XExtData ***)&object;
}

XAddToExtensionList(structure, ext_data)
    XExtData **structure;
    XExtData *ext_data;
{
    ext_data->next = *structure;
    *structure = ext_data;
}

XExtData *XFindOnExtensionList(structure, number)
    XExtData **structure;
    int number;
{
    XExtData *ext;

    ext = *structure;
    while (ext && (ext->number != number))
	ext = ext->next;
    return ext;
}

/*
 * Routines to hang procs on the extension structure.
 */
#if NeedFunctionPrototypes
int (*XESetCreateGC(
    Display*	dpy,
    int		extension,
    int (*proc) (
#if NeedNestedPrototypes
	      Display*		/* display */,
	      GC		/* gc */,
	      XExtCodes*	/* codes */
#endif
	    )
    ))(
#if NeedNestedPrototypes
    Display*, GC, XExtCodes*
#endif
)
#else
int (*XESetCreateGC(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	int (*proc)();		/* routine to call when GC created */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->create_GC;
	e->create_GC = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
int (*XESetCopyGC(
	Display * dpy,
	int extension,
	int (*proc)(
#if NeedNestedPrototypes
	      Display*			/* display */,
              GC			/* gc */,
              XExtCodes*		/* codes */
#endif
		    )
))(
#if NeedNestedPrototypes
    Display*, GC, XExtCodes*
#endif
)
#else
int (*XESetCopyGC(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	int (*proc)();		/* routine to call when GC copied */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->copy_GC;
	e->copy_GC = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
int (*XESetFlushGC(
    Display* dpy,
    int extension,
    int (*proc) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              GC			/* gc */,
              XExtCodes*		/* codes */
#endif
            )
))(
#if NeedNestedPrototypes
    Display*, GC, XExtCodes*
#endif
)
#else
int (*XESetFlushGC(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	int (*proc)();		/* routine to call when GC copied */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->flush_GC;
	e->flush_GC = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
int (*XESetFreeGC(
    Display* dpy,
    int extension,
    int (*proc) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              GC			/* gc */,
              XExtCodes*		/* codes */
#endif
            )
))(
#if NeedNestedPrototypes
    Display*, GC, XExtCodes*
#endif
)
#else
int (*XESetFreeGC(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	int (*proc)();		/* routine to call when GC freed */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->free_GC;
	e->free_GC = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
int (*XESetCreateFont(
    Display* dpy,
    int extension,
    int (*proc) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              XFontStruct*		/* fs */,
              XExtCodes*		/* codes */
#endif
            )
))(
#if NeedNestedPrototypes
    Display*, XFontStruct*, XExtCodes*
#endif
)
#else
int (*XESetCreateFont(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	int (*proc)();		/* routine to call when font created */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->create_Font;
	e->create_Font = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
int (*XESetFreeFont(
    Display* dpy,
    int	extension,
    int (*proc) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              XFontStruct*		/* fs */,
              XExtCodes*		/* codes */
#endif
            )
))(
#if NeedNestedPrototypes
    Display*, XFontStruct*, XExtCodes*
#endif
)
#else
int (*XESetFreeFont(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	int (*proc)();		/* routine to call when font freed */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->free_Font;
	e->free_Font = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
int (*XESetCloseDisplay(
    Display* dpy,
    int	extension,
    int (*proc) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              XExtCodes*		/* codes */
#endif
            )
))(
#if NeedNestedPrototypes
    Display*, XExtCodes*
#endif
)
#else
int (*XESetCloseDisplay(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	int (*proc)();		/* routine to call when display closed */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->close_display;
	e->close_display = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
int (*XESetWireToEvent(
    Display* dpy,
    int event_number,
    Bool (*proc) (
#if NeedNestedPrototypes
	       Display*			/* display */,
               XEvent*			/* re */,
               xEvent*			/* event */
#endif
             )
))(
#if NeedNestedPrototypes
    Display*, XEvent*, xEvent*
#endif
)
#else
Bool (*XESetWireToEvent(dpy, event_number, proc))()
	Display *dpy;		/* display */
	Bool (*proc)();		/* routine to call when converting event */
	int event_number;	/* event routine to replace */
#endif
{
	register Bool (*oldproc)();
	if (proc == NULL) proc = _XUnknownWireEvent;
	LockDisplay (dpy);
	oldproc = dpy->event_vec[event_number];
	dpy->event_vec[event_number] = proc;
	UnlockDisplay (dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
Status (*XESetEventToWire(
    Display* dpy,
    int	event_number,
    int (*proc) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              XEvent*			/* re */,
              xEvent*			/* event */
#endif
            )
))(
#if NeedNestedPrototypes
    Display*, XEvent*, xEvent*
#endif
)
#else
Status (*XESetEventToWire(dpy, event_number, proc))()
	Display *dpy;		/* display */
	Status (*proc)();	/* routine to call when converting event */
	int event_number;	/* event routine to replace */
#endif
{
	register Status (*oldproc)();
	if (proc == NULL) proc = _XUnknownNativeEvent;
	LockDisplay (dpy);
	oldproc = dpy->wire_vec[event_number];
	dpy->wire_vec[event_number] = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
Status (*XESetWireToError(
    Display* dpy,
    int	error_number,
    Bool (*proc) (
#if NeedNestedPrototypes
	       Display*			/* display */,
	       XErrorEvent*		/* he */,
	       xError*			/* we */
#endif
            )
))(
#if NeedNestedPrototypes
    Display*, XErrorEvent*, xError*
#endif
)
#else
Bool (*XESetWireToError(dpy, error_number, proc))()
	Display *dpy;		/* display */
	Bool (*proc)();		/* routine to call when converting error */
	int error_number;	/* error routine to replace */
#endif
{
	register Bool (*oldproc)();
	if (proc == NULL) proc = _XDefaultWireError;
	LockDisplay (dpy);
	if (!dpy->error_vec) {
	    int i;
	    dpy->error_vec = (Bool (**)())Xmalloc(256 * sizeof(oldproc));
	    for (i = 1; i < 256; i++)
		dpy->error_vec[i] = _XDefaultWireError;
	}
	if (dpy->error_vec) {
	    oldproc = dpy->error_vec[error_number];
	    dpy->error_vec[error_number] = proc;
	}
	UnlockDisplay (dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
int (*XESetError(
    Display* dpy,
    int	extension,
    int (*proc) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              xError*			/* err */,
              XExtCodes*		/* codes */,
              int*			/* ret_code */
#endif
            )		/* proc */    
))(
#if NeedNestedPrototypes
    Display*, xError*, XExtCodes*, int*
#endif
)
#else
int (*XESetError(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	int (*proc)();		/* routine to call when X error happens */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register int (*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->error;
	e->error = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
char* (*XESetErrorString(
    Display* dpy,
    int extension,
    char* (*proc) (
#if NeedNestedPrototypes
	        Display*		/* display */,
                int			/* code */,
                XExtCodes*		/* codes */,
                char*			/* buffer */,
                int			/* nbytes */
#endif
              )
))(
#if NeedNestedPrototypes
    Display*, int, XExtCodes*, char*, int
#endif
)
#else
char *(*XESetErrorString(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	char *(*proc)();	/* routine to call when I/O error happens */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register char *(*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->error_string;
	e->error_string = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}

#if NeedFunctionPrototypes
void (*XESetPrintErrorValues (
    Display* dpy,
    int extension,
    void (*proc)(
#if NeedNestedPrototypes
	      Display*			/* display */,
	      XErrorEvent*		/* ev */,
	      void*			/* fp */
#endif
	     )
))(
#if NeedNestedPrototypes
    Display*, XErrorEvent*, void*
#endif
)
#else
void (*XESetPrintErrorValues(dpy, extension, proc))()
	Display *dpy;		/* display */
	int extension;		/* extension number */
	void (*proc)();		/* routine to call to print */
#endif
{
	register _XExtension *e;	/* for lookup of extension */
	register void (*oldproc)();
	if ((e = XLookupExtension (dpy, extension)) == NULL) return (NULL);
	LockDisplay(dpy);
	oldproc = e->error_values;
	e->error_values = proc;
	UnlockDisplay(dpy);
	return (oldproc);
}
