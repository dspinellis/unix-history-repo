/*
 * $XConsortium: XOpenDis.c,v 11.123 91/12/20 16:04:29 rws Exp $
 */

/* Copyright    Massachusetts Institute of Technology    1985, 1986	*/

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
#include <X11/Xatom.h>
#include <stdio.h>

#ifdef X_NOT_STDC_ENV
extern char *getenv();
#endif

extern int _Xdebug;

#ifndef lint
static int lock;	/* get rid of ifdefs when locking implemented */
#endif

static xReq _dummy_request = {
	0, 0, 0
};

static OutOfMemory();

extern Bool _XWireToEvent();
extern Status _XUnknownNativeEvent();
extern Bool _XUnknownWireEvent();
/* 
 * Connects to a server, creates a Display object and returns a pointer to
 * the newly created Display back to the caller.
 */
#if NeedFunctionPrototypes
Display *XOpenDisplay (
	register _Xconst char *display)
#else
Display *XOpenDisplay (display)
	register char *display;
#endif
{
	register Display *dpy;		/* New Display object being created. */
	register int i;
	int j, k;			/* random iterator indexes */
	char *display_name;		/* pointer to display name */
	int endian;			/* to determine which endian. */
	xConnClientPrefix client;	/* client information */
	xConnSetupPrefix prefix;	/* prefix information */
	int vendorlen;			/* length of vendor string */
	char *setup = NULL;		/* memory allocated at startup */
	char *fullname = NULL;		/* expanded name of display */
	int idisplay;			/* display number */
	int iscreen;			/* screen number */
	union {
		xConnSetup *setup;
		char *failure;
		char *vendor;
		xPixmapFormat *sf;
		xWindowRoot *rp;
		xDepth *dp;
		xVisualType *vp;
	} u;				/* proto data returned from server */
	long setuplength;	/* number of bytes in setup message */
	char *conn_auth_name, *conn_auth_data;
	int conn_auth_namelen, conn_auth_datalen;
	unsigned long mask;
	extern Bool _XSendClientPrefix();
	extern int _XConnectDisplay();
	extern XID _XAllocID();

	/*
	 * If the display specifier string supplied as an argument to this 
	 * routine is NULL or a pointer to NULL, read the DISPLAY variable.
	 */
	if (display == NULL || *display == '\0') {
		if ((display_name = getenv("DISPLAY")) == NULL) {
			/* Oops! No DISPLAY environment variable - error. */
			return(NULL);
		}
	}
	else {
		/* Display is non-NULL, copy the pointer */
		display_name = (char *)display;
	}

/*
 * Lock against other threads trying to access global data (like the error
 * handlers and display list).
 */
	LockMutex(&lock);

/*
 * Set the default error handlers.  This allows the global variables to
 * default to NULL for use with shared libraries.
 */
	if (_XErrorFunction == NULL) (void) XSetErrorHandler (NULL);
	if (_XIOErrorFunction == NULL) (void) XSetIOErrorHandler (NULL);

/*
 * Attempt to allocate a display structure. Return NULL if allocation fails.
 */
	if ((dpy = (Display *)Xcalloc(1, sizeof(Display))) == NULL) {
		UnlockMutex(&lock);
		return(NULL);
	}

/*
 * Call the Connect routine to get the network socket. If -1 is returned, the
 * connection failed. The connect routine will set fullname to point to the
 * expanded name.
 */

	if ((dpy->fd = _XConnectDisplay (display_name, &fullname, &idisplay,
					 &iscreen, &conn_auth_name,
					 &conn_auth_namelen, &conn_auth_data,
					 &conn_auth_datalen)) < 0) {
		Xfree ((char *) dpy);
		UnlockMutex(&lock);
		return(NULL);
	}

	/* Initialize as much of the display structure as we can */
	dpy->display_name	= fullname;
	dpy->keysyms		= (KeySym *) NULL;
	dpy->modifiermap	= NULL;
	dpy->lock_meaning	= NoSymbol;
	dpy->keysyms_per_keycode = 0;
	dpy->current		= None;
	dpy->xdefaults		= (char *)NULL;
	dpy->scratch_length	= 0L;
	dpy->scratch_buffer	= NULL;
	dpy->key_bindings	= NULL;
	dpy->ext_procs		= (_XExtension *)NULL;
	dpy->ext_data		= (XExtData *)NULL;
	dpy->ext_number 	= 0;
	dpy->event_vec[X_Error] = _XUnknownWireEvent;
	dpy->event_vec[X_Reply] = _XUnknownWireEvent;
	dpy->wire_vec[X_Error]  = _XUnknownNativeEvent;
	dpy->wire_vec[X_Reply]  = _XUnknownNativeEvent;
	for (i = KeyPress; i < LASTEvent; i++) {
	    dpy->event_vec[i] 	= _XWireToEvent;
	    dpy->wire_vec[i] 	= NULL;
	}
	for (i = LASTEvent; i < 128; i++) {
	    dpy->event_vec[i] 	= _XUnknownWireEvent;
	    dpy->wire_vec[i] 	= _XUnknownNativeEvent;
	}
	dpy->resource_id	= 0;
	dpy->db 		= (struct _XrmHashBucketRec *)NULL;
	dpy->cursor_font	= None;
	dpy->flags		= 0;
/* 
 * Initialize pointers to NULL so that XFreeDisplayStructure will
 * work if we run out of memory
 */

	dpy->screens = NULL;
	dpy->vendor = NULL;
	dpy->buffer = NULL;
	dpy->atoms = NULL;
	dpy->error_vec = NULL;
	dpy->context_db = NULL;

/*
 * Setup other information in this display structure.
 */
	dpy->vnumber = X_PROTOCOL;
	dpy->resource_alloc = _XAllocID;
	dpy->synchandler = NULL;
	dpy->request = 0;
	dpy->last_request_read = 0;
	dpy->default_screen = iscreen;  /* Value returned by ConnectDisplay */
	dpy->last_req = (char *)&_dummy_request;

	/* Set up the output buffers. */
	if ((dpy->bufptr = dpy->buffer = Xmalloc(BUFSIZE)) == NULL) {
	        OutOfMemory (dpy, setup);
		UnlockMutex(&lock);
		return(NULL);
	}
	dpy->bufmax = dpy->buffer + BUFSIZE;
 
	/* Set up the input event queue and input event queue parameters. */
	dpy->head = dpy->tail = NULL;
	dpy->qlen = 0;

	/* Set up free-function record */
	if ((dpy->free_funcs = (_XFreeFuncRec *)Xcalloc(1,
							sizeof(_XFreeFuncRec)))
	    == NULL) {
	    OutOfMemory (dpy, setup);
	    UnlockMutex(&lock);
	    return(NULL);
	}

/*
 * The xConnClientPrefix describes the initial connection setup information
 * and is followed by the authorization information.  Sites that are interested
 * in security are strongly encouraged to use an authentication and 
 * authorization system such as Kerberos.
 */
	endian = 1;
	if (*(char *) &endian)
	    client.byteOrder = '\154'; /* 'l' */
	else
	    client.byteOrder = '\102'; /* 'B' */
	client.majorVersion = X_PROTOCOL;
	client.minorVersion = X_PROTOCOL_REVISION;
	client.nbytesAuthProto = conn_auth_namelen;
	client.nbytesAuthString = conn_auth_datalen;
	if (!_XSendClientPrefix(dpy, &client, conn_auth_name, conn_auth_data))
	{
	    _XDisconnectDisplay (dpy->fd);
	    Xfree ((char *)dpy);
	    UnlockMutex(&lock);
	    return(NULL);
	}	    
	if (conn_auth_name) Xfree(conn_auth_name);
	if (conn_auth_data) Xfree(conn_auth_data);
/*
 * Now see if connection was accepted...
 */
	_XRead (dpy, (char *)&prefix,(long)SIZEOF(xConnSetupPrefix));

	if (prefix.majorVersion < X_PROTOCOL) {
	    fprintf (stderr,
       "Xlib:  warning, client built for newer rev (%d) than server (%d)!\r\n",
		     X_PROTOCOL, prefix.majorVersion);
	}
	if (prefix.minorVersion != X_PROTOCOL_REVISION) {
	    fprintf (stderr,
     "Xlib:  warning, client is protocol rev %d, server is rev %d!\r\n",
		     X_PROTOCOL_REVISION, prefix.minorVersion);
	}

	setuplength = prefix.length << 2;
	if ( (u.setup = (xConnSetup *)
	      (setup =  Xmalloc ((unsigned) setuplength))) == NULL) {
		_XDisconnectDisplay (dpy->fd);
		Xfree ((char *)dpy);
		UnlockMutex(&lock);
		return(NULL);
	}
	_XRead (dpy, (char *)u.setup, setuplength);
/*
 * If the connection was not accepted by the server due to problems,
 * give error message to the user....
 */
	if (prefix.success != xTrue) {
		/* XXX - printing messages marks a bad programming interface */
		fprintf (stderr, 
			 "%s:  connection to \"%s\" refused by server\r\n%s:  ",
			 "Xlib", fullname, "Xlib");
		(void) fwrite (u.failure, sizeof(char),
			(int)prefix.lengthReason, stderr);
		(void) fwrite ("\r\n", sizeof(char), 2, stderr);
		OutOfMemory(dpy, setup);
		UnlockMutex(&lock);
		return (NULL);
	}

/*
 * We succeeded at authorization, so let us move the data into
 * the display structure.
 */
	dpy->proto_major_version= prefix.majorVersion;
	dpy->proto_minor_version= prefix.minorVersion;
	dpy->release 		= u.setup->release;
	dpy->resource_base	= u.setup->ridBase;
	dpy->resource_mask	= u.setup->ridMask;
	dpy->min_keycode	= u.setup->minKeyCode;
	dpy->max_keycode	= u.setup->maxKeyCode;
	dpy->motion_buffer	= u.setup->motionBufferSize;
	dpy->nformats		= u.setup->numFormats;
	dpy->nscreens		= u.setup->numRoots;
	dpy->byte_order		= u.setup->imageByteOrder;
	dpy->bitmap_unit	= u.setup->bitmapScanlineUnit;
	dpy->bitmap_pad		= u.setup->bitmapScanlinePad;
	dpy->bitmap_bit_order   = u.setup->bitmapBitOrder;
	dpy->max_request_size	= u.setup->maxRequestSize;
	mask = dpy->resource_mask;
	dpy->resource_shift	= 0;
	while (!(mask & 1)) {
	    dpy->resource_shift++;
	    mask = mask >> 1;
	}
/*
 * now extract the vendor string...  String must be null terminated,
 * padded to multiple of 4 bytes.
 */
	dpy->vendor = (char *) Xmalloc((unsigned) (u.setup->nbytesVendor + 1));
	if (dpy->vendor == NULL) {
	    OutOfMemory(dpy, setup);
	    UnlockMutex(&lock);
	    return (NULL);
	}
	vendorlen = u.setup->nbytesVendor;
 	u.setup = (xConnSetup *) (((char *) u.setup) + sz_xConnSetup);
  	(void) strncpy(dpy->vendor, u.vendor, vendorlen);
	dpy->vendor[vendorlen] = '\0';
 	vendorlen = (vendorlen + 3) & ~3;	/* round up */
	bcopy (u.vendor + vendorlen, setup,
	       (int) setuplength - sz_xConnSetup - vendorlen);
 	u.vendor = setup;
/*
 * Now iterate down setup information.....
 */
	dpy->pixmap_format = 
	    (ScreenFormat *)Xmalloc(
		(unsigned) (dpy->nformats *sizeof(ScreenFormat)));
	if (dpy->pixmap_format == NULL) {
	        OutOfMemory (dpy, setup);
		UnlockMutex(&lock);
		return(NULL);
	}
/*
 * First decode the Z axis Screen format information.
 */
	for (i = 0; i < dpy->nformats; i++) {
	    register ScreenFormat *fmt = &dpy->pixmap_format[i];
	    fmt->depth = u.sf->depth;
	    fmt->bits_per_pixel = u.sf->bitsPerPixel;
	    fmt->scanline_pad = u.sf->scanLinePad;
	    fmt->ext_data = NULL;
	    u.sf = (xPixmapFormat *) (((char *) u.sf) + sz_xPixmapFormat);
	}

/*
 * next the Screen structures.
 */
	dpy->screens = 
	    (Screen *)Xmalloc((unsigned) dpy->nscreens*sizeof(Screen));
	if (dpy->screens == NULL) {
	        OutOfMemory (dpy, setup);
		UnlockMutex(&lock);
		return(NULL);
	}
/*
 * Now go deal with each screen structure.
 */
	for (i = 0; i < dpy->nscreens; i++) {
	    register Screen *sp = &dpy->screens[i];
	    VisualID root_visualID = u.rp->rootVisualID;
	    sp->display	    = dpy;
	    sp->root 	    = u.rp->windowId;
	    sp->cmap 	    = u.rp->defaultColormap;
	    sp->white_pixel = u.rp->whitePixel;
	    sp->black_pixel = u.rp->blackPixel;
	    sp->root_input_mask = u.rp->currentInputMask;
	    sp->width	    = u.rp->pixWidth;
	    sp->height	    = u.rp->pixHeight;
	    sp->mwidth	    = u.rp->mmWidth;
	    sp->mheight	    = u.rp->mmHeight;
	    sp->min_maps    = u.rp->minInstalledMaps;
	    sp->max_maps    = u.rp->maxInstalledMaps;
	    sp->root_visual = NULL;  /* filled in later, when we alloc Visuals */
	    sp->backing_store= u.rp->backingStore;
	    sp->save_unders = u.rp->saveUnders;
	    sp->root_depth  = u.rp->rootDepth;
	    sp->ndepths	    = u.rp->nDepths;
	    sp->ext_data   = NULL;
	    u.rp = (xWindowRoot *) (((char *) u.rp) + sz_xWindowRoot);
/*
 * lets set up the depth structures.
 */
	    sp->depths = (Depth *)Xmalloc(
			(unsigned)sp->ndepths*sizeof(Depth));
	    if (sp->depths == NULL) {
		OutOfMemory (dpy, setup);
		UnlockMutex(&lock);
		return(NULL);
	    }
	    /*
	     * for all depths on this screen.
	     */
	    for (j = 0; j < sp->ndepths; j++) {
		Depth *dp = &sp->depths[j];
		dp->depth = u.dp->depth;
		dp->nvisuals = u.dp->nVisuals;
		u.dp = (xDepth *) (((char *) u.dp) + sz_xDepth);
		if (dp->nvisuals > 0) {
		    dp->visuals = 
		      (Visual *)Xmalloc((unsigned)dp->nvisuals*sizeof(Visual));
		    if (dp->visuals == NULL) {
			OutOfMemory (dpy, setup);
			UnlockMutex(&lock);
			return(NULL);
		    }
		    for (k = 0; k < dp->nvisuals; k++) {
			register Visual *vp = &dp->visuals[k];
			if ((vp->visualid = u.vp->visualID) == root_visualID)
			   sp->root_visual = vp;
			vp->class	= u.vp->class;
			vp->bits_per_rgb= u.vp->bitsPerRGB;
			vp->map_entries	= u.vp->colormapEntries;
			vp->red_mask	= u.vp->redMask;
			vp->green_mask	= u.vp->greenMask;
			vp->blue_mask	= u.vp->blueMask;
			vp->ext_data	= NULL;
			u.vp = (xVisualType *) (((char *) u.vp) +
						sz_xVisualType);
		    }
		} else {
		    dp->visuals = (Visual *) NULL;
		}
	    }
	}
		

/*
 * Now start talking to the server to setup all other information...
 */

	Xfree (setup);	/* all finished with setup information */

/*
 * Make sure default screen is legal.
 */
	if (iscreen >= dpy->nscreens) {
	    OutOfMemory(dpy, (char *) NULL);
	    UnlockMutex(&lock);
	    return(NULL);
	}

/*
 * Set up other stuff clients are always going to use.
 */
	for (i = 0; i < dpy->nscreens; i++) {
	    register Screen *sp = &dpy->screens[i];
	    XGCValues values;
	    values.foreground = sp->black_pixel;
	    values.background = sp->white_pixel;
	    if ((sp->default_gc = XCreateGC (dpy, sp->root,
					     GCForeground|GCBackground,
					     &values)) == NULL) {
		OutOfMemory(dpy, (char *) NULL);
		UnlockMutex (&lock);
		return (NULL);
	    }
	}
/*
 * call into synchronization routine so that all programs can be
 * forced synchronize
 */
	(void) XSynchronize(dpy, _Xdebug);

/*
 * and done mucking with the display
 */
	UnlockDisplay(dpy);		/* didn't exist, so didn't lock */
	UnlockMutex(&lock);

/*
 * get the resource manager database off the root window.
 */
	{
	    Atom actual_type;
	    int actual_format;
	    unsigned long nitems;
	    unsigned long leftover;
	    char *xdef = NULL;

	    if (XGetWindowProperty (dpy, RootWindow(dpy, 0),
				    XA_RESOURCE_MANAGER, 0L, 100000000L, False,
				    XA_STRING, &actual_type, &actual_format,
				    &nitems, &leftover,
				    (unsigned char **) &xdef) == Success) {
		if ((actual_type == XA_STRING) && (actual_format == 8)) {
		    LockDisplay (dpy);
		    dpy->xdefaults = xdef;
		    UnlockDisplay (dpy);
		} else if (xdef) {
		    Xfree (xdef);
		}
	    }
	}

#ifdef MOTIFBC
	{
	    extern Display *_XHeadOfDisplayList;
	    _XHeadOfDisplayList = dpy;
	}
#endif
/*
 * and return successfully
 */
 	return(dpy);
}


/* OutOfMemory is called if malloc fails.  XOpenDisplay returns NULL
   after this returns. */

static OutOfMemory (dpy, setup)
    Display *dpy;
    char *setup;
    {
    _XDisconnectDisplay (dpy->fd);
    _XFreeDisplayStructure (dpy);
    if (setup) Xfree (setup);
    }


/* XFreeDisplayStructure frees all the storage associated with a 
 * Display.  It is used by XOpenDisplay if it runs out of memory,
 * and also by XCloseDisplay.   It needs to check whether all pointers
 * are non-NULL before dereferencing them, since it may be called
 * by XOpenDisplay before the Display structure is fully formed.
 * XOpenDisplay must be sure to initialize all the pointers to NULL
 * before the first possible call on this.
 */

_XFreeDisplayStructure(dpy)
	register Display *dpy;
{
	while (dpy->ext_procs) {
	    _XExtension *ext = dpy->ext_procs;
	    dpy->ext_procs = ext->next;
	    if (ext->name)
		Xfree (ext->name);
	    Xfree ((char *)ext);
	}
	if (dpy->im_filters)
	   (*dpy->free_funcs->im_filters)(dpy);
	if (dpy->cms.clientCmaps)
	   (*dpy->free_funcs->clientCmaps)(dpy);
	if (dpy->cms.defaultCCCs)
	   (*dpy->free_funcs->defaultCCCs)(dpy);
	if (dpy->cms.perVisualIntensityMaps)
	   (*dpy->free_funcs->intensityMaps)(dpy);
	if (dpy->atoms)
	    (*dpy->free_funcs->atoms)(dpy);
	if (dpy->modifiermap)
	   (*dpy->free_funcs->modifiermap)(dpy->modifiermap);
	if (dpy->key_bindings)
	   (*dpy->free_funcs->key_bindings)(dpy);
	if (dpy->context_db)
	   (*dpy->free_funcs->context_db)(dpy);

	if (dpy->screens) {
	    register int i;

            for (i = 0; i < dpy->nscreens; i++) {
		Screen *sp = &dpy->screens[i];

		if (sp->depths) {
		   register int j;

		   for (j = 0; j < sp->ndepths; j++) {
			Depth *dp = &sp->depths[j];

			if (dp->visuals) {
			   register int k;

			   for (k = 0; k < dp->nvisuals; k++)
			     _XFreeExtData (dp->visuals[k].ext_data);
			   Xfree ((char *) dp->visuals);
			   }
			}

		   Xfree ((char *) sp->depths);
		   }

		_XFreeExtData (sp->ext_data);
		}

	    Xfree ((char *)dpy->screens);
	    }
	
	if (dpy->pixmap_format) {
	    register int i;

	    for (i = 0; i < dpy->nformats; i++)
	      _XFreeExtData (dpy->pixmap_format[i].ext_data);
            Xfree ((char *)dpy->pixmap_format);
	    }

	if (dpy->display_name)
	   Xfree (dpy->display_name);
	if (dpy->vendor)
	   Xfree (dpy->vendor);

        if (dpy->buffer)
	   Xfree (dpy->buffer);
	if (dpy->keysyms)
	   Xfree ((char *) dpy->keysyms);
	if (dpy->xdefaults)
	   Xfree (dpy->xdefaults);
	if (dpy->error_vec)
	    Xfree ((char *)dpy->error_vec);

	_XFreeExtData (dpy->ext_data);
	Xfree ((char *)dpy->free_funcs);

	Xfree ((char *)dpy);
}
