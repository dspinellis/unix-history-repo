/* Program to display images on uVax II/GPX under the X window system.
 *
 * Bill Wyatt and Jim Gettys, Feb. 12 1986 et. seq.
 *
 * 4/23/86 WFW
 *     Added FITS file reading capability, selected via -fits option.
 *
 * 7/86  WFW
 *     Added disk fits format via -dfits option, for not swapping bytes,
 *     but file otherwise in disk format.
 *
 * 7/86  WFW
 *     Added palette subwindow, zoomx4 window, independently resizeable.
 *
 * 8/11/86  WFW
 *     If the palette window needs to be independent instead of a subwindow,
 *     then compile with PALWIND defined.
 * 
 * 8/13/86 WFW
 *     Added  ifdef support from Eric Mandel for ROSAT & Einstein images
 */

#include <stdio.h>
#include <sys/file.h>
#include <X/Xlib.h>
#include <X/XMenu.h>
#include <X/Xkeyboard.h>

#ifdef MC68000
#include <sys/types.h>
#endif

#include "show.icon.ic"

#include "shimg.h"
#include "shopt.h"

#define TESTDEFAULT "=512x512+100+134"
#define PICTDEFAULT "+50+100"
#define TESTNROWS  512

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

main(argc, argv)
int argc;
char **argv;
{
    struct windmain  maininfo;
    struct colorwind colorinfo;
    struct imagewind imageinfo;

    Window wind, subwind, palwind, wzoom;
    Window iconwindow, iconwzoom;
#ifdef PALWIND
    Window iconpalwind;   /* palette window is independent, needs icon */
#endif
    WindowInfo winfo, palwinfo, wzoominfo;


    Cursor imcursor, pancursor, printcursor, zoom2cursor, zoom4cursor;
    Cursor palcursor;
    int xcenter = -1;
    int ycenter = -1;      /* nominal center of displayed image  */
    int xtempzero, ytempzero;
    int xzoomsize = 124, yzoomsize = 124;
    XMenu *showmenu, *menusetup();
    int XMenuSetFreeze(), XMenuActivate();
    int *currentflags;
    int pane = 0, sel = 0;
    int lastpane = 0, lastsel = 0;
    unsigned char palette[2048]; /* palette window array */
    short *readheader(), *readpict();
    int scalepict();
    int zoomfactor = 0;     /* replication factor for pixel display */
    int oldzoomfactor = 0;
    int fd, open(), close(), ipow();
    int w_mapped = 1;
    int rshift = 0;
    int keycode = 0;
    int xbut, ybut;
    char *malloc();
    char *keystring;  
    int j, k, l;
    
    XEvent event, peekevent;
    XExposeEvent               *expw  = (XExposeEvent *)&event;
    register XKeyOrButtonEvent *but   = (XButtonEvent *)&event;
    register XMouseMovedEvent  *mouse = (XMouseMovedEvent *)&event;
    
    register int downbutton = -1; /* mouse button flag */
    register int xzero, yzero;   /* image coordinates of window(0,0) */
    register int i;
    

/* **** MAIN EXECUTION CODE STARTS HERE  **** */

    maininfo.display = NULL;
    colorinfo.nplanes = -1;
    imageinfo.pmin = 70000;
    imageinfo.pmax = -3000;
    imageinfo.headskip = 0;
    imageinfo.calibration = 0;
    imageinfo.fitsflag = 0;

#ifdef XRAY
    imageinfo.ein = 0;
    imageinfo.ros = 0;  
#endif

    imageinfo.VOP_Flags = VOP_GrayScale | VOP_Initialize;
    imageinfo.SOP_Flags = SOP_Linear;
    imageinfo.COP_Flags = 0;
    imageinfo.FOP_Flags = 0;

    shgetopt(argc, argv, &maininfo, &colorinfo, &imageinfo);

    /* create the cursors */
    make_cursors(&imcursor,&pancursor,&printcursor,
		 &zoom2cursor, &zoom4cursor, &palcursor);
    
    /* try for 8 planes (probably only gets 6) */
    if (colorinfo.nplanes < 0) 
      colorinfo.nplanes = MAX(DisplayPlanes()-4,8);
    
    /* iterate until cells allocated. Check if enough */
    if (XGetColorCells(1, 1, colorinfo.nplanes, 
		       &colorinfo.planes, &(colorinfo.pixels[0])) == 0) 
      {
          while(--colorinfo.nplanes > 2)
	    if(XGetColorCells(1,1,colorinfo.nplanes,
			      &colorinfo.planes,
			      &(colorinfo.pixels[0])) != 0)  break;
	  if(colorinfo.nplanes <= 2) {
	      fprintf(stderr,"Insufficient color planes resource!\n");
	      exit(1);
	  }
	  if(colorinfo.nplanes < 6) 
	    fprintf(stderr, "Allocated %d planes\n",colorinfo.nplanes);
      }

    colorinfo.ncolors = ipow(2, colorinfo.nplanes);

    /* figure out how many bits we have to left shift the pixels */
    colorinfo.shift = 0;
    i = colorinfo.pixels[0] | colorinfo.planes;
    while (((i >>= 1) & 1) == 0) colorinfo.shift++;

    /* get the menu selections ready */
    showmenu = menusetup(argv[0]);

    if(imageinfo.calibration) { /** GENERATE TEST SCREEN */
	imageinfo.ncols = imageinfo.nrows = TESTNROWS;
	rshift = DisplayPlanes() - colorinfo.nplanes - colorinfo.shift;
	if((imageinfo.image = 
	    (unsigned char *)malloc(imageinfo.nrows*imageinfo.ncols))
	   == NULL) {
	    fprintf(stderr,"Can't allocate test image!\n");
	    exit(1);
	}
	for (j = 0; j < imageinfo.nrows; j++)
	  for (i = 0; i < imageinfo.ncols; i++)
	    *(imageinfo.image + j*imageinfo.ncols + i) = 
	      ((((i + j)&255) >> rshift) & colorinfo.planes)
		+ colorinfo.pixels[0];
	
	wind = XCreate(argv[0], argv[0], 
		       maininfo.geometry, TESTDEFAULT, &maininfo.frame,
		       100, 134);
    }
    else {  /** READ IN IMAGE **/

#ifdef XRAY
	/* code and conditional added by egm */

	if( imageinfo.ein == 1 ){
	    eimgopen(imageinfo.ict, imageinfo.filename);
	}
	else if( imageinfo.ros == 1 ){
	    strcpy(imageinfo.poename, imageinfo.filename);
	    strcat(imageinfo.poename, ".poe");
	    strcpy(imageinfo.hdrname, imageinfo.filename);
	    strcat(imageinfo.hdrname, ".hdr");
	    openpoe(imageinfo.muthict, imageinfo.poename, imageinfo.hdrname);
	}
	else {
#endif

	    if((fd = open(imageinfo.filename,O_RDONLY,0)) < 0) {
		fprintf(stderr,"Error opening file %s\n",imageinfo.filename);
		exit(1);
	    }
	    if((imageinfo.header =
		readheader(fd,imageinfo.headskip,
			   &imageinfo.nrows,&imageinfo.ncols,
			   imageinfo.fitsflag))
	       == NULL) {
		   fprintf(stderr,"Error reading header of file %s\n",
			   imageinfo.filename);
		   exit(1);
	    }

#ifdef PALWIND
	    sprintf(maininfo.filegeometry,"=%dx%d%s",imageinfo.ncols, 
		    imageinfo.nrows, PICTDEFAULT);
#else
	    sprintf(maininfo.filegeometry,"=%dx%d%s",imageinfo.ncols,
		    imageinfo.nrows+PALHEIGHT+2*PALBORDER, PICTDEFAULT);
#endif	

#ifdef XRAY
	}  /* end of conditional code added by egm */
#endif

	if((imageinfo.image = 
	    (unsigned char *)malloc(imageinfo.nrows*imageinfo.ncols))
	   == NULL) {
	    fprintf(stderr,"Can't allocate byte image?\n");
	    exit(1);
	}

#ifdef XRAY
	/* conditional added by egm */

	if( imageinfo.ein == 1 ) {   /* Einstein XRAY image */
	    if((imageinfo.picture = 
		(short *)malloc(imageinfo.nrows*imageinfo.ncols*2)) == NULL) {
		  fprintf(stderr,"Can't allocate xray image?\n");
		  exit(1);
		}
	    eimgread(imageinfo.ict,imageinfo.picture, 
		     imageinfo.ncols, imageinfo.nrows, 
		     imageinfo.iy, imageinfo.iz, 
		     imageinfo.zoom,imageinfo.zoom, imageinfo.energy);
	}
	else if( imageinfo.ros == 1) {   /* ROSAT XRAY image */
	    selectcenter(imageinfo.muthict, imageinfo.iy, imageinfo.iz);
	    selectres(imageinfo.muthict, imageinfo.zoom, imageinfo.zoom);
	    if((imageinfo.picture = 
		(short *)malloc(imageinfo.nrows*imageinfo.ncols*2)) == NULL) {
		  fprintf(stderr,"Can't allocate xray image?\n");
		  exit(1);
	    }
	    readpoe(imageinfo.muthict, imageinfo.picture, 
		    imageinfo.ncols, imageinfo.nrows);
	}
	else { 
#endif
	  /* SAOCCD or FITS image */
	    if((imageinfo.picture =
		readpict(fd,imageinfo.nrows,imageinfo.ncols,
			 imageinfo.fitsflag)) == NULL) {
	        fprintf(stderr,"Error reading file %s\n",imageinfo.filename);
		exit(1);
	    }
	    close(fd);

#ifdef XRAY
	}  /* end of conditional added by egm */
#endif

	if(imageinfo.pmin == 70000 && imageinfo.pmax == -3000)
	  maxminpict(imageinfo.picture, imageinfo.nrows, imageinfo.ncols,
		     &imageinfo.pmax, &imageinfo.pmin);
	scalepict(imageinfo.image, imageinfo.picture, 
		  imageinfo.pmax, imageinfo.pmin,
		  colorinfo.ncolors, colorinfo.pixels[0], colorinfo.shift,
		  imageinfo.nrows, imageinfo.ncols, imageinfo.SOP_Flags);
	wind = XCreate(argv[0], argv[0], 
		       maininfo.geometry, maininfo.filegeometry,
		       &maininfo.frame, 100, 100);
    }
    if (wind == 0) { 
	fprintf(stderr, "XCreate on root failed\n");
	exit(1);
    }
	
    /* get current info so as to be ready for subwindow placing */
    XQueryWindow(wind, &winfo);
    
    if((iconwindow =
	XCreateWindow(RootWindow, 0, 0,
		      show_width, show_height, 0, 0, 0)) == 0)
      {
	  fprintf(stderr, "XCreateWindow on iconwindow failed\n");
	  exit(1);
      }
    XSetIconWindow(wind, iconwindow);

#ifdef PALWIND	
    if((palwind = XCreateWindow(RootWindow, winfo.x,
				winfo.y+winfo.height,
				winfo.width,
				PALHEIGHT, border_width,
				maininfo.frame.border, BlackPixmap))
       == 0) {
	   fprintf(stderr, "XCreateWindow on palwind failed\n");
	   exit(1);
       }
    XStoreName(palwind, argv[0]);
#else
    if((palwind = XCreateWindow(wind, 0,
				winfo.height-PALHEIGHT-2*PALBORDER,
				winfo.width-2*PALBORDER,
				PALHEIGHT, PALBORDER,
				WhitePixmap, BlackPixmap)) == 0) {
				    fprintf(stderr, "XCreateWindow on palwind failed\n");
				    exit(1);
				}
#endif
    if((wzoom = XCreateWindow(RootWindow, 
			      winfo.x+winfo.width+
			      maininfo.frame.bdrwidth,
			      winfo.y,
			      xzoomsize, yzoomsize, 
			      maininfo.frame.bdrwidth,
			      maininfo.frame.border, BlackPixmap)) == 0)
      {
	  fprintf(stderr, "XCreateWindow on wzoom failed\n");
	  exit(1);
      }
    XStoreName(wzoom, argv[0]);
    
    if((iconwzoom = XCreateWindow(RootWindow, 0, 0,
				   show_width, show_height, 0, 0, 0)) == 0) {
	fprintf(stderr, "XCreateWindow on iconwzoom failed\n");
	exit(1);
    }
    XSetIconWindow(wzoom, iconwzoom);

#ifdef PALWIND
    if((iconpalwind = XCreateWindow(RootWindow, 0, 0,
				   show_width, show_height, 0, 0, 0)) == 0) {
        fprintf(stderr, "XCreateWindow on iconpalwind failed\n");
        exit(1);
    }
    XSetIconWindow(palwind, iconpalwind);
#endif

    /* define the cursors */
    XUndefineCursor(wind);
    XDefineCursor(wind,imcursor);
    
    XUndefineCursor(palwind);
    XDefineCursor(palwind,palcursor);
    
    /* assume we display center of image in center of window */
    if(xcenter < 0) xcenter = imageinfo.ncols>>1;
    if(ycenter < 0) ycenter = imageinfo.nrows>>1;
    
    XSelectInput(wind, ButtonPressed | ButtonReleased | MouseMoved |
		 KeyPressed |
		 ExposeRegion | ExposeWindow | ExposeCopy | UnmapWindow);
    XSelectInput(iconwindow, ExposeWindow); 

    XSelectInput(wzoom, ExposeWindow | UnmapWindow);
    XSelectInput(iconwzoom, ExposeWindow); 

#ifdef PALWIND
    XSelectInput(palwind, ExposeWindow | UnmapWindow);
    XSelectInput(iconpalwind, ExposeWindow); 
#else
    XSelectInput(palwind, ExposeWindow);
#endif

    XMapWindow(wind);
    XMapWindow(wzoom);

#ifdef PALWIND
    XMapWindow(palwind);
#else
    XMapSubwindows(wind); 
#endif

    XFlush();

    while(1) {
	if(imageinfo.VOP_Flags & VOP_Initialize) {
	    initcmap(colorinfo.ncolors,colorinfo.cmap,
		     colorinfo.pixels[0],colorinfo.nplanes,
		     imageinfo.VOP_Flags);
	    XStoreColors(colorinfo.ncolors, colorinfo.cmap);
	    imageinfo.VOP_Flags &= ~(VOP_Initialize);
	}
	
	XNextEvent(&event);
	
#ifdef GLOPEROO
	describe_XEvent(event.type, event.window, but->detail,
			wind, palwind, wzoom);
#endif
	switch((int)event.type) {
	  case KeyPressed:

#ifdef VAX
	    keycode = but->detail & 0x00ff;
	    if(IsCursorKey(keycode)) {
		XUpdateMouse(wind,&mouse->x,&mouse->y,&subwind);
		i = 1<<zoomfactor;
		if(keycode == KC_CURSOR_UP)    mouse->y -= i;
		if(keycode == KC_CURSOR_DOWN)  mouse->y += i;
		if(keycode == KC_CURSOR_RIGHT) mouse->x += i;
		if(keycode == KC_CURSOR_LEFT)  mouse->x -= i;
		if(mouse->x < 0) mouse->x = 0;
		if(mouse->y < 0) mouse->y = 0;
		if(mouse->x >= winfo.width) mouse->x = winfo.width-1;
		if(mouse->y >= winfo.height-PALHEIGHT-2*PALBORDER)
		    mouse->y = winfo.height-PALHEIGHT-2*PALBORDER-1;
		XWarpMouse(wind,(int)mouse->x,(int)mouse->y);
	    }
#else
	    keystring = XLookupMapping(but, &j);
	         /* ANSI cursor keys are esc. seq. */
	    if(j > 0 && *keystring == '\033') {
		i = 1<<zoomfactor;
		if(strcmp(keystring,"\033[A") == 0) mouse->y -= i; /* up */
		if(strcmp(keystring,"\033[B") == 0) mouse->y += i; /* down */
		if(strcmp(keystring,"\033[C") == 0) mouse->x += i; /* right */
		if(strcmp(keystring,"\033[D") == 0) mouse->x -= i; /* left */
		if(mouse->x < 0) mouse->x = 0;
		if(mouse->y < 0) mouse->y = 0;
		if(mouse->x >= winfo.width) mouse->x = winfo.width-1;
		if(mouse->y >= winfo.height-PALHEIGHT-2*PALBORDER)
		XWarpMouse(wind,(int)mouse->x,(int)mouse->y);
	    }
#endif
	    break;

	  case ButtonPressed:   /* Activate Menu ? */
	    if((but->detail & 
		(ControlMask | MetaMask | ShiftMask | ShiftLockMask))
	       == ControlMask)
	      {
		  XQueryWindow(wind, &winfo);
		  XMenuSetFreeze(showmenu);
		  pane = lastpane;
		  sel = lastsel;
		  if(XMenuActivate(showmenu,&pane,&sel,
				   but->x + winfo.x, but->y + winfo.y,
				   ButtonReleased, &currentflags)
		     == XM_SUCCESS) {  /* check out the selection */
			 lastpane = pane;
			 lastsel = sel;

			 switch(*currentflags & PANEMASK) {
		      /* EXIT */
			   case 0: 
			     exit(0); /* normal exit */

		      /* Set `viewing mode' - manipulation of color map */
			   case VOP:
			     imageinfo.VOP_Flags &= ~(currentflags[1]);
			     imageinfo.VOP_Flags |= 
			       (*currentflags & SELMASK);
			     XUndefineCursor(wind);
			     XDefineCursor(wind, imcursor);
			     imageinfo.COP_Flags = 0;
			     break;

                      /* Set cursor mode - other uses than above */
			   case COP:
			     /* if in test mode, disable all but viewing */
			     if(imageinfo.calibration) break;
			     imageinfo.COP_Flags &= ~(currentflags[1]);
			     imageinfo.COP_Flags |= 
			       (*currentflags & SELMASK);
			     if(imageinfo.COP_Flags & COP_Print) {
				 XUndefineCursor(wind);
				 XDefineCursor(wind, printcursor);
			     } else if(imageinfo.COP_Flags & COP_Pan) {
				 XUndefineCursor(wind);
				 XDefineCursor(wind, pancursor);
			     } else if(imageinfo.COP_Flags & COP_Zoom2) {
				 XUndefineCursor(wind);
				 XDefineCursor(wind, zoom2cursor);
			     } else if(imageinfo.COP_Flags & COP_Zoom4) {
				 XUndefineCursor(wind);
				 XDefineCursor(wind, zoom4cursor);
			     }
			     break;

		     /* set picture scaling mode */
			   case SOP:
			     /* if in test mode, disable all but viewing */
			     if(imageinfo.calibration) break;
			     /* first, see if any different */
			     if((imageinfo.SOP_Flags & 
				 (*currentflags & SELMASK)) == 0) 
			       {
				   imageinfo.SOP_Flags &= 
				     ~(currentflags[1]);
				   imageinfo.SOP_Flags |= 
				     (*currentflags & SELMASK);
				   scalepict(imageinfo.image, 
					     imageinfo.picture,
					     imageinfo.pmax, imageinfo.pmin,
					     colorinfo.ncolors, 
					     colorinfo.pixels[0],
					     colorinfo.shift, 
					     imageinfo.nrows, 
					     imageinfo.ncols, 
					     imageinfo.SOP_Flags);
				   writepix( wind, imageinfo.image, 
					    imageinfo.ncols, 
					    imageinfo.nrows,
					    zoomfactor, 0, 0, 
					    winfo.width, winfo.height,
					    xzero, yzero, 0, GXcopy,
					    AllPlanes);
			       }
			     break;

			   case FOP:
			     /* if in test mode, disable all but viewing */
			     if(imageinfo.calibration) break;
			     break;
			       
			   default:
			     fprintf(stderr,"Unknown menu pane %d\n",
				     *currentflags>>16);
			 }
		     }
		  break;
	      }
	    else if((but->detail &
		     (ControlMask | MetaMask | ShiftMask |
		      ShiftLockMask)) == 0)
	      {
		  /* check if viewing or cursor operation selected */
		  if(imageinfo.COP_Flags) break;
		  downbutton = but->detail &
		    (RightButton | MiddleButton | LeftButton);
		  CalcMap(&winfo,colorinfo.cmap,(int)but->x,(int)but->y,
			  colorinfo.ncolors, imageinfo.VOP_Flags,
			  downbutton);
		  XStoreColors(colorinfo.ncolors,colorinfo.cmap);
		  break;
	      }
	    break;

	  case ButtonReleased:
	    /* check if viewing or cursor operation selected */
	    if(imageinfo.COP_Flags) {
		if(imageinfo.COP_Flags & COP_Print) {  
		    /* print 11x11 picture region */
		    prpict(imageinfo.picture, but->x>>zoomfactor,
			   but->y>>zoomfactor, xzero, yzero,
			   imageinfo.ncols, imageinfo.nrows, 11, 11);
		}
		/* recenter (pan) on cursor position */
		if(imageinfo.COP_Flags & 
		   (COP_Pan | COP_Zoom2 | COP_Zoom4)) {
			
		       xcenter = xzero + (but->x >> zoomfactor);
		       ycenter = yzero + (but->y >> zoomfactor);
			
		       /* alter zoomfactor to reflect new zoom */
		       if(imageinfo.COP_Flags & COP_Pan) zoomfactor = 0;
		       if(imageinfo.COP_Flags & COP_Zoom2) zoomfactor = 1;
		       if(imageinfo.COP_Flags & COP_Zoom4) zoomfactor = 2;
			
		       xtempzero = 
			  MAX( 0, MIN(imageinfo.ncols - 
				      (winfo.width>>zoomfactor),
				    xcenter - (winfo.width>>(zoomfactor+1))));
			
#ifdef PALWIND
		       ytempzero =
			  MAX( 0, MIN(imageinfo.nrows-
				   (winfo.height>>zoomfactor),
				   ycenter - (winfo.height>>(zoomfactor+1))));
#else
		       ytempzero =
			  MAX( 0, MIN(imageinfo.nrows-
				      ((winfo.height-PALHEIGHT-2*PALBORDER)
				       >>zoomfactor),
				  ycenter - ((winfo.height-
					      PALHEIGHT-2*PALBORDER)
					     >>(zoomfactor+1))));
#endif
		       if(xzero != xtempzero || yzero != ytempzero ||
			  oldzoomfactor != zoomfactor)
			 {
			     xzero = xtempzero;
			     yzero = ytempzero;
			     writepix( wind, imageinfo.image, 
				      imageinfo.ncols, imageinfo.nrows,
				      zoomfactor, 0, 0, 
				      winfo.width, winfo.height,
				      xzero, yzero, 0, GXcopy, AllPlanes);
			     XWarpMouse(wind, (xcenter-xzero)<<zoomfactor, 
					(ycenter-yzero)<<zoomfactor);
			 }
		    /* COP_OldFlags = imageinfo.COP_Flags;  */
		       oldzoomfactor = zoomfactor;
		   }
		break;
	    }
	    if(downbutton < 0) break;
	    CalcMap(&winfo,colorinfo.cmap,(int)but->x,(int)but->y,
		    colorinfo.ncolors, imageinfo.VOP_Flags,
		    downbutton);
	    XStoreColors(colorinfo.ncolors,colorinfo.cmap);
	    downbutton = -1;
	    break;

	  case MouseMoved:
	    XUpdateMouse(wind, &xbut, &ybut, &subwind);
	    XQueryMouseButtons(wind, &xbut, &ybut,
			       &subwind, &mouse->detail);
	    
	    if(subwind == 0 && downbutton >= 0) {
		CalcMap(&winfo,colorinfo.cmap,xbut,ybut,
			colorinfo.ncolors,
			imageinfo.VOP_Flags,downbutton);
		XStoreColors(colorinfo.ncolors,colorinfo.cmap);
	    }
	    else { 
		updatezoom(wzoom, xzoomsize, yzoomsize,
			   (xzero + (but->x >> zoomfactor)),
			   (yzero + (but->y >> zoomfactor)),
			   imageinfo.ncols, imageinfo.nrows, 
			   imageinfo.image, (1<<zoomfactor));
	    }
	    break;
		
	    /* On ExposeWindow events, recalculate the image center and
	     * coordinates of upper left hand corner of window.
	     */
	  case ExposeCopy: 
	  case ExposeWindow:
	    if(QLength() > 0) {
		XPeekEvent(&peekevent);
		if(peekevent.type == ExposeWindow &&
		   peekevent.window == expw->window) break;
	    }
	    
	    if(expw->window == iconwindow) {
		XBitmapBitsPut(iconwindow, 0, 0, 
			       show_width, show_height, show_bits,
			       WhitePixel, BlackPixel, 0,
			       GXcopy, AllPlanes);
		break;
	    }
	    
#ifdef PALWIND
	    if(expw->window == iconpalwind) {
		XBitmapBitsPut(iconpalwind, 0, 0, 
			       show_width, show_height, show_bits,
			       WhitePixel, BlackPixel, 0,
			       GXcopy, AllPlanes);
		break;
	    }
#endif
	    if(expw->window == iconwzoom) {
		XBitmapBitsPut(iconwzoom, 0, 0, 
			       show_width, show_height, show_bits,
			       WhitePixel, BlackPixel, 0,
			       GXcopy, AllPlanes);
		break;
	    }
		
	    if(w_mapped == 0) {  /* check if to remap windows */
#ifdef PALWIND
		if(expw->window != palwind) XMapWindow(palwind);
#endif
		if(expw->window != wind)    XMapWindow(wind);
		if(expw->window != wzoom)   XMapWindow(wzoom);
		w_mapped++;
	    }
		
	    XQueryWindow(wind,&winfo);
		
	    if(expw->window == wind) {
		XQueryWindow(palwind,&palwinfo);
#ifdef PALWIND
		XConfigureWindow(palwind, winfo.x,
				 winfo.y+winfo.height+
				 maininfo.frame.bdrwidth,
				 winfo.width, PALHEIGHT);
#else
		XConfigureWindow(palwind, 0,
				 winfo.height-PALHEIGHT-2*PALBORDER,
				 winfo.width-2*PALBORDER,
				 PALHEIGHT);
#endif
		XQueryWindow(wzoom,&wzoominfo);
		xzoomsize = wzoominfo.width;
		yzoomsize = wzoominfo.height;
		XConfigureWindow(wzoom,
				 winfo.x+winfo.width+
				 maininfo.frame.bdrwidth,
				 winfo.y, wzoominfo.width,
				 wzoominfo.height);
		
		xzero = MAX( 0, 
			    MIN(imageinfo.ncols-(winfo.width>>zoomfactor),
				xcenter - 
				(winfo.width>>(zoomfactor+1))));
#ifdef PALWIND
		yzero = MAX( 0, 
			    MIN(nrows-
				(winfo.height>>zoomfactor),
				ycenter -
				(winfo.height>>(zoomfactor+1))));
#else
		yzero = MAX( 0, 
			    MIN(imageinfo.nrows-
				((winfo.height-PALHEIGHT-2*PALBORDER)
				 >>zoomfactor),
				ycenter - ((winfo.height -
					    PALHEIGHT-2*PALBORDER)
					   >>(zoomfactor+1))));
#endif
	    }
	    
	    if(event.type == ExposeCopy) break;

	    /* drop through to ExposeRegion to draw the window, and
	     * redraw the supporting windows */

	  case ExposeRegion:
	    if(expw->window == palwind) {
		XQueryWindow(palwind,&palwinfo);
		for(i=0; i<palwinfo.width; i++)
		  palette[i] =
		    ((colorinfo.ncolors * i)/palwinfo.width) + 
		      colorinfo.pixels[0];
		for(i=0; i<palwinfo.height; i++)
		  XPixmapBitsPutZ(palwind, 0, i, 
				  palwinfo.width, 1,
				  palette, 0, GXcopy, AllPlanes);
	    } 
	    else if(expw->window == wzoom) {
		XQueryWindow(wzoom,&wzoominfo);
		xzoomsize = wzoominfo.width;
		yzoomsize = wzoominfo.height;
		updatezoom(wzoom, xzoomsize, yzoomsize,
			   xcenter, ycenter, 
			   imageinfo.ncols, imageinfo.nrows,
			   imageinfo.image, (1<<zoomfactor));
	    } 
	    else {
		if( expw->x > imageinfo.ncols<<zoomfactor ||
		   expw->y > imageinfo.nrows<<zoomfactor)
		  break;
		
		writepix( wind, imageinfo.image,
			 imageinfo.ncols, imageinfo.nrows,
			 zoomfactor, expw->x, expw->y, 
			 expw->width, expw->height,
			 xzero, yzero, 0, GXcopy, AllPlanes);
	    }
	    break;
		
	/* If any window is unmapped, all three are unmapped */

	  case UnmapWindow:

#ifdef PALWIND
	    if(event.window != palwind) XUnmapWindow(palwind);
#endif
	    if(event.window != wzoom)   XUnmapWindow(wzoom);
	    if(event.window != wind)     XUnmapWindow(wind);
	    w_mapped = 0;
	    break;
	}
    }
}


#ifdef GLOPEROO
describe_XEvent(etype, ewindow, detail, wind, palwind, wzoom)
     unsigned long etype;
     unsigned short detail;
     Window ewindow, wind, palwind, wzoom;
{
    fprintf(stderr,"Window ");
    if(ewindow == wind) {
	fprintf(stderr,"wind, ");
    } else if(ewindow == palwind) {
	fprintf(stderr,"palwind, ");
    } else if(ewindow == wzoom) {
	fprintf(stderr,"wzoom, "); 
    } else {
	fprintf("ERROR %d\n", ewindow);
    }
    switch((int)etype) {
      case KeyPressed: fprintf(stderr,"KeyPressed"); 
		       fprintf(stderr," detail = %x", detail);
		       break;
      case KeyReleased: fprintf(stderr,"KeyReleased");
		       fprintf(stderr," detail = %x", detail);
		       break;
      case ButtonPressed: fprintf(stderr,"ButtonPressed");
		       fprintf(stderr," detail = %x", detail);
		       break;
      case ButtonReleased: fprintf(stderr,"ButtonReleased");
		       fprintf(stderr," detail = %x", detail);
		       break;
      case EnterWindow: fprintf(stderr,"EnterWindow");
		       fprintf(stderr," detail = %x", detail);
		       break;
      case LeaveWindow: fprintf(stderr,"LeaveWindow");
		       fprintf(stderr," detail = %x", detail);
		       break;
      case MouseMoved: fprintf(stderr,"MouseMoved");
		       fprintf(stderr," detail = %x", detail);
		       break;
      case ExposeWindow: fprintf(stderr,"ExposeWindow"); break;
      case ExposeRegion: fprintf(stderr,"ExposeRegion"); break;
      case ExposeCopy: fprintf(stderr,"ExposeCopy"); break;
      case UnmapWindow: fprintf(stderr,"UnmapWindow"); break;
      case FocusChange: fprintf(stderr,"FocusChange"); break;
      default: fprintf(stderr,"ERROR %d", etype);
		   }
    fprintf(stderr,"\n");
    fflush(stderr);
}
#endif
