#include "wm.h"

#ifndef lint
static char *rcsid_wmsubs_c = "$Header: xnwm.c,v 10.6 86/11/19 19:31:32 jg Rel $";
#endif

#define default_frame_width 5
#define default_mask MetaMask
#define default_menu_font "8x13"
#define default_size_font "8x13"
#define default_icon_font "8x13"
#define default_iconify_delta 5
#define default_menu_x 0
#define default_menu_y 0

/* Default button assignments; the numbers are indices into the
   menuOp array */

#define default_left 0
#define default_middle 1
#define default_right 2
#define unassigned_button -1
#define popup_button -2

/* Convert 0,1,2 into XRight(Middle,Left)Mask */
#define Button(i) (1 << (8 + (i)))

#include "wm.cursor"

#include "buttons.raster"
Pixmap buttonPixmap[3];

Window menu;
typedef int (*ptrToFunc) ();
int Raise(), Lower(), Move(), Resize(), IconifyOrDeIconify(),
	Select(), Circulate(), Assign(), Unassigned();
char *label[] = {"Select", "Raise", "Move", "(De)Iconify",
	"Lower", "Resize", "Circulate", "Assign", ""};
ptrToFunc menuOp[] = {Select, Raise, Move, IconifyOrDeIconify,
	Lower, Resize, Circulate, Assign};
int needsWindow[] = {TRUE, TRUE, TRUE, TRUE,
	TRUE, TRUE, FALSE, TRUE};
int canDoToMenu[] = {FALSE, TRUE, TRUE, FALSE,
	TRUE, FALSE, TRUE, TRUE};
/* "item" needs an extra entry at the end */
Window item[] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
int buttonFunc[3] = {default_right, default_middle, default_left};
int itemcount, itemWidth, itemHeight;
int menuWidth, menuHeight, menuX, menuY;
char code[] = "srmilzc";	/* button assignment codes for command line */
int skinny = FALSE, twoRows = FALSE, vertical = FALSE;
int lastMenuOp = -1;
int doingOp = FALSE;

int savex, savey;		/* Where and what the popup menu obscures */
Pixmap saveScreen;
Pixmap menuImage = 0;		/* What the popup menu looks like */
int popupSaved = FALSE;		/* True if a popup image is saved */

int reverse;			/* whether to reverse the normal colors */
int buttonGXfunc;		/* function to put up the button picture */

char *index();

main(argc, argv)
	int argc;
	char **argv;
{
	Window w;
	BEvent button;
	int raisedMenu = FALSE;

	SetUpEverything(argc, argv);

	while (TRUE) {
	    GetButton(&button);

	    /* If it's a ButtonReleased ignore; it's just left
	       over from some old click */

	    if (button.type == ButtonReleased) continue;

	    InterpretLocatorW(RootWindow, &w, button.location);

	    /* If in background, raise menu or move back if just raised.
	       If using popup windows, map the popup window.  Only do this
	       as long as the button is not bound to Select */

	    if (w == 0 && NonSelect(&button)) {
		if (popup) MapPopup(button.location);
		else {
		    if (raisedMenu) XMoveWindow(menu, menuX, menuY);
		    else XRaiseWindow(menu);
		    raisedMenu = TRUE;
		}
		continue;
	    } else raisedMenu = FALSE;

	    /* If in menu, do appropriate function */

	    if (w == menu) {
		ProcessMenuFunction(&button);
		if (popup) UnmapPopup();
		continue;
	    }

	    /* Otherwise do preassigned button function */

	    DoButtonFunc(&button, w);
	}
}

SetUpEverything(argc, argv)
	int argc;
	char **argv;
{
	char *mfname = default_menu_font;	/* font to use */
	char *sfname = default_size_font;	/* font to use */
	char *ifname = default_icon_font;	/* font to use */
	char *location = "";
	char display[256];		/* display to use */

	frameWidth = default_frame_width;
	mask = default_mask;
	iconifyDelta = default_iconify_delta;
	menuX = menuY = 0;
	freeze = TRUE;
	popup = FALSE;
	reverse = FALSE;

	GetDefaults(argv[0], &mfname, &sfname, &ifname, &location);
	ProcessArgs(argc, argv, display, &mfname, &sfname, &ifname, &location);
	OpenDisplay (display);
	StoreCursors();
	StorePixmaps();
	SetUpFonts(mfname, sfname, ifname);
	SetUpMenu(location);
	GrabButtons();
	InitializeWm();
}

GetDefaults(progname, mfname, sfname, ifname, location)
	char *progname;
	char **mfname, **sfname, **ifname, **location;
{
	register char *option;
	int newmask = 0;

	if ((option = XGetDefault(progname,"MenuFont")) != NULL) {
	    *mfname = option;
	}
	if ((option = XGetDefault(progname,"SizeFont")) != NULL) {
	    *sfname = option;
	}
	if ((option = XGetDefault(progname,"IconFont")) != NULL) {
	    *ifname = option;
	}	
	if ((option = XGetDefault(progname,"ReverseVideo")) != NULL) {
	    if (strcmp(option, "on") == 0) reverse = TRUE;
	}
	if ((option = XGetDefault(progname,"FrameWidth")) != NULL) {
	    frameWidth = atoi(option);
	}
	if ((option = XGetDefault(progname,"IconifyDelta")) != NULL) {
	    iconifyDelta = atoi(option);
	}
	if ((option = XGetDefault(progname,"MenuFormat")) != NULL) {
	    if (index(option, 't') != NULL) skinny = TRUE;
	    if (index(option, '2') != NULL) twoRows = skinny = TRUE;
	    if (index(option, 'v') != NULL) vertical = skinny = TRUE;
	}
	if ((option = XGetDefault(progname,"Freeze")) != NULL) {
	    if (strcmp(option, "off") == 0) freeze = FALSE;
	}
	if ((option = XGetDefault(progname,"KeyCombination")) != NULL) {
	    if (index(option, 'c') != NULL) newmask |= ControlMask;
	    if (index(option, 's') != NULL) newmask |= ShiftMask;
	    if (index(option, 'm') != NULL) newmask |= MetaMask;
	    if (index(option, 'l') != NULL) newmask |= ShiftLockMask;
	    if (index(option, 'n') != NULL) mask = 0;
	    else if (newmask) mask = newmask;
	}
	if ((option = XGetDefault(progname,"LeftButton")) != NULL) {
	    AssignButton(LeftButton, option[0]);
	}
	if ((option = XGetDefault(progname,"MiddleButton")) != NULL) {
	    AssignButton(MiddleButton, option[0]);
	}
	if ((option = XGetDefault(progname,"RightButton")) != NULL) {
	    AssignButton(RightButton, option[0]);
	}
	if ((option = XGetDefault(progname,"Geometry")) != NULL) {
	    *location = option;
	}

}

ProcessArgs(argc, argv, display, mfname, sfname, ifname, location)
	int argc;
	char **argv;
	char *display;
	char **mfname, **sfname, **ifname, **location;
{
	int i, newmask = 0, none = FALSE;
	register char *arg;

	for (i = 1; i < argc; i++) {
	    arg = argv[i];
	    switch (*arg) {
		case '\0':
		    continue;

		case '-':
		    arg++;
		    if (*arg == '\0') mask = 0;
		    else {
			newmask = 0;
			for (; *arg; arg++) {
			    switch (*arg) {
				case 'h':
				    Syntax();
				    break;

				case 'c':
				    newmask |= ControlMask;
				    break;

				case 's':
				    newmask |= ShiftMask;
				    break;

				case 'm':
				    newmask |= MetaMask;
				    break;

				case 'l':
				    newmask |= ShiftLockMask;
				    break;

				case 'n':
				    none = TRUE;
				    break;

				case 'f':
				    freeze = FALSE;
				    break;				

				case 'r':
				    reverse = TRUE;
				    break;

				case '2':
				    twoRows = skinny = TRUE;
				    vertical = FALSE;
				    break;

				case 'v':
				    vertical = skinny = TRUE;
				    twoRows = FALSE;
				    break;

				case 't':
				    skinny = TRUE;
				    vertical = twoRows = FALSE;
				    break;

				default:
				    Syntax();
				    break;
			    }
			}
			if (newmask) mask = newmask;
			if (none) mask = 0;
		    }
		    break;

		case '@':
		    frameWidth = atoi(arg+1);
		    if (frameWidth < 0 || frameWidth > 100) {
			errno = EDOM;
			perror("xnwm:");
		    }
		    break;

		case '%':
		    iconifyDelta = atoi(arg+1);
		    break;

		case '=':
		    *location = arg+1;
		    break;

		default:
		    if (arg[1] == '=') {
			switch (*arg) {
			    case 'l':
			        AssignButton(LeftButton, arg[2]);
				break;

			    case 'm':
			        AssignButton(MiddleButton, arg[2]);
				break;

			    case 'r':
			        AssignButton(RightButton, arg[2]);
				break;

			    default:
				Syntax();
				break;				
			}
		    } else if (arg[0] == 'f' && arg[2] == '=') {
			switch (arg[1]) {
			    case 'm':
				*mfname = arg+3;
				break;

			    case 's':
				*sfname = arg+3;
				break;

			    case 'i':
				*ifname = arg+3;
				break;

			    default:
				Syntax();
				break;
			}
		    } else strcpy(display, arg);
	    }
	}

	if (buttonFunc[RightButton] == popup_button ||
		buttonFunc[MiddleButton] == popup_button ||
		buttonFunc[LeftButton] == popup_button) {
    	    popup = vertical = skinny = TRUE;
	}
}

AssignButton(which, arg)
	int which;
	char arg;
{
	char *ch;

	if (arg == '\0') {
	    buttonFunc[which] = unassigned_button;
	    return;
	} else if (arg == 'p') {
	    buttonFunc[which] = popup_button;
	    return;
	}
	ch = index(code, arg);
	if (ch == NULL) {
	    errno = EDOM;
	    perror ("xnwm:");
	} else buttonFunc[which] = ch - code;
}

Syntax()
{
	puts("Usage: xnwm {-csmln} {-f} {-r} {-2vt} {@[framewidth]}");
	puts("       {%[iconifyDelta]} {=[{+-}xoff[{+-}yoff]]}");
	puts("       {l={srmilzcp}} {m={srmilzcp}} {r={srmilzcp}}");
	puts("       {fm=menu_font} {fs=size_font} {fi=icon_font}");
	puts("       {[host]:vs}");	/* whew */

	exit(0);
}

OpenDisplay(display)
	char *display;
{
	WindowInfo winfo;

	if (XOpenDisplay(display) == NULL) {
	    fprintf(stderr, "%s: Can't open display '%s'\n",
		    "xnwm" , XDisplayName(display));
	    exit(1);
	}

	QueryWindow(RootWindow, &winfo);
	screen_height = winfo.height;
	screen_width = winfo.width;

	XSelectInput(RootWindow,
		ButtonPressed | ButtonReleased | UnmapWindow | FocusChange);

	if (reverse) {
	    bgColor = BlackPixel;
	    bgPixmap = BlackPixmap;
	    fgColor = WhitePixel;
	    fgPixmap = WhitePixmap;
	    buttonGXfunc = GXor;
	} else {
	    bgColor = WhitePixel;
	    bgPixmap = WhitePixmap;
	    fgColor = BlackPixel;
	    fgPixmap = BlackPixmap;
	    buttonGXfunc = GXand;
	}
}

StoreCursors()
{
	wmCursor = XCreateCursor(nwm_width, nwm_height, 
		(caddr_t) nwm_bits, (caddr_t) NULL,
		1, 1,
		fgColor, bgColor,
		GXcopyInverted);
	if (wmCursor == NULL) {
	    Error("Couldn't store wmCursor in StoreCursors");
	}
}

StorePixmaps()
{
	static short gray_bits[] = {
	    0xaaaa, 0x5555, 0xaaaa, 0x5555,
	    0xaaaa, 0x5555, 0xaaaa, 0x5555,
	    0xaaaa, 0x5555, 0xaaaa, 0x5555,
	    0xaaaa, 0x5555, 0xaaaa, 0x5555
	};

	buttonPixmap[RightButton] =
		MakePixmap(button_width, button_height,
		(caddr_t) rbutton_bits, bgColor, fgColor);

	buttonPixmap[MiddleButton] =
		MakePixmap(button_width, button_height,
		(caddr_t) mbutton_bits, bgColor, fgColor);

	buttonPixmap[LeftButton] =
		MakePixmap(button_width, button_height,
		(caddr_t) lbutton_bits, bgColor, fgColor);

	gray = MakePixmap(16, 16,
		(caddr_t) gray_bits, bgColor, fgColor);
}

Pixmap MakePixmap(width, height, bits, fg, bg)
	int width, height;
	caddr_t bits;
	int fg, bg;
{
	Bitmap b;
	Pixmap p;

	b = XStoreBitmap(width, height, bits);
	if (b == NULL) Error("Couldn't store bitmap in MakePixmap");
	p = XMakePixmap (b, fg, bg);
	if (p == NULL) Error("Couldn't make pixmap in MakePixmap");
	XFreeBitmap(b);
	return p;
}

SetUpFonts(mfname, sfname, ifname)
	char *mfname, *sfname, *ifname;
{
	menufont = XGetFont(mfname);
	if (menufont == NULL) Error("Couldn't store menu font in SetUpFont");
	sizefont = XGetFont(sfname);
	if (sizefont == NULL) Error("Couldn't store size font in SetUpFont");
	iconfont = XGetFont(ifname);
	if (iconfont == NULL) Error("Couldn't store icon font in SetUpFont");
}

SetUpMenu(location)
	char *location;
{
	register int i = 0;
	int width = 0, textwidth;
	int x, y;
	char xSign, ySign;

	while (label[i][0] != '\0') {
	    textwidth = XQueryWidth(label[i], menufont);
	    if (textwidth == 0) {
		Error("Couldn't query string width in SetUpMenu");
	    }
	    if (textwidth > width) width = textwidth;
	    i++;
	}

	itemcount = i;
	if (skinny) {
	    itemWidth = 1 + 3 + width + 3 + button_width + 3 + 1;
	} else itemWidth = screen_width / itemcount;
	itemHeight = 20;

	if (vertical) {
	    menuWidth = itemWidth;
	    menuHeight = itemcount * itemHeight;
	} else if (twoRows) {
	    menuWidth = itemWidth * ((itemcount+1) >> 1);
	    menuHeight =  itemHeight * 2;
	} else {
	    menuWidth = itemWidth * itemcount;
	    menuHeight = itemHeight;
	}
	if (!popup && *location != '\0') {
	    /* Interpret the location string */

	    InterpLocation(location, &x, &y, &xSign, &ySign);

	    if (xSign == '+') menuX = x;
	    else menuX = screen_width - x - menuWidth;

	    if (ySign == '+') menuY = y;
	    else menuY = screen_height - y - menuHeight;
	}

	menu = XCreateWindow(RootWindow, menuX, menuY, menuWidth, menuHeight,
		1, fgPixmap, bgPixmap);
	if (menu == NULL) Error ("Couldn't open menu in SetUpMenu");
	XStoreName(menu, "xnwm menu");
	XDefineCursor(menu, wmCursor);

	if (popup) XSelectInput(menu, LeaveWindow);
	else XSelectInput(menu, ExposeWindow);

	if (!popup) {
	    x = y = -1;

	    for (i = 0; i < itemcount; i++) {
		if (twoRows && i == (itemcount+1) >> 1) {
		    y = itemHeight;
		    x = -1;
		}
		item[i] = XCreateWindow (menu, x, y, itemWidth, itemHeight, 1,
			fgPixmap, bgPixmap);
		if (item[i] == NULL) Error ("Couldn't open item in SetUpMenu");
		if (vertical) y += itemHeight;
		else x += itemWidth;
	    }
	    XMapSubwindows(menu);
	    XMapWindow(menu);
	}

	menuWidth += 2;		/* Consider the border in from now on */
	menuHeight += 2;

	/* Don't draw anything; the expose event will cause that */
}

InterpLocation(location, x, y, xSign, ySign)
	register char *location;
	register int *x, *y;
	char *xSign, *ySign;
{
	*xSign = *location;

	switch (*location) {
	    case '+':
	    case '-':
		location++;
		*x = 0;
		while(*location >= '0' && *location <= '9') {
		    *x = 10 * *x + (*location++ - '0');
		}
		break;

	    default:
		Syntax();
		break;
	}

	*ySign = *location;

	switch (*location) {
	    case '+':
	    case '-':
		location++;
		*y = 0;
		while(*location >= '0' && *location <= '9') {
		    *y = 10 * *y + (*location++ - '0');
		}
		break;

	    default:
		Syntax();
		break;
	}

	if (*location != '\0') Syntax();
}

GrabButtons()
{
	register int i;

	for (i = 0; i < 3; i++) {
	    if (buttonFunc[i] != unassigned_button) {
		status = XGrabButton(RootWindow, wmCursor,
			mask | Button(i),
			ButtonPressed|ButtonReleased);
		if (status == NULL) {
		    Error("Couldn't grab button in GrabButtons");
		}
	    }
	}
}

ProcessMenuFunction(button)
	BEvent *button;
{
	Window w;
	register int i = 0;

	InterpretLocatorW(menu, &w, button->location);
	while (item[i] != NULL && item[i] != w) i++;

	if (item[i]) {
	    InvertButton(i);
	    DoOperation(i, button->detail);
	    DisplayButton(i);
	    lastMenuOp = i;
	}
}

DoOperation(i, which)
	int i, which;
{
	BEvent newbutton;
	Window w;
	WindowInfo winfo;

	/* First wait for the upbutton; if it doesn't occur or
	   occurs in a different window abort */

	GetButton(&newbutton);
	InterpretLocatorW(menu, &w, newbutton.location);
	if (!MatchUp(newbutton, which) || w != item[i]) return;

	doingOp = TRUE;

	/* If the function needs a window, get one */

	if (needsWindow[i]) {
	    status = XGrabMouse(RootWindow, wmCursor,
		    ButtonPressed | ButtonReleased);
	    if (status  == NULL) Error ("Couldn't grab mouse in DoOperation");
	    GetButton(&newbutton);

	    /* This should be the same button as was pushed to select
	       the function */

	    if (!MatchDown(newbutton, which)) {
		XUngrabMouse();
		doingOp = FALSE;
		return;
	    }

	    InterpretLocatorW(RootWindow, &w, newbutton.location);
	    if (w == 0) w = RootWindow;
	    QueryWindow(w, &winfo);
	}

	/* Now call the appropriate function */

	if (canDoToMenu[i] || w != menu) {
	    (*menuOp[i])(which, newbutton.location, w, &winfo);
	}
	doingOp = FALSE;

	if (needsWindow[i]) XUngrabMouse();
}

/* Returns whether or not the button is bound to Select */

int NonSelect(button)
	BEvent *button;
{
	int which = button->detail & 0xff;	/* 0, 1, or 2 */
	int func = buttonFunc[which];

	return (func < 0 || menuOp[func] != Select);
}

DoButtonFunc(button, w)
	BEvent *button;
	Window w;
{
	int which = button->detail & 0xff;	/* 0, 1, or 2 */
	register int func = buttonFunc[which];
	WindowInfo winfo;

	if (func == unassigned_button) {
	    Unassigned();
	    return;
	}

	/* popup_button signifies the button to map the popup window */

	if (func == popup_button) {
	    MapPopup(button->location);
	    return;
	}

	if (w == 0) w = RootWindow;

	if (needsWindow[func]) {
	    status = XGrabMouse(RootWindow, wmCursor,
		    ButtonPressed|ButtonReleased);
	    if (status == 0) Error ("Couldn't grab mouse in DoButtonFunc");
	    QueryWindow(w, &winfo);
	}

	InvertButton(func);
	(*menuOp[func])(which, button->location, w, &winfo);
	DisplayButton(func);

	if (needsWindow[func]) XUngrabMouse();
}

Unassigned()
{
	XFeep (3);
}

MapPopup(loc)
	Locator loc;
{
	int x, y;
	Window w;
	register int i;

	/* If there's not a saved popup image, create the buttons */

	if (!popupSaved) {	/* Create the items */
	    XDestroySubwindows(menu);	/* Get rid of old items */
	    x = y = -1;
	    for (i = 0; i < itemcount; i++) {
		item[i] = XCreateWindow (menu, x, y, itemWidth, itemHeight, 1,
			fgPixmap, bgPixmap);
		if (item[i] == NULL) Error ("Couldn't open item in MapPopup");
		y += itemHeight;
	    }
	    XMapSubwindows(menu);
	}

	InterpretLocatorXY(RootWindow, &x, &y, loc);

	x -= menuWidth >> 1;
	if (lastMenuOp == -1) y -= menuHeight >> 1;
	else y -= lastMenuOp * (itemHeight+1) + (itemHeight >> 1);

	if (x < 0) x = 0;
	else if (x + menuWidth > screen_width) {
	    x = screen_width - menuWidth;
	}

	if (y < 0) y = 0;
	else if (y + menuHeight > screen_height) {
	    y = screen_height - menuHeight;
	}

	if (freeze) {
	    savex = x;
	    savey = y;
	    XGrabServer();
	    saveScreen = XPixmapSave(RootWindow, x, y, menuWidth, menuHeight);
	    if (saveScreen == 0) Error("Couldn't save screen in MapPopup");
	}

	XMoveWindow(menu, x, y);
	XMapWindow(menu);

	if (popupSaved) {
	    XPixmapPut(menu, 0, 0, 0, 0, menuWidth-2, menuHeight-2,
		    menuImage, GXcopy, AllPlanes);
	} else {
	    for (i = 0; i < itemcount; i++) DisplayButton(i);;
	    menuImage = XPixmapSave(menu, 0, 0, menuWidth-2, menuHeight-2);
	    if (menuImage == 0) Error("Couldn't save menu in MapPopup");
	}
	QueryMouse(RootWindow, &x, &y, &w);
	if (w != menu) UnmapPopup();
}

UnmapPopup()
{
	register int x, y, i;

	if (freeze && saveScreen != 0) {
	    XUnmapTransparent(menu);
	    XPixmapPut(RootWindow, 0, 0, savex, savey, menuWidth, menuHeight,
		    saveScreen, GXcopy, AllPlanes);
	    XFreePixmap(saveScreen);
	    XUngrabServer();
	    saveScreen = 0;
	} else XUnmapWindow(menu);

	/* Now set things up so we don't have to map everything next time */

	if (!popupSaved) {
	    if (menuImage != 0) {
		XDestroySubwindows(menu);	/* Get rid of old items */
		x = y = 0;
		for (i = 0; i < itemcount; i++) {
		    item[i] = XCreateTransparency(menu, x, y,
			    itemWidth, itemHeight);
		    if (item[i] == NULL) {
			Error("Couldn't open item in UnmapPopup");
		    }
		    y += itemHeight;
		}
		XMapSubwindows(menu);
		popupSaved = TRUE;
	    }
	}
}

/* Undo the popup menu caching.  We can't just test popupSaved, since
   it gets set in UnmapPopup and we might be called in between */

UnsavePopup()
{
	if (menuImage != 0) {
	    XFreePixmap(menuImage);
	    menuImage = 0;
	}

	if (popupSaved) popupSaved = FALSE;
}

InvertButton(i)
	int i;
{
	if (item[i] == 0) return;

	XPixFill(item[i], -1, -1, 200, 200, 0, (Bitmap) 0,
		GXinvert, 1);
}

DisplayButton(i)
	int i;
{
	register int j;

	if (item[i] == 0) return;

	XPixSet(item[i], -1, -1, 200, 200, bgColor);
	XText(item[i], 3, 3, label[i], strlen(label[i]), menufont,
		fgColor, bgColor);
	for (j = 0; j < 3; j++) {
	    if (buttonFunc[j] == i) {
		XPixmapPut (item[buttonFunc[j]], 0, 0,
			itemWidth - button_width - 3, 4,
			button_width, button_height, buttonPixmap[j],
			buttonGXfunc, AllPlanes);
	    }
	}
}

IconifyOrDeIconify(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	if (w == RootWindow) return;

	/* If we're trying to iconify an icon deiconify instead */

	if (winfo->type == IsIcon) Deiconify(which, loc, w, winfo);
	else Iconify(which, loc, w, winfo);
}

/* ARGSUSED */

Assign(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	register int i;
	int choice, cleared = FALSE;
	BEvent newbutton;
	Window neww;

	/* First make sure the new click was in the menu; if not
	   clear that button */

	if (w != menu) return;

	/* Now get rid of the old function assigned this button. But, don't
	   deassign the popup window button! */

	choice = buttonFunc[which];
	if (choice == popup_button) return;

	if (popup) UnsavePopup();	/* The saved popup menu is invalid */

	if (choice >= 0) {
	    XClear(item[choice]);
	    buttonFunc[which] = unassigned_button;
	    DisplayButton(choice);
	    cleared = TRUE;
	}

	/* Now find out which subwindow we were in */

	InterpretLocatorW(menu, &w, loc);
	for (i = 0; item[i] != NULL && item[i] != w; i++) {}

	/* Can't assign Assign */

	if (menuOp[i] == NULL || menuOp[i] == Assign) {
	    if (cleared) XUngrabButton(mask | Button(which));
	    return;
	}

	/* Now wait for the release; if it doesn't occur abort */

	GetButton(&newbutton);
	if (!MatchUp(newbutton, which)) {
	    if (cleared) XUngrabButton(mask | Button(which));
	    return;
	}

	InterpretLocatorW(menu, &neww, loc);

	if (neww != w) {
	    if (cleared) XUngrabButton(mask | Button(which));
	    return;
	}

	/* Now assign that function to the button */

	buttonFunc[which] = i;

	XPixmapPut(item[i], 0, 0,
		itemWidth - button_width - 3, 4,
		button_width, button_height, buttonPixmap[which],
		buttonGXfunc, AllPlanes);

	if (!cleared) {			/* New assignment */
		status = XGrabButton(RootWindow, wmCursor,
			mask | Button(which),
			ButtonPressed|ButtonReleased);
		if (status == 0) Error ("Couldn't grab button in Assign");
	}
}

/* Returns whether or not this is a real event */

int GetEvent(event)
	XEvent *event;
{
	XButtonEvent *be = (XButtonEvent *) event;
	XLeaveWindowEvent *lwe = (XLeaveWindowEvent *) event;
	XExposeEvent *ee = (XExposeEvent *) event;
	XUnmapEvent *ue = (XUnmapEvent *) event;
	XFocusChangeEvent *fce = (XFocusChangeEvent *) event;
	WindowInfo winfo;
	char *iconName;

	XNextEvent(event);

	/* If it's in the base window, no trouble */

	if (event->window == RootWindow) {
	    if (event->type == FocusChange) {
		if (fce->detail == EnterWindow && fce->subwindow == 0) {
		    FocusOn(RootWindow);
		    FrameFocus();
		}
		return FALSE;

	    } else if (event->type == UnmapWindow) {
		FrameFocus();	/* The background has changed */
		return FALSE;
	    }

	    be->detail &= 0xff;	/* mask out state of shift keys */
	    return TRUE;
	}

	/* Is it the menu window? */

	if (event->window == menu) {
	    if (event->type == ExposeWindow) {
		if (ee->subwindow != 0) RefreshMenu(ee->subwindow);
		return FALSE;
	    }
	    if (event->type == LeaveWindow) {
		if (lwe->subwindow == 0 && !doingOp) UnmapPopup();
		return FALSE;
	    }
	}

	/* Let's see if it's in an icon */

	QueryWindow(event->window, &winfo);

	if (winfo.type != IsIcon) return FALSE;	/* Nope -- nothing */

	if (event->type == UnmapWindow) return FALSE;

	status = XFetchName(winfo.assoc_wind, &iconName);
	if (status == 0) Error ("Couldn't fetch name in GetButton");

	if (event->type == ExposeWindow) {	/* Just refresh the icon */
	    XClear(ee->window);
	    if (iconName != NULL && iconName[0] != '\0') {
		XText(ee->window, 4, 4, iconName, strlen(iconName), iconfont,
			fgColor, bgColor);
	    }
	} else EditIconName((XKeyPressedEvent *)be, iconName);
	if (iconName) free(iconName);
	return FALSE;
}

GetButton(event)
	BEvent *event;
{
	while (!GetEvent(event)) ;
}

RefreshMenu(w)
	Window w;
{
	register int i;
	
	for (i = 0; i < itemcount && item[i] != w; i++) {}
	if (i >= itemcount) return;
	DisplayButton(i);
}

Error(string)
	char *string;
{
	fprintf(stderr, "\nxmwm: %s", string);
	fprintf(stderr, "\n\n");

	if (errno != 0) {
		perror("xwm");
		fprintf(stderr, "\n");
	}

	exit(1);
}

Flush()
{
	XFlush();
}

Sync()
{
	XSync(FALSE);
}
