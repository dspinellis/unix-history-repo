/*
 *	$Source: /u1/X/xterm/RCS/menu.h,v $
 *	$Header: menu.h,v 10.100 86/12/01 14:40:05 jg Rel $
 */

/* @(#)menu.h       X10/6.6B 12/26/86 */
/*
 * Menu items are constructed as follows, starting from the left side:
 *
 *	menuItemPad
 *	space for check mark
 *	menuItemPad
 *	text + padding
 *	menuItemPad
 *
 * The padding for the text is that amount that this text is narrower than the
 * widest text.
 */

typedef struct _menuItem {
	int itemHeight;			/* total height of this item */
	int itemFlags;			/* flags of item */

#define	itemDisabled		0x0001	/* item is disabled */
#define	itemChecked		0x0002	/* item has check mark */
#define	itemStateMask		0x0003	/* mask for current state */
#define	itemSetDisabled		0x0004	/* item wants to be disabled */
#define	itemSetChecked		0x0008	/* item wants check mark */
#define	itemSetMask		0x000c	/* mask for desired state */
#define	itemSetMaskShift	2	/* for comparison with actual */
#define	itemChanged		0x0010	/* item desires change */

	char *itemText;			/* text of item */
	int itemTextWidth;		/* width of text */
	int itemTextLength;		/* length of text */
	struct _menuItem *nextItem;	/* next item in chain */
} MenuItem;

typedef struct _menu {
	int menuWidth;			/* full width of menu */
	int menuHeight;			/* full height of menu */
	int menuFlags;			/* flags of this menu */

# define	menuChanged	0x0001		/* menu changed, must redraw */
# define	menuItemChanged	0x0002		/* item changed, must redraw */
# define	menuMapped	0x0004		/* menu is now mapped */
# define	menuFreeze	0x0008		/* freeze when doing menu */
# define	menuSaveMenu	0x0010		/* save copy of menu */

	int menuMaxTextWidth;		/* width of widest text */
	int menuInitialItem;		/* < 0 none, >= 0 initial item */
	int menuBorderWidth;		/* width of border */
	int menuBgColor;		/* background color */
	int menuFgColor;		/* foreground color */
	Pixmap menuBgTile;		/* backgroud tile */
	FontInfo *menuFontInfo;		/* fontinfo for menu font */
	int menuItemPad;		/* pad amount */
	Window menuWindow;		/* window of menu */
	int (*menuEventHandler)();	/* external event handler */
	Cursor menuCursor;		/* cursor used in menu */
	Pixmap menuSaved;		/* copy of menu */
	Pixmap menuSavedImage;		/* copy of image under menu */
	int menuSavedImageX;		/* X coordinate of the saved image */
	int menuSavedImageY;		/* Y coordinate of the saved image */
	MenuItem *menuItems;		/* head of menu item chain */
	char *menuTitle;		/* title of menu */
	int menuTitleWidth;		/* width of title */
	int menuTitleLength;		/* length of title */
	int menuItemTop;		/* position of top of first item */
} Menu;

#define	checkMarkWidth		9
#define	checkMarkHeight		8
#define	defaultCursorWidth	16
#define	defaultCursorHeight	16
#define	defaultCursorX		1
#define	defaultCursorY		1
#define	grayHeight		16
#define	grayWidth		16
#define	lineSeparatorHeight	9

#define	CheckItem(menu,item)	SetItemCheck(menu,item,1)
#define	DisableItem(menu,item)	SetItemDisable(menu,item,1)
#define	EnableItem(menu,item)	SetItemDisable(menu,item,0)
#define	SetMenuEventHandler(menu,f)	menu->menuEventHandler = f
#define	UncheckItem(menu,item)	SetItemCheck(menu,item,0)

extern Menu *NewMenu();
