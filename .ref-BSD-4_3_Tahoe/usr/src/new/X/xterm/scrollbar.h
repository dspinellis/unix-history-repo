/*
 *	$Source: /u1/X/xterm/RCS/scrollbar.h,v $
 *	$Header: scrollbar.h,v 10.100 86/12/01 14:40:30 jg Rel $
 */

/* @(#)scrollbar.h       X10/6.6B 12/26/86 */
#define	BARSTART		(2 * BUTTONHEIGHT)
#define	BUTTONHEIGHT		SCROLLBARWIDTH
#define	HILITED			1
#define	MINSCROLLBARHEIGHT	(BARSTART + 32)
#define	NBUTTONBITMAPS		(BUTTON_NORMAL / 2 + 1)
#define	NSAVESTATES		2
#define	PAUSETIME		(1000000L / 5)
#define	SAVELINES		64
#define	SCROLLBARWIDTH		20
#define	STEPTIME		(1000000L / 10)

/*
 * the first four must be consecutive and at the bottom
 */
#define	BUTTON_UPLINE		0
#define	BUTTON_UPLINEHI		(BUTTON_UPLINE | HILITED)
#define	BUTTON_DOWNLINE		(BUTTON_UPLINE + 2)
#define	BUTTON_DOWNLINEHI	(BUTTON_DOWNLINE | HILITED)
#define	BUTTON_UPPAGE		(BUTTON_DOWNLINE + 2)
#define	BUTTON_UPPAGEHI		(BUTTON_UPPAGE | HILITED)
#define	BUTTON_DOWNPAGE		(BUTTON_UPPAGE + 2)
#define	BUTTON_DOWNPAGEHI	(BUTTON_DOWNPAGE | HILITED)
#define	BUTTON_TOP		(BUTTON_DOWNPAGE + 2)
#define	BUTTON_TOPHI		(BUTTON_TOP | HILITED)
#define	BUTTON_BOTTOM		(BUTTON_TOP + 2)
#define	BUTTON_BOTTOMHI		(BUTTON_BOTTOM | HILITED)
#define	BUTTON_NORMAL		(BUTTON_BOTTOM + 2)

#define	SAVE_OFF		0
#define	SAVE_ON			1

#define	HIDE			1
#define	NONE			0
#define	SHOW			2

#define	GetButtonState(sb)		(sb->buttonset)
#define	GetSaveState(sb)		(sb->saveset)
#define	GetScrollBarBottom(sb)		(sb->set.bottomvalue)
#define	GetScrollBarRegion(sb)		(sb->set.regionheight)
#define	GetScrollBarTop(sb)		(sb->set.topvalue)
#define	GetScrollBarValue(sb)		(sb->set.value)
#define	SetScrollBarBottom(sb,bot)	sb->set.bottomvalue = (bot)
#define	SetScrollBarRegion(sb,reg)	sb->set.regionheight = (reg)
#define	SetScrollBarTop(sb,top)		sb->set.topvalue = (top)
#define	SetScrollBarValue(sb,val)	sb->set.value = (val)

struct scroll_region {
	int value;			/* value at top of region */
	int regionheight;		/* region height below value */
	int topvalue;			/* of scroll area */
	int bottomvalue;		/* of scroll area */
	int height;			/* of scroll area */
	int y;				/* y position of region */
	int pixelheight;		/* height in pixel of region */
};

typedef struct scroll_bar {
	int visible;				/* scrollbar visible */
	int buttonstate;			/* current button state */
	int buttonset;				/* requested button state */
	int savestate;				/* current save state */
	int saveset;				/* requested save state */
	int regionvisible;			/* region visible */
	int action;				/* state is changing */
	Window bar;				/* main scrollbar window */
	Window button;				/* button window */
	Window save;				/* save state window */
	Window region;				/* region window */
	Cursor cursor;				/* scrollbar cursor */
	short *buttonbits[NBUTTONBITMAPS];	/* button state bitmaps */
	short *savebits[NSAVESTATES];		/* save state bitmaps */
	int fg;					/* foreground color */
	int bg;					/* background color */
	struct scroll_region state;		/* current region state */
	struct scroll_region set;		/* requested region state */
} ScrollBar;

ScrollBar *CreateScrollBar();
