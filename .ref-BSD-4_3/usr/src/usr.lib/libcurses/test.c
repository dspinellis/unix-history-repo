#include <curses.h>

#define YPOSBOX		 0
#define XPOSBOX		 0
#define YBOX		20
#define XBOX	        80

#define YPOSSBOX	 2
#define XPOSSBOX	10
#define YSBOX	        17
#define XSBOX	        66

WINDOW *boxing,*sub_box;
main()
{
	boxing = newwin(YBOX,XBOX,YPOSBOX,XPOSBOX);
	sub_box = subwin(boxing,YSBOX,XSBOX,YPOSSBOX,XPOSSBOX);
	initscr();
	box(boxing,'|','-');
	wrefresh(boxing);
	box(sub_box,'.','.');
	overlay(sub_box,boxing);/* overlays sub_box on top of boxing */
	wrefresh(sub_box);
	mvcur(0,COLS-1,LINES-1,0);	/* move to bottom of screen */
	endwin();
}
