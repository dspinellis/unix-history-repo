/*
 * @(#)sun.c	1.1	%G%
 *
 * SUN specific routines for the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <suntool/tool_hs.h>
#include <suntool/menu.h>
#include <sys/file.h>
#include "gremlin.h"


/* imports from main.c */

extern tool_fd, pix_fd, menu_fd, text_fd;
extern struct pixfont *text_pf;

/* locals */

struct prompt prt = { {PROMPT_FLEXIBLE, PROMPT_FLEXIBLE,
		       PROMPT_FLEXIBLE, PROMPT_FLEXIBLE},
		       0, (struct pixfont *) NULL, (char *) NULL };

/*
 * Flush all input events for a window.
 */
flush_window_input(windowfd)
int windowfd;
{
    int nfds, readfds, writefds, exceptfds;
    struct timeval timeout;
    struct inputevent ie;

    do {
	readfds = 1 << windowfd;
	writefds = 0;
	exceptfds = 0;
	timeout.tv_sec = 0L;
	timeout.tv_usec = 0L;

	nfds = select(20, &readfds, &writefds, &exceptfds, &timeout);
	if (nfds > 0)
	    input_readevent(windowfd, &ie);
    } while (nfds > 0);
}


/*
 * Display user prompt and wait for affirmative (MS_LEFT)
 * or negative (MS_MIDDLE or MS_RIGHT) input event.
 * Return TRUE if OK to do it, FALSE if not OK.
 * This routine flushes the input event queue for the windowfd
 * handling the prompt.
 */
prompt_ok(windowfd, msg)
int windowfd;
char *msg;
{
    struct inputmask im, button_im;
    struct inputevent ie;
    int designee;

    win_getinputmask(windowfd, &im, &designee);

    input_imnull(&button_im);
    win_setinputcodebit(&button_im, MS_LEFT);
    win_setinputcodebit(&button_im, MS_MIDDLE);
    win_setinputcodebit(&button_im, MS_RIGHT);
    win_setinputmask(windowfd, &button_im, NULL, WIN_NULLLINK);

    prt.prt_font = text_pf;
    prt.prt_windowfd = windowfd;
    prt.prt_text = msg;
    
    flush_window_input(windowfd);

    menu_prompt(&prt, &ie, windowfd);

    win_setinputmask(windowfd, &im, NULL, designee);

    return(ie.ie_code == MS_LEFT);
}

/*
 * draw box with upper left corner at (x0, y0)
 * lower right corner at (x1, y1);
 */
pw_box(pw, x0, y0, x1, y1, op, value)
struct pixwin *pw;
register x0, y0, x1, y1;
int op, value;
{
    pw_vector(pw, x0, y0, x1, y0, op, value);
    pw_vector(pw, x1, y0, x1, y1, op, value);
    pw_vector(pw, x1, y1, x0, y1, op, value);
    pw_vector(pw, x0, y1, x0, y0, op, value);
}


/*
 * wait for input from any button.
 */
get_any_button()
{
    int nfds, readfds, writefds, exceptfds;
    struct inputevent ie;

    for (;;) {
	readfds = (1<<tool_fd) | (1<<pix_fd) | (1<<menu_fd) | (1<<text_fd);
	writefds = 0;
	exceptfds = 0;

	nfds = select(20, &readfds, &writefds,
				    &exceptfds, (struct timeval *) NULL);
	if (nfds > 0) {
	    if (readfds & (1 << menu_fd))
		input_readevent(menu_fd, &ie);
	    else if (readfds & (1 << pix_fd))
		input_readevent(pix_fd, &ie);
	    else if (readfds & (1 << text_fd))
		input_readevent(text_fd, &ie);
	    else /* tool window input event */
		input_readevent(tool_fd, &ie);

	    if ((ie.ie_code == MS_LEFT) || (ie.ie_code == MS_MIDDLE) ||
					   (ie.ie_code == MS_RIGHT))
                break;
	}
    }    
}
