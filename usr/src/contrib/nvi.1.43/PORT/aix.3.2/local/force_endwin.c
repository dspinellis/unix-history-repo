#include <sys/types.h>
#include <sys/queue.h>
#include <sys/time.h>

#include <bitstring.h>
#include <curses.h>
#include <signal.h>

#include "compat.h"
#include <db.h>
#include <regex.h>

#include "vi.h"

int
force_endwin()
{
	int rval;
	
#undef endwin
	rval = endwin();

	/*
	 * Some (many...) System V derived implementations of curses will
	 * not reset the terminal modes correctly if you do any tgetent(3)
	 * style calls.   We can often get around this by using the System
	 * V tiget(3) style calls, but in some implementations they're not
	 * available.  So, we have to reset the terminal modes explicitly
	 * when endin is called.
	 */
	(void)(tcsetattr(STDIN_FILENO,
	    TCSASOFT | TCSADRAIN, &__global_list->original_termios));
	return (rval);
}
