/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 */

# if defined(HPUX) || (defined(BSD_RELEASE) && BSD_RELEASE >= 44)
# include	<termios.h>
# endif
# include	<curses.h>
# include	<ctype.h>
# include	<signal.h>
# include	<errno.h>
# include	"hunt.h"
# include	<sys/file.h>

# ifndef FREAD
# define	FREAD	1
# endif

# if !defined(USE_CURSES) || !defined(TERMINFO)
# define	beep()		(void) putchar(CTRL('G'))
# endif
# if !defined(USE_CURSES)
# undef		refresh
# define	refresh()	(void) fflush(stdout);
# endif
# ifdef USE_CURSES
# define	clear_eol()	clrtoeol()
# define	put_ch		addch
# define	put_str		addstr
# endif

int		input();
static int	nchar_send;
static int	in	= FREAD;
# ifndef USE_CURSES
char		screen[SCREEN_HEIGHT][SCREEN_WIDTH2], blanks[SCREEN_WIDTH];
int		cur_row, cur_col;
# endif
# ifdef OTTO
int		Otto_count;
int		Otto_mode;
static int	otto_y, otto_x;
static char	otto_face;
# endif

# define	MAX_SEND	5
# define	STDIN		0

/*
 * ibuf is the input buffer used for the stream from the driver.
 * It is small because we do not check for user input when there
 * are characters in the input buffer.
 */
static int		icnt = 0;
static unsigned char	ibuf[256], *iptr = ibuf;
static unsigned char	getchr();

#define	GETCHR()	(--icnt < 0 ? getchr() : *iptr++)

#if !defined(BSD_RELEASE) || BSD_RELEASE < 44
extern int	_putchar();
#endif

/*
 * playit:
 *	Play a given game, handling all the curses commands from
 *	the driver.
 */
playit()
{
	register int	ch;
	register int	y, x;
	extern int	errno;
	long		version;

	if (read(Socket, (char *) &version, LONGLEN) != LONGLEN) {
		bad_con();
		/* NOTREACHED */
	}
	if (ntohl(version) != HUNT_VERSION) {
		bad_ver();
		/* NOTREACHED */
	}
	errno = 0;
# ifdef OTTO
	Otto_count = 0;
# endif
	nchar_send = MAX_SEND;
	while ((ch = GETCHR()) != EOF) {
# ifdef DEBUG
		fputc(ch, stderr);
# endif
		switch (ch & 0377) {
		  case MOVE:
			y = GETCHR();
			x = GETCHR();
# ifdef USE_CURSES
			move(y, x);
# else
			mvcur(cur_row, cur_col, y, x);
			cur_row = y;
			cur_col = x;
# endif
			break;
		  case ADDCH:
			ch = GETCHR();
# ifdef OTTO
			switch (ch) {

			case '<':
			case '>':
			case '^':
			case 'v':
				otto_face = ch;
# ifdef USE_CURSES
				getyx(stdscr, otto_y, otto_x);
# else
				otto_y = cur_row;
				otto_x = cur_col;
# endif
				break;
			}
# endif
			put_ch(ch);
			break;
		  case CLRTOEOL:
			clear_eol();
			break;
		  case CLEAR:
			clear_the_screen();
			break;
		  case REFRESH:
			refresh();
			break;
		  case REDRAW:
			redraw_screen();
			refresh();
			break;
		  case ENDWIN:
			refresh();
			if ((ch = GETCHR()) == LAST_PLAYER)
				Last_player = TRUE;
			ch = EOF;
			goto out;
		  case BELL:
			beep();
			break;
		  case READY:
			refresh();
			if (nchar_send < 0)
# if defined(HPUX) || (defined(BSD_RELEASE) && BSD_RELEASE >= 44)
				tcflush(STDIN, TCIFLUSH);
# else
# ifndef TCFLSH
				(void) ioctl(STDIN, TIOCFLUSH, &in);
# else
				(void) ioctl(STDIN, TCFLSH, 0);
# endif
# endif
			nchar_send = MAX_SEND;
# ifndef OTTO
			(void) GETCHR();
# else
			Otto_count -= (GETCHR() & 0xff);
			if (!Am_monitor) {
# ifdef DEBUG
				fputc('0' + Otto_count, stderr);
# endif
				if (Otto_count == 0 && Otto_mode)
					otto(otto_y, otto_x, otto_face);
			}
# endif
			break;
		  default:
# ifdef OTTO
			switch (ch) {

			case '<':
			case '>':
			case '^':
			case 'v':
				otto_face = ch;
# ifdef USE_CURSES
				getyx(stdscr, otto_y, otto_x);
# else
				otto_y = cur_row;
				otto_x = cur_col;
# endif
				break;
			}
# endif
			put_ch(ch);
			break;
		}
	}
out:
	(void) close(Socket);
}

/*
 * getchr:
 *	Grab input and pass it along to the driver
 *	Return any characters from the driver
 *	When this routine is called by GETCHR, we already know there are
 *	no characters in the input buffer.
 */
static
unsigned char
getchr()
{
	long	readfds, s_readfds;
	int	driver_mask, stdin_mask;
	int	nfds, s_nfds;

	driver_mask = 1L << Socket;
	stdin_mask = 1L << STDIN;
	s_readfds = driver_mask | stdin_mask;
	s_nfds = (Socket > STDIN) ? Socket : STDIN;
	s_nfds++;

one_more_time:
	do {
		errno = 0;
		readfds = s_readfds;
		nfds = s_nfds;
		nfds = select(nfds, &readfds, NULL, NULL, NULL);
	} while (nfds <= 0 && errno == EINTR);

	if (readfds & stdin_mask)
		send_stuff();
	if ((readfds & driver_mask) == 0)
		goto one_more_time;
	icnt = read(Socket, ibuf, sizeof ibuf);
	if (icnt < 0) {
		bad_con();
		/* NOTREACHED */
	}
	if (icnt == 0)
		goto one_more_time;
	iptr = ibuf;
	icnt--;
	return *iptr++;
}

/*
 * send_stuff:
 *	Send standard input characters to the driver
 */
send_stuff()
{
	register int	count;
	register char	*sp, *nsp;
	static char	inp[sizeof Buf];

	count = read(STDIN, Buf, sizeof Buf);
	if (count <= 0)
		return;
	if (nchar_send <= 0 && !no_beep) {
		(void) write(1, "\7", 1);	/* CTRL('G') */
		return;
	}

	/*
	 * look for 'q'uit commands; if we find one,
	 * confirm it.  If it is not confirmed, strip
	 * it out of the input
	 */
	Buf[count] = '\0';
	nsp = inp;
	for (sp = Buf; *sp != '\0'; sp++)
		if ((*nsp = map_key[*sp]) == 'q')
			intr();
		else
			nsp++;
	count = nsp - inp;
	if (count) {
# ifdef OTTO
		Otto_count += count;
# endif
		nchar_send -= count;
		if (nchar_send < 0)
			count += nchar_send;
		(void) write(Socket, inp, count);
	}
}

/*
 * quit:
 *	Handle the end of the game when the player dies
 */
long
quit(old_status)
long	old_status;
{
	register int	explain, ch;

	if (Last_player)
		return Q_QUIT;
# ifdef OTTO
	if (Otto_mode)
		return Q_CLOAK;
# endif
# ifdef USE_CURSES
	move(HEIGHT, 0);
# else
	mvcur(cur_row, cur_col, HEIGHT, 0);
	cur_row = HEIGHT;
	cur_col = 0;
# endif
	put_str("Re-enter game [ynwo]? ");
	clear_eol();
	explain = FALSE;
	for (;;) {
		refresh();
		if (isupper(ch = getchar()))
			ch = tolower(ch);
		if (ch == 'y')
			return old_status;
		else if (ch == 'o')
			break;
		else if (ch == 'n') {
# ifndef INTERNET
			return Q_QUIT;
# else
# ifdef USE_CURSES
			move(HEIGHT, 0);
# else
			mvcur(cur_row, cur_col, HEIGHT, 0);
			cur_row = HEIGHT;
			cur_col = 0;
# endif
			put_str("Write a parting message [yn]? ");
			clear_eol();
			refresh();
			for (;;) {
				if (isupper(ch = getchar()))
					ch = tolower(ch);
				if (ch == 'y')
					goto get_message;
				if (ch == 'n')
					return Q_QUIT;
			}
# endif
		}
# ifdef INTERNET
		else if (ch == 'w') {
			static	char	buf[WIDTH + WIDTH % 2];
			char		*cp, c;

get_message:
			c = ch;		/* save how we got here */
# ifdef USE_CURSES
			move(HEIGHT, 0);
# else
			mvcur(cur_row, cur_col, HEIGHT, 0);
			cur_row = HEIGHT;
			cur_col = 0;
# endif
			put_str("Message: ");
			clear_eol();
			refresh();
			cp = buf;
			for (;;) {
				refresh();
				if ((ch = getchar()) == '\n' || ch == '\r')
					break;
# if defined(TERMINFO) || BSD_RELEASE >= 44
				if (ch == erasechar())
# else
				if (ch == _tty.sg_erase)
# endif
				{
					if (cp > buf) {
# ifdef USE_CURSES
						int y, x;
						getyx(stdscr, y, x);
						move(y, x - 1);
# else
						mvcur(cur_row, cur_col, cur_row,
								cur_col - 1);
						cur_col -= 1;
# endif
						cp -= 1;
						clear_eol();
					}
					continue;
				}
# if defined(TERMINFO) || BSD_RELEASE >= 44
				else if (ch == killchar())
# else
				else if (ch == _tty.sg_kill)
# endif
				{
# ifdef USE_CURSES
					int y, x;
					getyx(stdscr, y, x);
					move(y, x - (cp - buf));
# else
					mvcur(cur_row, cur_col, cur_row,
							cur_col - (cp - buf));
					cur_col -= cp - buf;
# endif
					cp = buf;
					clear_eol();
					continue;
				} else if (!isprint(ch)) {
					beep();
					continue;
				}
				put_ch(ch);
				*cp++ = ch;
				if (cp + 1 >= buf + sizeof buf)
					break;
			}
			*cp = '\0';
			Send_message = buf;
			return (c == 'w') ? old_status : Q_MESSAGE;
		}
# endif
		beep();
		if (!explain) {
			put_str("(Yes, No, Write message, or Options) ");
			explain = TRUE;
		}
	}

# ifdef USE_CURSES
	move(HEIGHT, 0);
# else
	mvcur(cur_row, cur_col, HEIGHT, 0);
	cur_row = HEIGHT;
	cur_col = 0;
# endif
# ifdef FLY
	put_str("Scan, Cloak, Flying, or Quit? ");
# else
	put_str("Scan, Cloak, or Quit? ");
# endif
	clear_eol();
	refresh();
	explain = FALSE;
	for (;;) {
		if (isupper(ch = getchar()))
			ch = tolower(ch);
		if (ch == 's')
			return Q_SCAN;
		else if (ch == 'c')
			return Q_CLOAK;
# ifdef FLY
		else if (ch == 'f')
			return Q_FLY;
# endif
		else if (ch == 'q')
			return Q_QUIT;
		beep();
		if (!explain) {
# ifdef FLY
			put_str("[SCFQ] ");
# else
			put_str("[SCQ] ");
# endif
			explain = TRUE;
		}
		refresh();
	}
}

# ifndef USE_CURSES
put_ch(ch)
	char	ch;
{
	if (!isprint(ch)) {
		fprintf(stderr, "r,c,ch: %d,%d,%d", cur_row, cur_col, ch);
		return;
	}
	screen[cur_row][cur_col] = ch;
	putchar(ch);
	if (++cur_col >= COLS) {
		if (!AM || XN)
			putchar('\n');
		cur_col = 0;
		if (++cur_row >= LINES)
			cur_row = LINES;
	}
}

put_str(s)
	char	*s;
{
	while (*s)
		put_ch(*s++);
}
# endif

clear_the_screen()
{
# ifdef USE_CURSES
	clear();
	move(0, 0);
	refresh();
# else
	register int	i;

	if (blanks[0] == '\0')
		for (i = 0; i < SCREEN_WIDTH; i++)
			blanks[i] = ' ';

	if (CL != NULL) {
#if !defined(BSD_RELEASE) || BSD_RELEASE < 44
		tputs(CL, LINES, _putchar);
#else
		tputs(CL, LINES, __cputchar);
#endif
		for (i = 0; i < SCREEN_HEIGHT; i++)
			memcpy(screen[i], blanks, SCREEN_WIDTH);
	} else {
		for (i = 0; i < SCREEN_HEIGHT; i++) {
			mvcur(cur_row, cur_col, i, 0);
			cur_row = i;
			cur_col = 0;
			clear_eol();
		}
		mvcur(cur_row, cur_col, 0, 0);
	}
	cur_row = cur_col = 0;
#endif
}

#ifndef USE_CURSES
clear_eol()
{
	if (CE != NULL)
#if !defined(BSD_RELEASE) || BSD_RELEASE < 44
		tputs(CE, 1, _putchar);
#else
		tputs(CE, 1, __cputchar);
#endif
	else {
		fwrite(blanks, sizeof (char), SCREEN_WIDTH - cur_col, stdout);
		if (COLS != SCREEN_WIDTH)
			mvcur(cur_row, SCREEN_WIDTH, cur_row, cur_col);
		else if (AM)
			mvcur(cur_row + 1, 0, cur_row, cur_col);
		else
			mvcur(cur_row, SCREEN_WIDTH - 1, cur_row, cur_col);
	}
	memcpy(&screen[cur_row][cur_col], blanks, SCREEN_WIDTH - cur_col);
}
# endif

redraw_screen()
{
# ifdef USE_CURSES
	clearok(stdscr, TRUE);
	touchwin(stdscr);
# else
	register int	i;
# ifndef NOCURSES
	static int	first = 1;

	if (first) {
		curscr = newwin(SCREEN_HEIGHT, SCREEN_WIDTH, 0, 0);
		if (curscr == NULL) {
			fprintf(stderr, "Can't create curscr\n");
			exit(1);
		}
# if !defined(BSD_RELEASE) || BSD_RELEASE < 44
		for (i = 0; i < SCREEN_HEIGHT; i++)
			curscr->_y[i] = screen[i];
# endif
		first = 0;
	}
# if defined(BSD_RELEASE) && BSD_RELEASE >= 44
	for (i = 0; i < SCREEN_HEIGHT; i++) {
		register int	j;
		for (j = 0; j < SCREEN_WIDTH; j++)
			curscr->lines[i]->line[j].ch = screen[i][j];
	}
	curscr->cury = cur_row;
	curscr->curx = cur_col;
# else
	curscr->_cury = cur_row;
	curscr->_curx = cur_col;
# endif
	clearok(curscr, TRUE);
	touchwin(curscr);
	wrefresh(curscr);
#else
	mvcur(cur_row, cur_col, 0, 0);
	for (i = 0; i < SCREEN_HEIGHT - 1; i++) {
		fwrite(screen[i], sizeof (char), SCREEN_WIDTH, stdout);
		if (COLS > SCREEN_WIDTH || (COLS == SCREEN_WIDTH && !AM))
			putchar('\n');
	}
	fwrite(screen[SCREEN_HEIGHT - 1], sizeof (char), SCREEN_WIDTH - 1,
		stdout);
	mvcur(SCREEN_HEIGHT - 1, SCREEN_WIDTH - 1, cur_row, cur_col);
#endif
#endif
}

/*
 * do_message:
 *	Send a message to the driver and return
 */
do_message()
{
	extern int	errno;
	long		version;

	if (read(Socket, (char *) &version, LONGLEN) != LONGLEN) {
		bad_con();
		/* NOTREACHED */
	}
	if (ntohl(version) != HUNT_VERSION) {
		bad_ver();
		/* NOTREACHED */
	}
# ifdef INTERNET
	if (write(Socket, Send_message, strlen(Send_message)) < 0) {
		bad_con();
		/* NOTREACHED */
	}
# endif
	(void) close(Socket);
}
