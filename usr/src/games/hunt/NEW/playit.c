/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 *
 *  Copyright (c) 1985 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 */

# include	<curses.h>
# include	<ctype.h>
# include	<signal.h>
# include	<errno.h>
# include	"hunt.h"
# include	<sys/file.h>
# ifdef TERMINFO
# include	<term.h>

# define	AM		auto_right_margin
# define	XN		eat_newline_glitch
# define	CL		clear_screen
# define	CE		clr_eol
# define	_putchar	_outchar
# endif

int		input();
static int	nchar_send;
static int	in	= FREAD;
char		screen[SCREEN_HEIGHT][SCREEN_WIDTH2], blanks[SCREEN_WIDTH];
int		cur_row, cur_col;
# ifdef OTTO
int		Otto_count;
int		Otto_mode;
static int	otto_y, otto_x;
static char	otto_face;
# endif OTTO

# define	MAX_SEND	5

/*
 * ibuf is the input buffer used for the stream from the driver.
 * It is small because we do not check for user input when there
 * are characters in the input buffer.
 */
static char	ibuf[20];

#define	GETCHR(fd)	(--(fd)->_cnt < 0 ? getchr(fd) \
				: *((unsigned char *) (fd)->_ptr++))

/*
 * playit:
 *	Play a given game, handling all the curses commands from
 *	the driver.
 */
playit()
{
	register FILE		*inf;
	register int		ch;
	register int		y, x;
	extern int		errno;
	extern int		_putchar();
	long			version;

	if (read(Socket, (char *) &version, LONGLEN) != LONGLEN) {
		bad_con();
		/* NOTREACHED */
	}
	if (ntohl(version) != HUNT_VERSION) {
		bad_ver();
		/* NOTREACHED */
	}
	errno = 0;
	while ((inf = fdopen(Socket, "r")) == NULL)
		if (errno == EINTR)
			errno = 0;
		else {
			perror("fdopen of socket");
			exit(1);
		}
# ifdef L_ctermid
	setvbuf(inf, ibuf, _IOFBF, sizeof ibuf);
# else
	setbuffer(inf, ibuf, sizeof ibuf);
# endif
# ifdef OTTO
	Otto_count = 0;
# endif OTTO
	nchar_send = MAX_SEND;
	while ((ch = GETCHR(inf)) != EOF) {
# ifdef DEBUG
		fputc(ch, stderr);
# endif DEBUG
		switch (ch & 0377) {
		  case MOVE:
			y = GETCHR(inf);
			x = GETCHR(inf);
			mvcur(cur_row, cur_col, y, x);
			cur_row = y;
			cur_col = x;
			break;
		  case ADDCH:
			ch = GETCHR(inf);
# ifdef OTTO
			switch (ch) {

			case '<':
			case '>':
			case '^':
			case 'v':
				otto_face = ch;
				otto_y = cur_row;
				otto_x = cur_col;
				break;
			}
# endif OTTO
			put_ch(ch);
			break;
		  case CLRTOEOL:
			clear_eol();
			break;
		  case CLEAR:
			clear_the_screen();
			break;
		  case REFRESH:
			fflush(stdout);
			break;
		  case REDRAW:
			redraw_screen();
			fflush(stdout);
			break;
		  case ENDWIN:
			fflush(stdout);
			if ((ch = GETCHR(inf)) == LAST_PLAYER)
				Last_player = TRUE;
			ch = EOF;
			goto out;
		  case BELL:
			putchar(CTRL(G));
			break;
		  case READY:
			(void) fflush(stdout);
			if (nchar_send < 0)
# ifndef TCFLSH
				(void) ioctl(fileno(stdin), TIOCFLUSH, &in);
# else
				(void) ioctl(fileno(stdin), TCFLSH, 0);
# endif
			nchar_send = MAX_SEND;
# ifndef OTTO
			(void) GETCHR(inf);
# else OTTO
			Otto_count -= (GETCHR(inf) & 255);
			if (!Am_monitor) {
# ifdef DEBUG
				fputc('0' + Otto_count, stderr);
# endif DEBUG
				if (Otto_count == 0 && Otto_mode)
					otto(otto_y, otto_x, otto_face);
			}
# endif OTTO
			break;
		  default:
# ifdef OTTO
			switch (ch) {

			case '<':
			case '>':
			case '^':
			case 'v':
				otto_face = ch;
				otto_y = cur_row;
				otto_x = cur_col;
				break;
			}
# endif OTTO
			put_ch(ch);
			break;
		}
	}
out:
	(void) fclose(inf);
}

/*
 * getchr:
 *	Grab input and pass it along to the driver
 *	Return any characters from the driver
 *	When this routine is called by GETCHR, we already know there are
 *	no characters in the input buffer.
 */
getchr(fd)
register FILE	*fd;
{
	long	readfds, s_readfds;
	int	driver_mask, stdin_mask;
	int	nfds, s_nfds;

	driver_mask = 1L << fileno(fd);
	stdin_mask = 1L << fileno(stdin);
	s_readfds = driver_mask | stdin_mask;
	s_nfds = (fileno(fd) > fileno(stdin)) ? fileno(fd) : fileno(stdin);
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
	return _filbuf(fd);
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

	count = read(fileno(stdin), Buf, sizeof Buf);
	if (count <= 0)
		return;
	if (nchar_send <= 0 && !no_beep) {
		(void) write(1, "\7", 1);	/* CTRL(G) */
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
# endif OTTO
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
# endif OTTO
	mvcur(cur_row, cur_col, HEIGHT, 0);
	cur_row = HEIGHT;
	cur_col = 0;
	put_str("Re-enter game [ynwo]? ");
	clear_eol();
	fflush(stdout);
	explain = FALSE;
	for (;;) {
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
			mvcur(cur_row, cur_col, HEIGHT, 0);
			cur_row = HEIGHT;
			cur_col = 0;
			put_str("Write a parting message [yn]? ");
			clear_eol();
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
			mvcur(cur_row, cur_col, HEIGHT, 0);
			cur_row = HEIGHT;
			cur_col = 0;
			put_str("Message: ");
			clear_eol();
			cp = buf;
			while ((ch = getchar()) != '\n' && ch != '\r') {
# ifdef TERMINFO
				if (ch == erasechar())
# else
				if (ch == _tty.sg_erase)
# endif
				{
					if (cp > buf) {
						mvcur(cur_row, cur_col, cur_row,
								cur_col - 1);
						cur_col -= 1;
						cp -= 1;
						clear_eol();
					}
					continue;
# ifdef TERMINFO
				} else if (ch == killchar()) {
# else
				} else if (ch == _tty.sg_kill) {
# endif
					mvcur(cur_row, cur_col, cur_row,
							cur_col - (cp - buf));
					cur_col -= cp - buf;
					cp = buf;
					clear_eol();
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
# endif INTERNET
		(void) putchar(CTRL(G));
		if (!explain) {
			put_str("(Yes, No, Write message, or Options) ");
			explain = TRUE;
		}
		fflush(stdout);
	}

	mvcur(cur_row, cur_col, HEIGHT, 0);
	cur_row = HEIGHT;
	cur_col = 0;
# ifdef FLY
	put_str("Scan, Cloak, Flying, or Quit? ");
# else
	put_str("Scan, Cloak, or Quit? ");
# endif FLY
	clear_eol();
	fflush(stdout);
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
# endif FLY
		else if (ch == 'q')
			return Q_QUIT;
		(void) putchar(CTRL(G));
		if (!explain) {
# ifdef FLY
			put_str("[SCFQ] ");
# else
			put_str("[SCQ] ");
# endif FLY
			explain = TRUE;
		}
		fflush(stdout);
	}
}

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

clear_the_screen()
{
	register int	i;

	if (blanks[0] == '\0')
		for (i = 0; i < SCREEN_WIDTH; i++)
			blanks[i] = ' ';

	if (CL != NULL) {
		tputs(CL, LINES, _putchar);
		for (i = 0; i < SCREEN_HEIGHT; i++)
			bcopy(blanks, screen[i], SCREEN_WIDTH);
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
#ifdef TERMINFO
	move(0, 0);
	refresh();
#endif
}

clear_eol()
{
	if (CE != NULL)
		tputs(CE, 1, _putchar);
	else {
		fwrite(blanks, sizeof (char), SCREEN_WIDTH - cur_col, stdout);
		if (COLS != SCREEN_WIDTH)
			mvcur(cur_row, SCREEN_WIDTH, cur_row, cur_col);
		else if (AM)
			mvcur(cur_row + 1, 0, cur_row, cur_col);
		else
			mvcur(cur_row, SCREEN_WIDTH - 1, cur_row, cur_col);
	}
	bcopy(blanks, &screen[cur_row][cur_col], SCREEN_WIDTH - cur_col);
}

redraw_screen()
{
	register int	i;
# ifndef NOCURSES
	static int	first = 1;

	if (first) {
		curscr = newwin(SCREEN_HEIGHT, SCREEN_WIDTH, 0, 0);
		if (curscr == NULL) {
			fprintf(stderr, "Can't create curscr\n");
			exit(1);
		}
		for (i = 0; i < SCREEN_HEIGHT; i++)
			curscr->_y[i] = screen[i];
		first = 0;
	}
	curscr->_cury = cur_row;
	curscr->_curx = cur_col;
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
