/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)playit.c	5.3 (Berkeley) 6/27/88";
#endif /* not lint */

/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 */

# include	<curses.h>
# include	<ctype.h>
# include	<signal.h>
# include	<errno.h>
# include	"hunt.h"
# include	<sys/file.h>

# undef  CTRL
# define CTRL(x)	('x' & 037)

int		input();
static int	nchar_send;
static int	in	= FREAD;
char		screen[24][80], blanks[80];
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

#define	GETCHR(fd)	(--(fd)->_cnt >= 0 ? *(fd)->_ptr++&0377 : getchr(fd))

extern int		_putchar();

/*
 * playit:
 *	Play a given game, handling all the curses commands from
 *	the driver.
 */
playit()
{
	register FILE		*inf;
	register int		ch;
	register unsigned int	y, x;
	extern int		Master_pid;
	extern int		errno;

	errno = 0;
	while ((inf = fdopen(Socket, "r")) == NULL)
		if (errno == EINTR)
			errno = 0;
		else {
			perror("fdopen of socket");
			exit(1);
		}
	setbuffer(inf, ibuf, sizeof ibuf);
	Master_pid = getw(inf);
	if (Master_pid == 0 || Master_pid == EOF) {
		bad_con();
		/* NOTREACHED */
	}
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
				getyx(stdscr, otto_y, otto_x);
				break;
			}
# endif OTTO
			put_ch(ch);
			break;
		  case CLRTOEOL:
			clear_eol();
			break;
		  case CLEAR:
			clear_screen();
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
				(void) ioctl(fileno(stdin), TIOCFLUSH, &in);
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
				getyx(stdscr, otto_y, otto_x);
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
	long	nchar;
	long	readfds, s_readfds;
	int	driver_mask, stdin_mask;
	int	nfds, s_nfds;
	extern int	errno;

	driver_mask = 1L << fileno(fd);
	stdin_mask = 1L << fileno(stdin);
	s_readfds = driver_mask | stdin_mask;
	s_nfds = (driver_mask > stdin_mask) ? driver_mask : stdin_mask;
	s_nfds++;

one_more_time:
	do {
		errno = 0;
		readfds = s_readfds;
		nfds = s_nfds;
# ifndef OLDIPC
		nfds = select(nfds, &readfds, NULL, NULL, NULL);
# else OLDIPC
		nfds = select(nfds, &readfds, (int *) NULL, 32767);
# endif OLDIPC
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
	extern char	map_key[256];

	count = read(fileno(stdin), Buf, sizeof Buf);
	if (count <= 0)
		return;
	if (nchar_send <= 0) {
		(void) write(1, "\7", 1);
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
# ifdef OTTO
		else if (*nsp == CTRL(O))
			Otto_mode = !Otto_mode;
# endif OTTO
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
quit()
{
	register int	explain, ch;

	if (Last_player)
		return TRUE;
# ifdef OTTO
	if (Otto_mode)
		return FALSE;
# endif OTTO
	mvcur(cur_row, cur_col, HEIGHT, 0);
	cur_row = HEIGHT;
	cur_col = 0;
	put_str("Re-enter game? ");
	clear_eol();
	fflush(stdout);
	explain = FALSE;
	for (;;) {
		if (isupper(ch = getchar()))
			ch = tolower(ch);
		if (ch == 'y') {
			sleep(2);
			return FALSE;
		}
		else if (ch == 'n')
			return TRUE;
		(void) putchar(CTRL(G));
		if (!explain) {
			put_str("(Y or N) ");
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

clear_screen()
{
	register int	i;

	if (blanks[0] == '\0')
		for (i = 0; i < 80; i++)
			blanks[i] = ' ';

	if (CL != NULL) {
		tputs(CL, LINES, _putchar);
		for (i = 0; i < 24; i++)
			bcopy(blanks, screen[i], 80);
	} else {
		for (i = 0; i < 24; i++) {
			mvcur(cur_row, cur_col, i, 0);
			cur_row = i;
			cur_col = 0;
			clear_eol();
		}
		mvcur(cur_row, cur_col, 0, 0);
	}
	cur_row = cur_col = 0;
}

clear_eol()
{
	if (CE != NULL)
		tputs(CE, 1, _putchar);
	else {
		fwrite(blanks, sizeof (char), 80 - cur_col, stdout);
		if (COLS != 80)
			mvcur(cur_row, 80, cur_row, cur_col);
		else if (AM)
			mvcur(cur_row + 1, 0, cur_row, cur_col);
		else
			mvcur(cur_row, 79, cur_row, cur_col);
	}
	bcopy(blanks, &screen[cur_row][cur_col], 80 - cur_col);
}

redraw_screen()
{
	register int	i;
	static int	first = 1;

	if (first) {
		if ((curscr = newwin(24, 80, 0, 0)) == NULL) {
			fprintf(stderr, "Can't create curscr\n");
			exit(1);
		}
		for (i = 0; i < 24; i++)
			curscr->_y[i] = screen[i];
		first = 0;
	}
	curscr->_cury = cur_row;
	curscr->_curx = cur_col;
	wrefresh(curscr);
#ifdef	NOCURSES
	mvcur(cur_row, cur_col, 0, 0);
	for (i = 0; i < 23; i++) {
		fwrite(screen[i], sizeof (char), 80, stdout);
		if (COLS > 80 || (COLS == 80 && !AM))
			putchar('\n');
	}
	fwrite(screen[23], sizeof (char), 79, stdout);
	mvcur(23, 79, cur_row, cur_col);
#endif
}
