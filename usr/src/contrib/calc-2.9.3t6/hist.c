/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Adapted from code written by Stephen Rothwell.
 *
 * Interactive readline module.  This is called to read lines of input,
 * while using emacs-like editing commands within a command stack.
 * The key bindings for the editing commands are (slightly) configurable.
 */

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include "hist.h"
#include "terminal.h"
#include "have_string.h"


#if defined(USE_TERMIOS)
# include <termios.h>
# define TTYSTRUCT	struct	termios
#else /* USE_SGTTY */
# if defined(USE_TERMIO)
#  include <termio.h>
#  define TTYSTRUCT	struct	termio
# else /* USE_TERMIO */
   /* assume USE_SGTTY */
#  include <sys/ioctl.h>
#  define TTYSTRUCT	struct	sgttyb
# endif /* USE_TERMIO */
#endif /* USE_SGTTY */

#ifdef HAVE_STRING_H
# include <string.h>
#endif


#define	STDIN		0
#define	SAVE_SIZE	256		/* size of save buffer */
#define	MAX_KEYS	60		/* number of key bindings */


#define CONTROL(x)		((char)(((int)(x)) & 0x1f))

static	struct {
	char	*prompt;
	char	*buf;
	char	*pos;
	char	*end;
	char	*mark;
	int	bufsize;
	int	linelen;
	int	histcount;
	int	curhist;
} HS;


typedef	void (*FUNCPTR)();

typedef struct {
	char	*name;
	FUNCPTR	func;
} FUNC;


static void	flush_input(), start_of_line(), end_of_line();
static void	forward_char(), backward_char(), forward_word();
static void	backward_word(), delete_char(), forward_kill_char();
static void	backward_kill_char(), forward_kill_word(), kill_line();
static void	new_line(), save_line(), forward_history();
static void	backward_history(), insert_char();
static void	goto_line(), list_history(), refresh_line(), swap_chars();
static void	set_mark(), yank(), save_region(), kill_region();
static void	reverse_search(), quote_char(), uppercase_word();
static void	lowercase_word(), ignore_char(), arrow_key(), quit_calc();


static	FUNC	funcs[] =
{
	{"ignore-char",		ignore_char},
	{"flush-input",		flush_input},
	{"start-of-line",	start_of_line},
	{"end-of-line",		end_of_line},
	{"forward-char",	forward_char},
	{"backward-char",	backward_char},
	{"forward-word",	forward_word},
	{"backward-word",	backward_word},
	{"delete-char",		delete_char},
	{"forward-kill-char",	forward_kill_char},
	{"backward-kill-char",	backward_kill_char},
	{"forward-kill-word",	forward_kill_word},
	{"uppercase-word",	uppercase_word},
	{"lowercase-word",	lowercase_word},
	{"kill-line",		kill_line},
	{"goto-line",		goto_line},
	{"new-line",		new_line},
	{"save-line",		save_line},
	{"forward-history",	forward_history},
	{"backward-history",	backward_history},
	{"insert-char",		insert_char},
	{"list-history",	list_history},
	{"refresh-line",	refresh_line},
	{"swap-chars",		swap_chars},
	{"set-mark",		set_mark},
	{"yank",		yank},
	{"save-region",		save_region},
	{"kill-region",		kill_region},
	{"reverse-search",	reverse_search},
	{"quote-char",		quote_char},
	{"arrow-key",		arrow_key},
	{"quit",		quit_calc},
	{NULL, 			NULL}
};


typedef struct key_ent	KEY_ENT;
typedef struct key_map	KEY_MAP;

struct key_ent	{
	FUNCPTR		func;
	KEY_MAP		*next;
};


struct key_map {
	char		*name;
	KEY_ENT		default_ent;
	KEY_ENT		*map[256];
};


static char	base_map_name[] = "base-map";
static char	esc_map_name[] = "esc-map";


static KEY_MAP	maps[] = {
	{base_map_name},
	{esc_map_name}
};


#define	INTROUND	(sizeof(int) - 1)
#define	HISTLEN(hp)	((((hp)->len + INTROUND) & ~INTROUND) + sizeof(int))
#define	HISTOFFSET(hp)	(((char *) (hp)) - histbuf)
#define	FIRSTHIST	((HIST *) histbuf)
#define	NEXTHIST(hp)	((HIST *) (((char *) (hp)) + HISTLEN(hp)))


typedef struct {
	int	len;		/* length of data */
	char	data[1];	/* varying length data */
} HIST;


static	int		inited;
static	int		canedit;
static	int		histused;
static	int		key_count;
static	int		save_len;
static	TTYSTRUCT	oldtty;
static	KEY_MAP		*cur_map;
static	KEY_MAP		*base_map;
static	KEY_ENT		key_table[MAX_KEYS];
static	char		histbuf[HIST_SIZE + 1];
static	char		save_buffer[SAVE_SIZE];


static	FUNCPTR	find_func();
static	HIST	*get_event();
static	HIST	*find_event();
static	void	read_key();
static	void	erasechar();
static	void	newline();
static	void	backspace();
static	void	beep();
static	void	echo_char();
static	void	echo_string();
static	void	savetext();
static	void	memrcpy();
static	int	read_bindings();
static	int	in_word();


/*
 * Read a line into the specified buffer.  The line ends in a newline,
 * and is NULL terminated.  Returns the number of characters read, or
 * zero on an end of file or error.  The prompt is printed before reading
 * the line.
 */
int
hist_getline(prompt, buf, len)
	char	*prompt;
	char	*buf;
	int	len;
{
	if (!inited)
		(void) hist_init((char *) NULL);

	HS.prompt = prompt;
	HS.bufsize = len - 2;
	HS.buf = buf;
	HS.pos = buf;
	HS.end = buf;
	HS.mark = NULL;
	HS.linelen = -1;

	fputs(prompt, stdout);
	fflush(stdout);

	if (!canedit) {
		if (fgets(buf, len, stdin) == NULL)
			return 0;
		return strlen(buf);
	}

	while (HS.linelen < 0)
		read_key();

	return HS.linelen;
}


/*
 * Initialize the module by reading in the key bindings from the specified
 * filename, and then setting the terminal modes for noecho and cbreak mode.
 * If the supplied filename is NULL, then a default filename will be used.
 * Returns zero if successful, or a nonzero error code if unsuccessful.
 * If this routine fails, hist_getline, hist_saveline, and hist_term can
 * still be called but all fancy editing is disabled.
 */
int
hist_init(filename)
	char	*filename;
{
	TTYSTRUCT	newtty;

	if (inited)
		return HIST_INITED;

	inited = 1;
	canedit = 0;

	if (filename == NULL)
		filename = HIST_BINDING_FILE;

	if (read_bindings(filename))
		return HIST_NOFILE;

#ifdef	USE_SGTTY
	if (ioctl(STDIN, TIOCGETP, &oldtty) < 0)
		return HIST_NOTTY;

	newtty = oldtty;
	newtty.sg_flags &= ~ECHO;
	newtty.sg_flags |= CBREAK;

	if (ioctl(STDIN, TIOCSETP, &newtty) < 0)
		return HIST_NOTTY;
#endif

#ifdef	USE_TERMIO
	if (ioctl(STDIN, TCGETA, &oldtty) < 0)
		return HIST_NOTTY;

	newtty = oldtty;
	newtty.c_lflag &= ~(ECHO | ECHOE | ECHOK);
	newtty.c_iflag |= ISTRIP;
	newtty.c_lflag &= ~ICANON;
	newtty.c_cc[VMIN] = 1;
	newtty.c_cc[VTIME] = 0;

	if (ioctl(STDIN, TCSETAW, &newtty) < 0)
		return HIST_NOTTY;
#endif

#ifdef	USE_TERMIOS
	if (tcgetattr(STDIN, &oldtty) < 0)
		return HIST_NOTTY;

	newtty = oldtty;
	newtty.c_lflag &= ~(ECHO | ECHOE | ECHOK);
	newtty.c_iflag |= ISTRIP;
	newtty.c_lflag &= ~ICANON;
	newtty.c_cc[VMIN] = 1;
	newtty.c_cc[VTIME] = 0;

	if (tcsetattr(STDIN, TCSANOW, &newtty) < 0)
		return HIST_NOTTY;
#endif

	canedit = 1;

	return HIST_SUCCESS;
}


/*
 * Reset the terminal modes just before exiting.
 */
void
hist_term()
{
	if (!inited || !canedit) {
		inited = 0;
		return;
	}

#ifdef	USE_SGTTY
	(void) ioctl(STDIN, TIOCSETP, &oldtty);
#endif

#ifdef	USE_TERMIO
	(void) ioctl(STDIN, TCSETAW, &oldtty);
#endif

#ifdef	USE_TERMIOS
	(void) tcsetattr(STDIN, TCSANOW, &oldtty);
#endif
}


static KEY_MAP *
find_map(map)
	char	*map;
{
	int	i;

	for (i = 0; i < sizeof(maps) / sizeof(maps[0]); i++) {
		if (strcmp(map, maps[i].name) == 0)
			return &maps[i];
	}
	return NULL;
}


static void
unbind_key(map, key)
	int key;
	KEY_MAP		*map;
{
	map->map[key] = NULL;
}


static void
raw_bind_key(map, key, func, next_map)
	int key;
	KEY_MAP		*map;
	FUNCPTR		func;
	KEY_MAP		*next_map;
{
	if (map->map[key] == NULL) {
		if (key_count >= MAX_KEYS)
			return;
		map->map[key] = &key_table[key_count++];
	}
	map->map[key]->func = func;
	map->map[key]->next = next_map;
}


static KEY_MAP *
do_map_line(line)
	char	line[];
{
	char	*cp;
	char	*map_name;

	cp = line;
	while (isspace(*cp))
		cp++;
	if (*cp == '\0')
		return NULL;
	map_name = cp;
	while ((*cp != '\0') && !isspace(*cp))
		cp++;
	*cp = '\0';
	return find_map(map_name);
}


static void
do_bind_line(map, line)
	KEY_MAP		*map;
	char		line[];
{
	char		*cp;
	char		key;
	char		*func_name;
	char		*next_name;
	KEY_MAP		*next;
	FUNCPTR		func;

	if (map == NULL)
		return;
	cp = line;
	key = *cp++;
	if (*cp == '\0') {
		unbind_key(map, key);
		return;
	}
	if (key == '^') {
		if (*cp == '?') {
			key = 0177;
			cp++;
		} else
			key = CONTROL(*cp++);
	}
	else if (key == '\\')
		key = *cp++;

	while (isspace(*cp))
		cp++;
	if (*cp == '\0') {
		unbind_key(map, key);
		return;
	}

	func_name = cp;
	while ((*cp != '\0') && !isspace(*cp))
		cp++;
	if (*cp) {
		*cp++ = '\0';
		while (isspace(*cp))
			cp++;
	}
	func = find_func(func_name);
	if (func == NULL) {
		fprintf(stderr, "Unknown function \"%s\"\n", func_name);
		return;
	}

	if (*cp == '\0') {
		next = map->default_ent.next;
		if (next == NULL)
			next = base_map;
	} else {
		next_name = cp;
		while ((*cp != '\0') && !isspace(*cp))
			cp++;
		if (*cp) {
			*cp++ = '\0';
			while (isspace(*cp))
				cp++;
		}
		next = find_map(next_name);
		if (next == NULL)
			return;
	}
	raw_bind_key(map, key, func, next);
}


static void
do_default_line(map, line)
	KEY_MAP		*map;
	char		*line;
{
	char		*cp;
	char		*func_name;
	char		*next_name;
	KEY_MAP		*next;
	FUNCPTR		func;

	if (map == NULL)
		return;
	cp = line;
	while (isspace(*cp))
		cp++;
	if (*cp == '\0')
		return;

	func_name = cp;
	while ((*cp != '\0') && !isspace(*cp))
		cp++;
	if (*cp != '\0')
	{
		*cp++ = '\0';
		while (isspace(*cp))
			cp++;
	}
	func = find_func(func_name);
	if (func == NULL)
		return;

	if (*cp == '\0')
		next = map;
	else
	{
		next_name = cp;
		while ((*cp != '\0') && !isspace(*cp))
			cp++;
		if (*cp != '\0')
		{
			*cp++ = '\0';
			while (isspace(*cp))
				cp++;
		}
		next = find_map(next_name);
		if (next == NULL)
			return;
	}

	map->default_ent.func = func;
	map->default_ent.next = next;
}


/*
 * Read bindings from specified file.
 * Returns nonzero on error.
 */
static int
read_bindings(bindfile)
	char	*bindfile;
{
	char	*cp;
	KEY_MAP	*input_map;
	FILE	*fp;
	char	line[100];

	base_map = find_map(base_map_name);
	cur_map = base_map;
	input_map = base_map;

	fp = fopen(bindfile, "r");
	if (fp == NULL)
		return 1;

	while (fgets(line, sizeof(line) - 1, fp)) {
		cp = line;
		while (isspace(*cp))
			cp++;

		if ((*cp == '\0') || (*cp == '#') || (*cp == '\n'))
			continue;

		if (cp[strlen(cp) - 1] == '\n')
			cp[strlen(cp) - 1] = '\0';

		if (memcmp(cp, "map", 3) == 0)
			input_map = do_map_line(&cp[3]);
		else if (memcmp(cp, "default", 7) == 0)
			do_default_line(input_map, &cp[7]);
		else
			do_bind_line(input_map, cp);
	}
	fclose(fp);
	return 0;
}


static void
read_key()
{
	KEY_ENT		*ent;
	int		key;

	fflush(stdout);
	key = fgetc(stdin);
	if (key == EOF) {
		HS.linelen = 0;
		HS.buf[0] = '\0';
		return;
	}

	ent = cur_map->map[key];
	if (ent == NULL)
		ent = &cur_map->default_ent;
	if (ent->next)
		cur_map = ent->next;
	if (ent->func)
		(*ent->func)(key);
	else
		insert_char(key);
}


/*
 * Return the Nth history event, indexed from zero.
 * Earlier history events are lower in number.
 */
static HIST *
get_event(n)
	int n;
{
	register HIST *	hp;

	if ((n < 0) || (n >= HS.histcount))
		return NULL;
	hp = FIRSTHIST;
	while (n-- > 0)
		hp = NEXTHIST(hp);
	return hp;
}


/*
 * Search the history list for the specified pattern.
 * Returns the found history, or NULL.
 */
static HIST *
find_event(pat, len)
	int len;
	char *	pat;
{
	register HIST *	hp;

	for (hp = FIRSTHIST; hp->len; hp = NEXTHIST(hp)) {
		if ((hp->len == len) && (memcmp(hp->data, pat, len) == 0))
		    	return hp;
	}
	return NULL;
}


/*
 * Insert a line into the end of the history table.
 * If the line already appears in the table, then it is moved to the end.
 * If the table is full, then the earliest commands are deleted as necessary.
 * Warning: the incoming line cannot point into the history table.
 */
void
hist_saveline(line, len)
	int len;
	char *	line;
{
	HIST *	hp;
	HIST *	hp2;
	int	left;

	if ((len > 0) && (line[len - 1] == '\n'))
		len--;
	if (len <= 0)
		return;

	/*
	 * See if the line is already present in the history table.
	 * If so, and it is already at the end, then we are all done.
	 * Otherwise delete it since we will reinsert it at the end.
	 */
	hp = find_event(line, len);
	if (hp) {
		hp2 = NEXTHIST(hp);
		left = histused - HISTOFFSET(hp2);
		if (left <= 0)
			return;
		histused -= HISTLEN(hp);
		memcpy(hp, hp2, left + 1);
		HS.histcount--;
	}

	/*
	 * If there is not enough room left in the history buffer to add
	 * the new command, then repeatedly delete the earliest command
	 * as many times as necessary in order to make enough room.
	 */
	while ((histused + len) >= HIST_SIZE) {
		hp = (HIST *) histbuf;
		hp2 = NEXTHIST(hp);
		left = histused - HISTOFFSET(hp2);
		histused -= HISTLEN(hp);
		memcpy(hp, hp2, left + 1);
		HS.histcount--;
	}

	/*
	 * Add the line to the end of the history table.
	 */
	hp = (HIST *) &histbuf[histused];
	hp->len = len;
	memcpy(hp->data, line, len);
	histused += HISTLEN(hp);
	histbuf[histused] = 0;
	HS.curhist = ++HS.histcount;
}


/*
 * Find the function for a specified name.
 */
static FUNCPTR
find_func(name)
	char	*name;
{
	FUNC	*fp;

	for (fp = funcs; fp->name; fp++) {
		if (strcmp(fp->name, name) == 0)
			return fp->func;
	}
	return NULL;
}


static void
arrow_key()
{
	switch (fgetc(stdin)) {
		case 'A':
			backward_history();
			break;
		case 'B':
			forward_history();
			break;
		case 'C':
			forward_char();
			break;
		case 'D':
			backward_char();
			break;
	}
}


static void
back_over_char(ch)
	char	ch;
{
	backspace();
	if (!isprint(ch))
		backspace();
}


static void
remove_char(ch)
	char	ch;
{
	erasechar();
	if (!isprint(ch))
		erasechar();
}


static void
echo_rest_of_line()
{
	echo_string(HS.pos, HS.end - HS.pos);
}


static void
goto_start_of_line()
{
	while (HS.pos > HS.buf)
		back_over_char(*--HS.pos);
}


static void
goto_end_of_line()
{
	echo_rest_of_line();
	HS.pos = HS.end;
}


static void
decrement_end(n)
	int n;
{
	HS.end -= n;
	if (HS.mark && (HS.mark > HS.end))
		HS.mark = NULL;
}


static void
ignore_char()
{
}


static void
flush_input()
{
	echo_rest_of_line();
	while (HS.end > HS.buf)
		remove_char(*--HS.end);
	HS.pos = HS.buf;
	HS.mark = NULL;
}


static void
start_of_line()
{
	goto_start_of_line();
}


static void
end_of_line()
{
	goto_end_of_line();
}


static void
forward_char()
{
	if (HS.pos < HS.end)
		echo_char(*HS.pos++);
}


static void
backward_char()
{
	if (HS.pos > HS.buf)
		back_over_char(*--HS.pos);
}


static void
uppercase_word()
{
	while ((HS.pos < HS.end) && !in_word(*HS.pos))
		echo_char(*HS.pos++);
	while ((HS.pos < HS.end) && in_word(*HS.pos)) {
		if ((*HS.pos >= 'a') && (*HS.pos <= 'z'))
			*HS.pos += 'A' - 'a';
		echo_char(*HS.pos++);
	}
}


static void
lowercase_word()
{
	while ((HS.pos < HS.end) && !in_word(*HS.pos))
		echo_char(*HS.pos++);
	while ((HS.pos < HS.end) && in_word(*HS.pos)) {
		if ((*HS.pos >= 'A') && (*HS.pos <= 'Z'))
			*HS.pos += 'a' - 'A';
		echo_char(*HS.pos++);
	}
}


static void
forward_word()
{
	while ((HS.pos < HS.end) && !in_word(*HS.pos))
		echo_char(*HS.pos++);
	while ((HS.pos < HS.end) && in_word(*HS.pos))
		echo_char(*HS.pos++);
}


static void
backward_word()
{
	if ((HS.pos > HS.buf) && in_word(*HS.pos))
		back_over_char(*--HS.pos);
	while ((HS.pos > HS.buf) && !in_word(*HS.pos))
		back_over_char(*--HS.pos);
	while ((HS.pos > HS.buf) && in_word(*HS.pos))
		back_over_char(*--HS.pos);
	if ((HS.pos < HS.end) && !in_word(*HS.pos))
		echo_char(*HS.pos++);
}


static void
forward_kill_char()
{
	int	rest;
	char	ch;

	rest = HS.end - HS.pos;
	if (rest-- <= 0)
		return;
	ch = *HS.pos;
	if (rest > 0) {
		memcpy(HS.pos, HS.pos + 1, rest);
		*(HS.end - 1) = ch;
	}
	echo_rest_of_line();
	remove_char(ch);
	decrement_end(1);
	while (rest > 0)
		back_over_char(HS.pos[--rest]);
}


static void
delete_char()
{
	if (HS.end > HS.buf)
		forward_kill_char();
}


static void
backward_kill_char()
{
	if (HS.pos > HS.buf) {
		HS.pos--;
		back_over_char(*HS.pos);
		forward_kill_char();
	}
}


static void
forward_kill_word()
{
	char	*cp;

	if (HS.pos >= HS.end)
		return;
	echo_rest_of_line();
	for (cp = HS.end; cp > HS.pos;)
		remove_char(*--cp);
	cp = HS.pos;
	while ((cp < HS.end) && !in_word(*cp))
		cp++;
	while ((cp < HS.end) && in_word(*cp))
		cp++;
	savetext(HS.pos, cp - HS.pos);
	memcpy(HS.pos, cp, HS.end - cp);
	decrement_end(cp - HS.pos);
	echo_rest_of_line();
	for (cp = HS.end; cp > HS.pos;)
		back_over_char(*--cp);
}


static void
kill_line()
{
	if (HS.end <= HS.pos)
		return;
	savetext(HS.pos, HS.end - HS.pos);
	echo_rest_of_line();
	while (HS.end > HS.pos)
		remove_char(*--HS.end);
	decrement_end(0);
}


/*
 * This is the function which completes a command line editing session.
 * The final line length is returned in the HS.linelen variable.
 * The line is NOT put into the edit history, so that the caller can
 * decide whether or not this should be done.
 */
static void
new_line()
{
	int	len;

	newline();
	fflush(stdout);

	HS.mark = NULL;
	HS.end[0] = '\n';
	HS.end[1] = '\0';
	len = HS.end - HS.buf + 1;
	if (len <= 1) {
		HS.curhist = HS.histcount;
		HS.linelen = 1;
		return;
	}
	HS.curhist = HS.histcount;
	HS.pos = HS.buf;
	HS.end = HS.buf;
	HS.linelen = len;
}


static void
save_line()
{
	int	len;

	len = HS.end - HS.buf;
	if (len > 0) {
		hist_saveline(HS.buf, len);
		flush_input();
	}
	HS.curhist = HS.histcount;
}


static void
goto_line()
{
	int	num;
	char	*cp;
	HIST	*hp;

	num = 0;
	cp = HS.buf;
	while ((*cp >= '0') && (*cp <= '9') && (cp < HS.pos))
		num = num * 10 + (*cp++ - '0');
	if ((num <= 0) || (num > HS.histcount) || (cp != HS.pos)) {
		beep();
		return;
	}
	flush_input();
	HS.curhist = HS.histcount - num;
	hp = get_event(HS.curhist);
	memcpy(HS.buf, hp->data, hp->len);
	HS.end = &HS.buf[hp->len];
	goto_end_of_line();
}


static void
forward_history()
{
	HIST	*hp;

	flush_input();
	if (++HS.curhist >= HS.histcount)
		HS.curhist = 0;
	hp = get_event(HS.curhist);
	if (hp) {
		memcpy(HS.buf, hp->data, hp->len);
		HS.end = &HS.buf[hp->len];
	}
	goto_end_of_line();
}


static void
backward_history()
{
	HIST	*hp;

	flush_input();
	if (--HS.curhist < 0)
		HS.curhist = HS.histcount - 1;
	hp = get_event(HS.curhist);
	if (hp) {
		memcpy(HS.buf, hp->data, hp->len);
		HS.end = &HS.buf[hp->len];
	}
	goto_end_of_line();
}


static void
insert_char(key)
	int key;
{
	int	len;
	int	rest;

	len = HS.end - HS.buf;
	if (len >= HS.bufsize) {
		beep();
		return;
	}
	rest = HS.end - HS.pos;
	if (rest > 0)
		memrcpy(HS.pos + 1, HS.pos, rest);
	HS.end++;
	*HS.pos++ = key;
	echo_char(key);
	echo_rest_of_line();
	while (rest > 0)
		back_over_char(HS.pos[--rest]);
}


static void
insert_string(str, len)
	int len;
	char	*str;
{
	int	rest;
	int	totallen;

	if (len <= 0)
		return;
	totallen = (HS.end - HS.buf) + len;
	if (totallen > HS.bufsize) {
		beep();
		return;
	}
	rest = HS.end - HS.pos;
	if (rest > 0)
		memrcpy(HS.pos + len, HS.pos, rest);
	HS.end += len;
	memcpy(HS.pos, str, len);
	HS.pos += len;
	echo_string(str, len);
	echo_rest_of_line();
	while (rest > 0)
		back_over_char(HS.pos[--rest]);
}


static void
list_history()
{
	HIST	*hp;
	int	num;

	for (num = 0; num < HS.histcount; num++) {
		hp = get_event(num);
		printf("\n%3d: ", HS.histcount - num);
		echo_string(hp->data, hp->len);
	}
	refresh_line();
}


static void
refresh_line()
{
	char	*cp;

	newline();
	fputs(HS.prompt, stdout);
	if (HS.end > HS.buf) {
		echo_string(HS.buf, HS.end - HS.buf);
		cp = HS.end;
		while (cp > HS.pos)
			back_over_char(*--cp);
	}
}


static void
swap_chars()
{
	char	ch1;
	char	ch2;

	if ((HS.pos <= HS.buf) || (HS.pos >= HS.end))
		return;
	ch1 = *HS.pos--;
	ch2 = *HS.pos;
	*HS.pos++ = ch1;
	*HS.pos = ch2;
	back_over_char(ch2);
	echo_char(ch1);
	echo_char(ch2);
	back_over_char(ch2);
}


static void
set_mark()
{
	HS.mark = HS.pos;
}


static void
save_region()
{
	int	len;

	if (HS.mark == NULL)
		return;
	len = HS.mark - HS.pos;
	if (len > 0)
		savetext(HS.pos, len);
	if (len < 0)
		savetext(HS.mark, -len);
}


static void
kill_region()
{
	char	*cp;
	char	*left;
	char	*right;

	if ((HS.mark == NULL) || (HS.mark == HS.pos))
		return;

	echo_rest_of_line();
	if (HS.mark < HS.pos) {
		left = HS.mark;
		right = HS.pos;
		HS.pos = HS.mark;
	} else {
		left = HS.pos;
		right = HS.mark;
		HS.mark = HS.pos;
	}
	savetext(left, right - left);
	for (cp = HS.end; cp > left;)
		remove_char(*--cp);
	if (right < HS.end)
		memcpy(left, right, HS.end - right);
	decrement_end(right - left);
	echo_rest_of_line();
	for (cp = HS.end; cp > HS.pos;)
		back_over_char(*--cp);
}


static void
yank()
{
	insert_string(save_buffer, save_len);
}


static void
reverse_search()
{
	int	len;
	int	count;
	int	testhist;
	HIST	*hp;
	char	*save_pos;

	count = HS.histcount;
	len = HS.pos - HS.buf;
	if (len <= 0)
		count = 0;
	testhist = HS.curhist;
	do {
		if (--count < 0) {
			beep();
			return;
		}
		if (--testhist < 0)
			testhist = HS.histcount - 1;
		hp = get_event(testhist);
	} while ((hp == NULL) || (hp->len < len) ||
		memcmp(hp->data, HS.buf, len));

	HS.curhist = testhist;
	save_pos = HS.pos;
	flush_input();
	memcpy(HS.buf, hp->data, hp->len);
	HS.end = &HS.buf[hp->len];
	goto_end_of_line();
	while (HS.pos > save_pos)
		back_over_char(*--HS.pos);
}


static void
quote_char()
{
	int	ch;

	ch = fgetc(stdin);
	if (ch != EOF)
		insert_char(ch);
}


/*
 * Save data in the save buffer.
 */
static void
savetext(str, len)
	int len;
	char	*str;
{
	save_len = 0;
	if (len <= 0)
		return;
	if (len > SAVE_SIZE)
		len = SAVE_SIZE;
	memcpy(save_buffer, str, len);
	save_len = len;
}


/*
 * Test whether a character is part of a word.
 */
static int
in_word(ch)
	char	ch;
{
	return (isalnum(ch) || (ch == '_'));
}


static void
erasechar()
{
	fputs("\b \b", stdout);
}


static void
newline()
{
	fputc('\n', stdout);
}


static void
backspace()
{
	fputc('\b', stdout);
}


static void
beep()
{
	fputc('\007', stdout);
}


static void
echo_char(ch)
	int ch;
{
	if (isprint(ch))
		putchar(ch);
	else {
		putchar('^');
		putchar((ch + '@') & 0x7f);
	}
}


static void
echo_string(str, len)
	int len;
	char	*str;
{
	while (len-- > 0)
		echo_char(*str++);
}


static void
memrcpy(dest, src, len)
	int len;
	char	*dest, *src;
{
	dest += len - 1;
	src += len - 1;
	while (len-- > 0)
		*dest-- = *src--;
}


static void
quit_calc()
{
	hist_term();
	putchar('\n');
	exit(0);
}


#ifdef	HIST_TEST

/*
 * Main routine to test history.
 */
main(argc, argv)
	int	argc;
	char	*argv[];
{
	char	*filename;
	int	len;
	char	buf[256];

	filename = NULL;
	if (argc > 1)
		filename = argv[1];

	switch (hist_init(filename)) {
		case HIST_SUCCESS:
			break;
		case HIST_NOFILE:
			fprintf(stderr, "Binding file was not found\n");
			break;
		case HIST_NOTTY:
			fprintf(stderr, "Cannot set terminal parameters\n");
			break;
		case HIST_INITED:
			fprintf(stderr, "Hist is already inited\n");
			break;
		default:
			fprintf(stderr, "Unknown error from hist_init\n");
			break;
	}

	do {
		len = hist_getline("HIST> ", buf, sizeof(buf));
		hist_saveline(buf, len);		
	} while (len && (buf[0] != 'q'));

	hist_term();

	return 0;
}
#endif

/* END CODE */
