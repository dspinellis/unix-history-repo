/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

typedef struct word	Word;
typedef struct table	Table;

struct word {
	Word	*wd_next;
	char	*wd_text;
};

struct table {
	Table	*t_next;
	Word	*t_wordlist;
};

extern Table	*make_table();
extern Word	*word_in_table();

#define	table_top(table)	(table->t_wordlist)
#define next_word(w)		(w->wd_next)
#define last_word_p(w)		(w->wd_next == NIL)
#define word_text(w)		(w->wd_text)
#define word_length(w)		(strlen(word_text(w)))
