/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "table.h"

private Table	*tables = NIL;

Table *
make_table()
{
	Table	*tab = (Table *) emalloc(sizeof *tab);

	tab->t_next = tables;
	tables = tab;
	tab->t_wordlist = NIL;

	return tab;
}

Word *
word_in_table(text, table)
char	*text;
Table	*table;
{
	register Word	*w;

	for (w = table_top(table); w != NIL; w = next_word(w))
		if (strcmp(word_text(w), text) == 0)
			break;	/* already in list */
	return w;
}

void
add_word(wname, table)
char	*wname;
Table	*table;
{
	register Word	*w;

	if (w = word_in_table(wname, table))
		return;
	w = (Word *) emalloc(sizeof *w);
	word_text(w) = wname;
	next_word(w) = table_top(table);
	table_top(table) = w;
}
