/*
 * Routines dealing with the "position" table.
 * This is a table which tells the position (in the input file) of the
 * first char on each currently displayed line.
 *
 * {{ The position table is scrolled by moving all the entries.
 *    Would be better to have a circular table 
 *    and just change a couple of pointers. }}
 */

#include "less.h"
#include "position.h"

#define	NPOS	100		/* {{ sc_height must be less than NPOS }} */
static POSITION table[NPOS];	/* The position table */

extern int sc_width, sc_height;

/*
 * Return the position of one of:
 *	the top (first) line on the screen
 *	the second line on the screen
 *	the bottom line on the screen
 *	the line after the bottom line on the screen
 */
	public POSITION
position(where)
	int where;
{
	switch (where)
	{
	case BOTTOM:
		where = sc_height - 2;
		break;
	case BOTTOM_PLUS_ONE:
		where = sc_height - 1;
		break;
	}
	return (table[where]);
}

/*
 * Add a new file position to the bottom of the position table.
 */
	public void
add_forw_pos(pos)
	POSITION pos;
{
	register int i;

	/*
	 * Scroll the position table up.
	 */
	for (i = 1;  i < sc_height;  i++)
		table[i-1] = table[i];
	table[sc_height - 1] = pos;
}

/*
 * Add a new file position to the top of the position table.
 */
	public void
add_back_pos(pos)
	POSITION pos;
{
	register int i;

	/*
	 * Scroll the position table down.
	 */
	for (i = sc_height - 1;  i > 0;  i--)
		table[i] = table[i-1];
	table[0] = pos;
}

/*
 * Initialize the position table, done whenever we clear the screen.
 */
	public void
pos_clear()
{
	register int i;

	for (i = 0;  i < sc_height;  i++)
		table[i] = NULL_POSITION;
}

/*
 * See if the byte at a specified position is currently on the screen.
 * Check the position table to see if the position falls within its range.
 * Return the position table entry if found, -1 if not.
 */
	public int
onscreen(pos)
	POSITION pos;
{
	register int i;

	if (pos < table[0])
		return (-1);
	for (i = 1;  i < sc_height;  i++)
		if (pos < table[i])
			return (i-1);
	return (-1);
}
