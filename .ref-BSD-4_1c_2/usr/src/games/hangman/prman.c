# include	"hangman.h"

/*
 * prman:
 *	Print out the man appropriately for the give number
 *	of incorrect guesses.
 */
prman()
{
	register int	i;

	for (i = 0; i < Errors; i++)
		mvaddch(Err_pos[i].y, Err_pos[i].x, Err_pos[i].ch);
	while (i < MAXERRS) {
		mvaddch(Err_pos[i].y, Err_pos[i].x, ' ');
		i++;
	}
}
