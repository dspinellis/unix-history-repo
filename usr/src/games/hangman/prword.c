# include	"hangman.h"

/*
 * prword:
 *	Print out the current state of the word
 */
prword()
{
	move(KNOWNY, KNOWNX + sizeof "Word: ");
	addstr(Known);
	clrtoeol();
}
