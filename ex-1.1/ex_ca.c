#
/*
 * Ex - a text editor
 * Bill Joy September 1977
 */

#include "ex.h"
#include "ex_tty.h"

/*
canCA()
{

	switch (TTY) {

	case 'ca':
	case 'cH':
		return (1);
	}
	return (0);
}

*/

cgoto()
{
	static char result[10];

	switch (TTY) {

	case 'ca':
		strcpy(result, "\033=xx");
		result[2] = destline + ' ';
		result[3] = destcol + ' ';
		break;
#ifdef UNIMP
	case 'cH':
		strcpy(result, "\033&axxcyyY");
		result[3] = destcol / 10 + '0';
		result[4] = destcol % 10 + '0';
		result[6] = destline / 10 + '0';
		result[7] = destline % 10 + '0';
		break;
#endif
	default:
		strcpy(result, "OOPS");
		break;
	}
	return (result);
}
