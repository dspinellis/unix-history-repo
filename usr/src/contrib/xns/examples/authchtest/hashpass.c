#include <ctype.h>

unsigned short
hashpass(hpw)
	char *hpw;
{
	long hash;
	register char c;

	hash = 0;
	while ((c = *hpw++) != '\0') {
		hash = (hash*65536) + (isupper(c) ? tolower(c) : c);
		hash %= 65357;
	}
	return((unsigned short) hash);
}
