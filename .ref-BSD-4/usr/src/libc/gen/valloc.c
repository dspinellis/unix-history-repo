#include <valign.h>

char	*malloc();

char *
valloc(i)
	int i;
{
	char *cp = malloc(i + (VALSIZ-1));
	int j;

	j = ((int)cp + (VALSIZ-1)) &~ (VALSIZ-1);
	return ((char *)j);
}
