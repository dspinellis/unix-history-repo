#include <sys/cdefs.h>
#include <string.h>

void *
memset(dst, c, n)
	void *dst;
	register int c;
	register size_t n;
{

	if (n != 0) {
		register char *d = dst;

		do
			*d++ = c;
		while (--n != 0);
	}
	return (dst);
}
