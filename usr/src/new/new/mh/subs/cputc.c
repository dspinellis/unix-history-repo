#include <stdio.h>

cputc(chr, ip)
register FILE *ip;
{
	if(ip != NULL) {
		putc(chr, ip);
		if(ferror(ip)) {
			perror("Write error");
			done(-1);
		}
	}
}
