#include <stdio.h>
#include "config.h"
char code[256];
#define FIRST 3
#define FOLLOW 2
#define ZERO 6
#define next() (((c = getc(si))==EOF)? exit(0):0)
#define copy() putc(c,so)
#define type() code[(unsigned char)c]
#if os_unix_ts
#define index strchr
#endif

init() {
	doit(FIRST,"ABCDEFGHIJKLMNOPQRSTUVWXYZ_");
	doit(FIRST,"0abcdefghijklmnopqrstuvwxyz_");
	doit(FOLLOW,"123456789");
}
doit(act,list)
register unsigned char *list;
{
	while(*list) {code[*list++]=act;}
}
main( )
{
	register FILE *si = stdin, *so = stdout;
	register c, count;

	init();
copying:
	do { next(); copy();} while(type()!=FIRST);
hexnum:
	if(c=='0') {
		next();
		if(c=='X'||c=='x') {
			do { copy(); next();}
			while (index("0123456789abcdefABCDEF",c&0x7f)>0);
		}
		ungetc(c,si); goto copying;
	}
counting:
	for(count = 0; count < 7; count++) {
		next();
		copy();
		if(!(type()&FOLLOW)) goto copying;
	}
squelch:
	do { next(); } while (type()&FOLLOW);
	copy();
	goto copying;
}
