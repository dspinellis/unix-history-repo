/*
 *	@(#)system_.c	5.1 (Berkeley) 11/3/86
 */

/* f77 interface to system routine */

system_(s, n)
register char *s;
long int n;
{
char buff[1001];
register char *bp, *blast;

blast = buff + (n<1000 ? n : 1000L);

for(bp = buff ; bp<blast && *s!='\0' ; )
	*bp++ = *s++;
*bp = '\0';
system(buff);
}
