#include	<stdio.h>
#define	ntof(n)	(&_iob[n])

int	cin	0;
int	cout	1;

cclose(n)
{
	return(fclose(ntof(n)));
}

ceof(n)
{
	return(feof(ntof(n)));
}

cexit(x)
{
	exit(x);
}

cflush(n)
{
	return(fflush(ntof(n)));
}

cgetc(n)
{
	register c;

	if ((c = getc(ntof(n))) < 0)
		return(0);
	return(c);
}

copen(f, m)
char m;
{
	register struct _iobuf *iop;

	if ((iop = fopen(f, &m)) == NULL)
		return(-1);
	return(fileno(iop));
}

cputc(c, n)
{
	putc(c, ntof(n));
}

cread(a, b, c, n)
{
	return(fread(a, b, c, ntof(n)));
}

cwrite(a, b, c, n)
{
	return(fwrite(a, b, c, ntof(n)));
}
getcharz()
{
	register c;

	if ((c = getc(ntof(cin))) < 0)
		return(0);
	return(c);
}

printf(a, b, c, d)
{
	struct _iobuf _strbuf;
	if (a==-1) {
		_strbuf._flag = _IOWRT+_IOSTRG;
		_strbuf._ptr = b;
		_strbuf._cnt = 32767;
		_doprnt(c, &d, &_strbuf);
		putc('\0', &_strbuf);
	} else if (a<=10) {
		_doprnt(b, &c, ntof(a));
	} else
		_doprnt(a, &b, ntof(cout));
}

putcharz(c)
{
	return(putc(c, ntof(cout)));
}

gets(s)
char *s;
{
	register c;
	register char *cs;

	cs = s;
	while ((c = getc(ntof(cin))) != '\n' && c>=0)
		*cs++ = c;
	if (c<0 && cs==s)
		return(NULL);
	*cs++ = '\0';
	return(s);
}

puts(s)
char *s;
{
	register c;

	while (c = *s++)
		putc(c, ntof(cout));
	putc('\n', ntof(cout));
}

rew(n)
{
	rewind(ntof(n));
}

scanf(a, b, c, d)
{
	struct _iobuf strbuf;
	register char *s;

	if (a == -1) {
		strbuf._flag = _IOREAD|_IOSTRG;
		strbuf._ptr = strbuf._base = b;
		strbuf._cnt = 0;
		s = b;
		while (*s++)
			strbuf._cnt++;
		return(_doscan(&strbuf, c, &d));
	} else if (a<=10)
		return(_doscan(ntof(a), b, &c));
	else
		return(_doscan(ntof(cin), a, &b));
}
