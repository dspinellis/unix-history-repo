#include <stdio.h>

rin()
{
	double d;
	register n, c, f;

	d = 0.;
	f = 0;
	n = 0;
loop:
	c = getchar();
	if(c == EOF)
		exit();
	if(c == '-') {
		f++;
		goto loop;
	}
	if(c == '.') {
		n++;
		goto loop;
	}
	if(isdigit(c)) {
		if(n)
			n++;
		d = d*10.+c-'0';
		goto loop;
	}
	if(f)
		d = -d;
	for(f=1; f<n; f++)
		d /= 10.;
	n = d;
	return(n);
}
