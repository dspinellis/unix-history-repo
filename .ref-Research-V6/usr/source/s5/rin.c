rin()
{
	double d;
	register n, c, f;

	d = 0.;
	f = 0;
	n = 0;
loop:
	c = getchar();
	if(c == '\0')
		exit();
	if(c == '-') {
		f++;
		goto loop;
	}
	if(c == '.') {
		n++;
		goto loop;
	}
	if(c>='0' && c<='9') {
		if(n)
			n++;
		d = d*10.+c-'0';
		goto loop;
	}
	if(f)
		d = -d;
	for(f=1; f<n; f++)
		d =/ 10.;
	n = d;
	return(n);
}
