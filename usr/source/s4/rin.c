rin()
{
	register n, c, f;

	f = 1;
	n = 0;
loop:
	c = getchar();
	if(c == '\0')
		exit();
	if(c == '-') {
		f = -f;
		goto loop;
	}
	if(c>='0' && c<='9') {
		n = n*10+c-'0';
		goto loop;
	}
	return(n*f);
}
