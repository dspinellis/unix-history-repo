type(ch, s)
char *s;
{
	register char *p;

	for (p = s; *p++; );
	--p;
	write(ch, s, p-s);
	return(p-s);
}
