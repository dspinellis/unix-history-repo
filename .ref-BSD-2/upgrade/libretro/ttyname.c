ttyname(c)
	short c;
{
	static char name[] = "/dev/ttyx";

	name[8] = ttyn(c);
	if (name[8] == 'x')
		return (0);
	return (name);
}
