IEH3err (message, a, b, c, d, e)
char message[];
	{
	extern int cgoof;
	printf("ERROR ");
	printf(message, a, b, c, d, e);
	cputc('\n');
	cexit(cgoof);
	}
cgoof 127;
