char	*arg;
int	mode[3];

main(argc, argv)
char	*argv[];
{
	int i;

	gtty(1, mode);
	while(--argc > 0) {

		arg = *++argv;
		if(eq("erase") || eq("kill") || eq("ek"))
			mode[1] = '#@';
		if(eq("even"))
			set(0200);
		if(eq("-even"))
			reset(0200);
		if(eq("odd"))
			set(0100);
		if(eq("-odd"))
			reset(0100);
		if(eq("raw"))
			set(040);
		if(eq("-raw") || eq("cooked"))
			reset(040);
		if(eq("-nl"))
			set(020);
		if(eq("nl"))
			reset(020);
		if(eq("echo"))
			set(010);
		if(eq("-echo"))
			reset(010);
		if(eq("lcase"))
			set(04);
		if(eq("-lcase"))
			reset(04);
		if(eq("-tabs"))
			set(02);
		if(eq("tabs"))
			reset(02);
		if(eq("-delay"))
			set(01);
		if(eq("delay"))
			reset(01);
		if(eq("hup"))
			set(0400);
		if(eq("-hup"))
			reset(0400);
		if(eq("tdelay"))
			reset(010000);
		if(eq("-tdelay"))
			set(010000);
		if(arg)
			printf("unknown mode: %s\n", arg);
	}
	stty(1,mode);
}

eq(string)
char *string;
{
	int i;

	if(!arg)
		return(0);
	i = 0;
loop:
	if(arg[i] != string[i])
		return(0);
	if(arg[i++] != '\0')
		goto loop;
	arg = 0;
	return(1);
}

set(b)
{

	mode[2] =| b;
}

reset(b)
{

	mode[2] =& ~b;
}
