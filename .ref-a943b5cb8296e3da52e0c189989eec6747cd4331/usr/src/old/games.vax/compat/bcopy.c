static char sccsid[] = "	bcopy.c	4.1	82/05/12	";

bcopy(from, to, count)
	char *from, *to;
	int count;
{

	asm("	movc3	12(ap),*4(ap),*8(ap)");
}

seteuid(n)
{
}

setegid(n)
{
}
