int	errno;
int	sys_nerr;
char	*sys_errlist[];
perror(s)
char *s;
{
	char *c;

	c = "Unknown error";
	if(errno < sys_nerr)
		c = sys_errlist[errno];
	if(*s == '\0')
		printf("%s", c); else
		printf("%s: %s\n", s, c);
}
