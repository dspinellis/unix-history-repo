/* su -- become super-user */

char	password[100];
char	pwbuf[100];
int	ttybuf[3];
main()
{
	register char *p, *q;
	extern fin;

	if(getpw(0, pwbuf))
		goto badpw;
	(&fin)[1] = 0;
	p = pwbuf;
	while(*p != ':')
		if(*p++ == '\0')
			goto badpw;
	if(*++p == ':')
		goto ok;
	gtty(0, ttybuf);
	ttybuf[2] =& ~010;
	stty(0, ttybuf);
	printf("password: ");
	q = password;
	while((*q = getchar()) != '\n')
		if(*q++ == '\0')
			return;
	*q = '\0';
	ttybuf[2] =| 010;
	stty(0, ttybuf);
	printf("\n");
	q = crypt(password);
	while(*q++ == *p++);
	if(*--q == '\0' && *--p == ':')
		goto ok;
	goto error;

badpw:
	printf("bad password file\n");
ok:
	setuid(0);
	execl("/bin/sh", "-", 0);
	printf("cannot execute shell\n");
error:
	printf("sorry\n");
}
