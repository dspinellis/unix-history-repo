main(narg,parg)
    int narg;   char **parg;
{   char *cp1,*cp2;   int fildes,found,iarg;
    char buffer[16];
    int ttyname;

    fildes = open("/etc/utmp",0);
    for (iarg=1;iarg<narg;iarg++)
    {	seek(fildes,0,0);
	cp1 = parg[iarg];
	if (compar(cp1,"is")) continue;
	if (compar(cp1,"are")) continue;
	if (compar(cp1,"and")) continue;
	found = 0;
	while (read(fildes,buffer,16))
	{   ttyname = buffer[8];   buffer[8] = ' ';
	    if (ttyname < 033) {
		ttyname =+ 'a' - 1;
		ttyname =<< 8;
		ttyname =| 'C';
	    }
	    cp2 = buffer;   while (*cp2 != ' ') cp2++;   *cp2 = '\0';
	    if (compar(cp1,buffer))
	    {	/* found him (her)! */
/*
		if (!found)
*/
		    printf("%s is on tty%c",cp1,ttyname), show(ttyname);
/*
		else
		    printf(" and tty%c",ttyname), show(ttyname);
*/
		found++;
	    }
	}
	if (!found)
	    printf("%s is not logged in\n",cp1);
/*
	else
	    printf("\n");
*/
    }
}

compar(s1,s2)
    char *s1,*s2;
{   while (*s1++ == *s2)
    {	if (*s2++ == '\0') return(1);   }
    return(0);
}

show(ttyname)
	int ttyname;
{
	register c;
	int ibuf[259];
	char c1, c2;

	c1 = ttyname & 0377;
	c2 = ttyname >> 8;
	c2 =& 0377;
	if (c2 == 0)
		c2 = '\t';
	close(0);
	if (fopen("/etc/ttywhere", 0) < 0)
		goto bye;
	for (;;) {
		c = getc(ibuf);
		if (c != 't')
			goto skipnl;
		c = getc(ibuf);
		if (c != 't')
			goto skipnl;
		c = getc(ibuf);
		if (c != 'y')
			goto skipnl;
		c = getc(ibuf);
		if (c != c1)
			goto skipnl;
		c = getc(ibuf);
		if (c != c2)
			goto skipnl;
		putchar('\t');
		for (;;) {
			c = getc(ibuf);
			if (c == -1 || c == '\n')
				break;
			putchar(c);
		}
		close(0);
skipnl:
		if (c == -1)
			break;
		while (c != '\n' && c != -1)
			c = getc(ibuf);
	}
bye:
	putchar('\n');
}
