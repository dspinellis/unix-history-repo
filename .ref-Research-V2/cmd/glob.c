/* global command --

   glob params

   "*" in params matches r.e ".*"
   "?" in params matches r.e. "."
   "[...]" in params matches character class
   "[...a-z...]" in params matches a through z.

   perform command with argument list
  constructed as follows:
     if param does not contain "*", "[", or "?", use it as is
     if it does, find all files in current directory
     which match the param, sort them, and use them

   prepend the command name with "/bin" or "/usr/bin"
   as required.
*/

char	ab[2000];		/* generated characters */
char	*ava[200];		/* generated arguments */
char	**av ava;
char	*string ab;

main(argc, argv)
char *argv[];
{
	int i, j, c;
	int inode, dirf, ap;
	int fb[5], sb[17];
	char *cp, *cpo;

	if (argc < 3) {
		write(1, "Arg count\n", 10);
		return;
	}
	ap = 0;
	av++;
	fb[4] = 0;
loop:
	cpo = cp = *++argv;
	while(c = *cp++) if (c=='*' | c=='?' | c=='[') goto compl;
	av[ap++] = copy(cpo);
	if (--argc>=2) goto loop;
	goto donow;

compl:
	if(*--cp == '/') {
		*cp = '\0';
		if((dirf=open(cp==cpo? "/" : cpo, 0))<0)
			goto oper;
		*cp++ = '/';
		goto compl1;
	}
	if(cp != cpo) goto compl;
	if((dirf=open(".",0)) >= 0) goto compl1;
oper:
	write(1, "No directory\n", 13);
	return;
compl1:
	j = ap;
l2:
	while (read(dirf, &inode, 2)>0) {
		read(dirf, fb, 8);
		if (inode==0) goto l2;
		if (match(fb, cp)) {
			c = *cp;
			*cp = '\0';
			av[ap++] = cat(cpo, fb);
			*cp = c;
		}
	}
	close(dirf);
	i = j;
	while(i<ap-1) {
		j = i;
		while(++j<ap) {
			if (compar(av[i],av[j])) {
				c = av[i];
				av[i] = av[j];
				av[j] = c;
			}
		}
		i++;
	}
	if (--argc>=2) goto loop;
donow:
	if (ap<=1) {
		write(1, "No match\n", 9);
		return;
	}
	av[ap] = 0;
	execv(av[0], av);
	i = cat("/bin/", av[0]);
	execv(i, av);
	i = cat("/usr", i);
	execv(i, av);
	if (stat(i, sb) == 0) {
		*av = i;
		*--av = "/bin/sh";
		execv(av[0], av);
	}
	write(1, "No command\n", 11);
}

match(s, p)
char *s, *p; {
	if (*s=='.' & *p!='.') return(0);
	return(amatch(s, p));
}

amatch(s, p)
char *s, *p;
{
	int c, cc, ok, lc, scc;

	scc = *s;
	lc = 077777;
	switch (c = *p) {

	case '[':
		ok = 0;
		while (cc = *++p) {
			switch (cc) {

			case ']':
				if (ok)
					return(amatch(++s, ++p));
				else
					return(0);

			case '-':
				ok =| lc <= scc & scc <= (cc=p[1]);
			}
			if (scc==(lc=cc)) ok++;
		}
		return(0);

	case '?':
	caseq:
		if(scc) return(amatch(++s, ++p));
		return(0);
	case '*':
		return(umatch(s, ++p));
	case 0:
		return(!scc);
	}
	if (c==scc) goto caseq;
	return(0);
}

umatch(s, p)
char *s, *p;
{
	if(*p==0) return(1);
	while(*s)
		if (amatch(s++,p)) return(1);
	return(0);
}

compar(s1,s2)
char *s1, *s2;
{
	int c1,c2;

loop:
	if ((c1 = *s1++)==0) return(0);
	if ((c2 = *s2++)==0) return(1);
	if (c1==c2) goto loop;
	return(c1>c2);
}

copy(s1)
char *s1;
{
	char *ts;

	ts = string;
	while(*string++ = *s1++);
	return(ts);
}

cat(s1, s2)
char *s1, *s2;
{
	char *ts;

	ts = string;
	while(*string++ = *s1++);
	string--;
	while(*string++ = *s2++);
	return(ts);
}
seq;
	return(0);
}

umatch(s, p)
char *s, *p;
{
	if(*p==0) return(1);
	while(*