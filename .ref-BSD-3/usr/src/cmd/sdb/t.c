struct {
	char cc;
	int aa;
} s1, s2, s3[2];

main(argc,argv,envp)
char **argv, **envp; {
	s1.cc = 'a';
	s2.cc = 'b';
	s3[0].cc = 'c';
	s3[1].cc = 'd';
	
	s1.aa = 22;
	s2.aa = 33;
	s3[0].aa = 44;
	
	sub(s1.cc);
	
	abort();
}

sub(c)
char c;
{
	register char d;
	
	d = c;
	inner(&c);
}

inner(s)
char *s; {
	char c;
	
	c = *s;
	core(s);
}

core(s)
register char *s; {
	char *p;
	char c;

	p = s;
	c = *p;
}

