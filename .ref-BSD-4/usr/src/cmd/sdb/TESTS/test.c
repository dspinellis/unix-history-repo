int r = 22;
int array[] = { 5, 4, 3, 2, 1};
char s[] = "abcdefg";

ignore(){
	int i;
	i++;
}

main(argc, argv) 
char **argv; {
	char *p;
	p = s;
	test();
}

test() {
	int a,b,c;
	a = 1;
	b = 2;
	c = 3;
	sub (a,b, 14);
}
sub(x,y) {
	x = 2;
	r = 22;
	final(y);
}

final(z) 
register int z; {
	register p,q,a;
	int f,g;
	int x[10];
	struct {
		int aa;
		int bb;
	} d, *pd;
	pd = &d;
	d.aa=23;
	d.bb=34;
	p = 1;
	q = 2;
	a = 3;
	f = 4; 
	{
		int i, j, k;
		i = 2;
		g = 5;
		x[10000] = 2;
		g = 0;
	}
	g = 1;
}
