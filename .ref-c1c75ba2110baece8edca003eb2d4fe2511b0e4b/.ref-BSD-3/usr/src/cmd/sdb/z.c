struct z{
	int i,j,k;
} ;

main() {
	struct z a;

	a.i = 3;
	a.j = 4;
	a.k = 5;

	sub(a,12);

	abort();
}

sub(y,m)
struct z y; {
	y.i++;
}
