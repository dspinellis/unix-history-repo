
int i;

p2 (i) int i;
{
    if (i < 5) {
	p2(i+1);
    }
}

p1 (i) int i;
{
    p2(i+1);
}

main ()
{
    i = 0;
    p1(i+1);
}
