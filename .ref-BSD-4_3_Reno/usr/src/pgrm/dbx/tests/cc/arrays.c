/*
 * Test of debugging arrays in C.
 */

int a[10], *b;

p (i, a, j)
int i, a[], j;
{
    a[3] = i;
    a[4] = j;
}

main ()
{
    int i;

    b = a;
    for (i = 0; i < 10; i++) {
	a[i] = i;
    }
    p(4, a, 5);
}
