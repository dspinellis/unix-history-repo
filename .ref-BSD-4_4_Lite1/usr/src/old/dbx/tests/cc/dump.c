/*
 * Test of nested blocks.
 */
 
int i;

main ()
{
    i = 3;
    sub();
}

sub ()
{
    int i, j;

    for (i = 1; i <= 10; i++) {
	int j;

	j = j + i;
    }
    j = 0;
    for (i = 11; i <= 20; i++) {
	j = j + i;
    }
}

after ()
{
    int a;

    a = 3;
}
