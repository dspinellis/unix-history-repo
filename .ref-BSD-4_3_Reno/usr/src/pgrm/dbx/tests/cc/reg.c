struct blah {
    int x;
    int y;
};

main ()
{
    register int i;
    register struct blah *p;
    register char *s;
    struct blah b;
    int j;

    s = "this is a test";
    s += 5;
    j = 0;
    p = &b;
    p->x = 3;
    p->y = 4;
    for (i = 0; i < 2; i++) {
	j = i;
	put(i);
    }
}

static put(i)
register int i;
{
    printf("%d ", i);
}
