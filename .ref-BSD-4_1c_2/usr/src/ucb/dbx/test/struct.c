struct blah {
    int a;
    struct {
	int b;
    } c;
};

struct blah f(x)
int x;
{
    struct blah r;

    r.a = x;
    return r;
}

main()
{
    struct blah x;
    struct blah *y;

    x = f(3);
    y = &x;
}
