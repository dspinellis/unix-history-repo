main()
{
    f(3);
    f(4);
}

f(x)
int x;
{
    static int ownx;

    ownx = x;
}
