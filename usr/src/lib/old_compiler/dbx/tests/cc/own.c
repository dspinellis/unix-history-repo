/*
 * Test of static variables.
 */
 
static int ownx;

main()
{
    ownx = 2;
    f(3);
    f(4);
    return(0);
}

static int owny;

f(x)
int x;
{
    static int ownx;

    ownx = x;
}
