/*
 * Test of floats and doubles.
 */

double f(x)
double x;
{
    return 3.14*x;
}

main()
{
    double x;
    float y;

    y = 3.0;
    x = f(y);
    return 0;
}
