/*
 * Test of displaying strings compiled into the text segment via -R.
 */

char str[] = "this is a test";

main ()
{
    f("parameter test");
}

f (s)
char *s;
{
    abort();
}
