/*
 * Test program for dbx call command.
 */

int global;

main (argc, argv)
int argc;
char *argv[];
{
    int main_local;

    global = 2;
    main_local = 19;
    p1();
    p2(main_local);
    p3("test");
}

p1 ()
{
    printf("in p1\n");
    global = 4;
}

p2 (from_main)
int from_main;
{
    printf("in p2(%d)\n", from_main);
    global = 9;
}

p3 (s)
char s[];
{
    printf("in p3(%s)\n", s);
    global = 10;
}
