typedef enum { RED, GREEN, BLUE } Color;

main()
{
    Color c;

    c = BLUE;
    f(RED);
}

f(c)
Color c;
{
    printf("c = %d\n", c);
}
