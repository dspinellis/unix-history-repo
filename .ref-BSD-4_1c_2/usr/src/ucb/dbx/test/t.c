#include <stdio.h>

typedef struct Blah *Blah;

struct recursive {
    int val;
    struct recursive *next;
    struct nest {
	int a;
    } n;
} r;

typedef int Integer;

struct blah { int x, y; } z;
int array[10];
int i = 3;

main(argc, argv)
int argc;
char **argv;
{
    struct blah p;
    int a, b;
    typedef enum color { RED, GREEN, BLUE } Color;
    Color c;
    double x;
    float y;

    printf("testing %s", argv[0]);
    if (argc > 1) {
	printf(" %s", argv[1]);
    }
    putchar('\n');
    c = BLUE;
    x = 3.5;
    y = 4.6;
    abort();
}
