/*
 * Test for C structures.
 */

/*
 * A simple nested structure.
 */

struct simple {
    int a;
    char b;
    double c;
    struct {
	int a;
	char b;
	double c;
    } d;
    int e;
    char f;
    double g;
} simple;

/*
 * Mutually recursive structures, using typedefs.
 */

typedef struct first *First;
typedef struct second *Second;

struct second {
    int b;
    char c;
};

struct first {
    int a;
    Second p;
};

UseRecurStructs()
{
    struct first b, *p;
    struct second list;

    p = &b;
    b.a = 3;
    b.p = &list;
    b.p->b = 4;
    b.p->c = 'c';
}

/*
 * Functions returning structures.
 */

struct simple f(x)
int x;
{
    struct simple s;

    s.a = x;
    s.g = 3.14;
    return s;
}

main()
{
    struct simple x;
    struct simple *y;

    UseRecurStructs();
    x = f(3);
    y = &x;
}
