typedef struct Blah *Blah;

struct Blah {
    int a;
    int b;
};

struct Blah x;

int t2;

Blah f()
{
    x.a = 3;
    x.b = 4;
    return &x;
}
