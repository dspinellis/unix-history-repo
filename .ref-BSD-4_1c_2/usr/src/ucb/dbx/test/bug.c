typedef struct blah *Blah;
typedef struct List *List;

struct List {
    int b;
    char c;
};

struct blah {
    int a;
    List p;
};

main()
{
    struct blah b, *p;
    struct List list;

    p = &b;
    b.a = 3;
    b.p = &list;
    b.p->b = 4;
    b.p->c = 'c';
}
