typedef struct proc *proc_p;

struct proc {
    int a;
    int b;
};

main ()
{
    struct proc p;

    p.a = 3;
    p.b = 4;
}
