
double
t(a, i)
        double a;
{
        fortran float sin(), cos();

        switch(i) {
        case 0:
                return (cos(a*(3.141592/180.)));
        case 1:
                return (sin(a*(3.141592/180.)));
        }
}

main() { printf("%g\n", t(90.,0)); }
