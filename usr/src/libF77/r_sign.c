double r_sign(a,b)
float *a, *b;
{
float x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}
