/* demonstration of c coded foreign integer-function */

/* the following will be used to extract fixnums out of a list of fixnums */
struct listoffixnumscell
{    struct listoffixnumscell *cdr;
     int *fixnum;
};

struct listcell
{	struct listcell *cdr;
	int car;
};

cfoo(a,b,c,d)
int *a;
double b[];
int *c[];
struct listoffixnumscell *d;
{
    printf("a: %d, b[0]: %f, b[1]: %f\n", *a, b[0], b[1]);
    printf(" c (first): %d   c (second): %d\n",
	       *c[0],*c[1]);
    printf(" ( %d %d ... )\n ", *(d->fixnum), *(d->cdr->fixnum));
    b[1] = 3.1415926;
    return(3);
}

struct listcell *
cmemq(element,list)
int element;
struct listcell *list;
{   
   for( ; list && element != list->car ;  list = list->cdr);
   return(list);
}
