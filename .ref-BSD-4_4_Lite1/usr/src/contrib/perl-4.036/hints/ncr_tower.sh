optimize='-O0'
ccflags="$ccflags -W2,-Sl,2000"
eval_cflags='large="-W0,-XL"'
teval_cflags=$eval_cflags
d_mkdir=$undef
usemymalloc='y'
mallocsrc='malloc.c'
mallocobj='malloc.o'
