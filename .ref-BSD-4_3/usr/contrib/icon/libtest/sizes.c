#include "../h/rt.h"
#include <stdio.h>


main()
   {

   double x;
   printf("size of double = %d\n",sizeof(x));
   printf("size of integer = %d\n",sizeof(int *));
   printf("size of descrip = %d\n",sizeof(struct descrip));
   printf("size of b_int = %d\n",sizeof(struct b_int));
   printf("size of b_real = %d\n",sizeof(struct b_real));
   printf("size of b_cset = %d\n",sizeof(struct b_cset));
   printf("size of b_file = %d\n",sizeof(struct b_file));
   printf("size of b_proc = %d\n",sizeof(struct b_proc));
   printf("size of b_iproc = %d\n",sizeof(struct b_iproc));
   printf("size of b_list = %d\n",sizeof(struct b_list));
   printf("size of b_lelem = %d\n",sizeof(struct b_lelem));
   printf("size of b_table = %d\n",sizeof(struct b_table));
   printf("size of b_telem = %d\n",sizeof(struct b_telem));
   printf("size of b_set = %d\n",sizeof(struct b_set));
   printf("size of b_selem = %d\n",sizeof(struct b_selem));
   printf("size of b_record = %d\n",sizeof(struct b_record));
   printf("size of b_tvsubs = %d\n",sizeof(struct b_tvsubs));
   printf("size of b_tvtbl = %d\n",sizeof(struct b_tvtbl));
   printf("size of b_estack = %d\n",sizeof(struct b_estack));
   printf("size of b_eblock = %d\n",sizeof(struct b_eblock));
   }

