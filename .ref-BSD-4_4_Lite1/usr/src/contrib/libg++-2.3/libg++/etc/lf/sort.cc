#include <stdio.h>
#include <ctype.h>
#include <std.h>

/* the next 6 #defines implement a very fast in-line stack abstraction       */

#define MAX_STACK_SIZE 32
#define DISPOSE_STACK(S) (free(S))
#define PUSH(LOW,HIGH) do {top->lo = LOW;top++->hi = HIGH;} while (0)
#define POP(LOW,HIGH)  do {LOW = (--top)->lo;HIGH = top->hi;} while (0)
#define STACK_NOT_EMPTY (stack < top)                
#define SWAP(A,B) do {char *_temp = (A);(A) = (B);(B) = _temp;} while (0)

/* Discontinue quicksort algorithm when partition gets below this size.
   This particular magic number was chosen to work best on a Sun 4/260. */
#define MAX_THRESH 30

/* Data structure for stack of unfulfilled obligations. */
typedef struct 
{
  char **lo;
  char **hi;
} stack_node;

/* Once main array is partially sorted by quicksort the remainder is 
   completely sorted using insertion sort, since this is efficient 
   for partitions below MAX_THRESH size. BASE points to the beginning 
   of the array to sort, and END_PTR points at the very last element in
   the array (*not* one beyond it!). */

inline static void
insert_sort (char **base_ptr, char **end_ptr)
{
  char **run_ptr;
  char **tmp_ptr = base_ptr;
  char **thresh  = end_ptr <? base_ptr + MAX_THRESH;

  /* Find the smallest element in the first threshold and put it at the 
     end of the array.  This is guaranteed to be the smallest element in 
     the array, and it speeds up the inner loop of insertion sort. */

  for (run_ptr = tmp_ptr + 1; run_ptr <= thresh; run_ptr++)
    if (strcmp (*run_ptr, *tmp_ptr) < 0)
      tmp_ptr = run_ptr;

  SWAP (*tmp_ptr, *base_ptr);

  /* Typical insertion sort, but we run from the `right-hand-side'
     downto the `left-hand-side.' */

  for (run_ptr = base_ptr + 1; run_ptr < end_ptr; run_ptr++)
    {
      char *temp = *(tmp_ptr = run_ptr + 1);

      /* Select the correct location for the new element, 
         by sliding everyone down by 1 to make room! */

      while (strcmp (temp, *--tmp_ptr) < 0)
        tmp_ptr[1] = *tmp_ptr;

      tmp_ptr[1] = temp; 
    }
}

/* Return the median value selected from among the 
   LOW, MIDDLE, and HIGH.  Rearrange LOW and HIGH so
   the three values are sorted amongst themselves. 
   This speeds up later work... */

inline static char *
find_pivot (char **low, char **high)
{
  char **middle = low + ((high - low ) >> 1);

  if (strcmp (*middle, *low) < 0)
    SWAP (*middle, *low);
  if (strcmp (*high, *middle) < 0)
    SWAP (*middle, *high);
  else 
    return *middle;

  if (strcmp (*middle, *low) < 0)
    SWAP (*middle, *low);

  return *middle;
}

/* Order elements using quicksort.  This implementation incorporates
   4 optimizations discussed in Sedgewick:
   
   1. Non-recursive, using an explicit stack of log (n) pointers that 
      indicate the next array partition to sort.

   2. Choses the pivot element to be the median-of-three, reducing
      the probability of selecting a bad pivot value.

   3. Only quicksorts TOTAL_ELEMS / MAX_THRESH partitions, leaving
      insertion sort to sort within the partitions.  This is a
      big win, since insertion sort is faster for small, mostly
      sorted array segements.
   
   4. The larger of the 2 sub-partitions are always pushed onto the
      stack first, with the algorithm then concentrating on the
      smaller partition.  This *guarantees* no more than log (n)
      stack size is needed! */
      
void
sort (char **base_ptr, int total_elems)
{
  if (total_elems > MAX_THRESH)
    {
      char       **lo = base_ptr;
      char       **hi = lo + (total_elems - 1);
      char       **left_ptr;
      char       **right_ptr;
      stack_node   stack[MAX_STACK_SIZE];
      stack_node  *top = stack + 1;

      while (STACK_NOT_EMPTY)
        {
          /* Select the median-of-three here. This allows us to
            skip forward for the LEFT_PTR and RIGHT_PTR. */
          char *pivot = find_pivot (lo, hi);
          left_ptr  = lo + 1;
          right_ptr = hi - 1; 

          /* Here's the famous ``collapse the walls'' section of
            quicksort.  Gotta like those tight inner loops! */
          do 
            {
              while (strcmp (*left_ptr, pivot) < 0)
                left_ptr++;

              while (strcmp (pivot, *right_ptr) < 0)
                right_ptr--;

              if (left_ptr < right_ptr) 
                {
                  SWAP (*left_ptr, *right_ptr);
                  left_ptr++;
                  right_ptr--;
                }
              else if (left_ptr == right_ptr) 
                {
                  left_ptr++;
                  right_ptr--;
                  break;
                }
            } 
          while (left_ptr <= right_ptr);

          if ((right_ptr - lo) <= MAX_THRESH) 
            {
              if ((hi - left_ptr) <= MAX_THRESH) /* ignore both small tables */
                POP (lo, hi);
              else              /* ignore small left table */
                lo = left_ptr;
            }
          else if ((hi - left_ptr) <= MAX_THRESH) /* ignore small right table */
            hi = right_ptr;
          else if ((right_ptr - lo) > (hi - left_ptr)) 
            {                   /* push larger left table */
              PUSH (lo, right_ptr);
              lo = left_ptr;
            }
          else 
            {                   /* push larger right table */
              PUSH (left_ptr, hi);
              hi = right_ptr;
            }
        }
    }

  insert_sort (base_ptr, base_ptr + (total_elems - 1));
}
