// From: "Douglas C. Schmidt" <schmidt@blanche.ics.uci.edu>
// Date: Sat, 29 Oct 88 14:11:51 -0700

#include <stream.h>
#include <std.h>
#include <builtin.h>

/**********************************************************************/
/**********************************************************************/

typedef int ITEM_TYPE;

class Additive_Search
{
  
  // Implements the binary search variation described on pages 138 and 139
  // of Tim Standish's ``Data Structure Techniques'' textbook.  The big
  // win here is that this version uses no division (or right shift),
  // only addition, so it runs faster on most computers.
  
private:
  
  ITEM_TYPE        *Vector_Buffer; // Hold's copy of the initialized search structure
  int               Vector_Length; // Length of the user's input
  static int        Tree_Height;   // Height of the implicit binary search tree
  static ITEM_TYPE *Temp;          // Temporary storage during initialization
  
  void Fill_Buffer (int h, int i); // Transforms sorted array into special
  // representation that supports the
  // additive binary search technique
public:
  
  Additive_Search  (ITEM_TYPE *Array, int Len);
  int  Is_Member   (ITEM_TYPE K);
  ~Additive_Search (void);
  
};

int        Additive_Search::Tree_Height = 0;
ITEM_TYPE* Additive_Search::Temp = 0; 

/**********************************************************************/

void Additive_Search::Fill_Buffer (int Hgt, int Index) 
{
  // Uses a very concise recursive routine to initialize the Vector_Buffer from
  // the original sorted array (which must have been sorted, or else this
  // *won't* work)!  This magic sequence of statements transforms the original sorted
  // array into an new array representing the level order traversal of a complete
  // binary search tree.  See Standish, page 138 for details...
  
  if ((Hgt <= Tree_Height) && (Index < Vector_Length))
    {
      Fill_Buffer (Hgt + 1, (Index + Index) + 1);
      Vector_Buffer [Index] = *Temp++;
      Fill_Buffer (Hgt + 1, (Index + Index) + 2);
    }

}

/**********************************************************************/

Additive_Search::Additive_Search (ITEM_TYPE *Array, int Len)
{
  Tree_Height = lg (Len);
  Vector_Length = Len;
  Temp = Array;
  Vector_Buffer = new ITEM_TYPE [Len];
  Fill_Buffer (0, 0);             // Tree_Height and Index both start off at 0
}

/**********************************************************************/

int  
Additive_Search::Is_Member (ITEM_TYPE K) 
{
  // Performs the additive binary search upon the initialized Vector_Buffer.
  // Note that we perform no division or multiplication in this search.
  // Such omissions generally yield faster running times on most machines.
  
  register int i = 0;
  
  while (i < Vector_Length)
    {
      register int Cmp = (Vector_Buffer [i] - K);
    
      if (Cmp == 0) 
        return 1;
      else
        {
          i += i + 1;
          if (Cmp < 0) 
            i++;
        }
    }
  
  return 0;
}

/**********************************************************************/

Additive_Search::~Additive_Search (void) 
{
  delete Vector_Buffer;
}

/**********************************************************************/
/**********************************************************************/

class Binary_Search 
{
  // Performs the typical binary search algorithm.  For comparison purposes only.
#define COPY(DEST,SRC,NB) {register typeof(DEST) a = (DEST), b = (SRC);\
register int i, j = (NB);\
for (i = (j & 07)+1; --i;) *a++ = *b++;\
  for (i = (j>>3)+1; --i>0;) {\
*a++ = *b++; *a++ = *b++; *a++ = *b++; *a++ = *b++;\
*a++ = *b++; *a++ = *b++; *a++ = *b++; *a++ = *b++;\
}}

private:
  ITEM_TYPE *Vector_Buffer;
  int        Vector_Length;
  
public:
  
  Binary_Search (ITEM_TYPE *Array, int Len): Vector_Length (Len)
    {
      Vector_Buffer = new ITEM_TYPE [Len];
      
      COPY (Vector_Buffer, Array, Len); // The infamous Duff's Device!
    }
  
  int  Is_Member (ITEM_TYPE K) 
    {                           // Yawn, this looks familiar
      register int hi;
      register int lo;
      
      for (lo = 0, hi = Vector_Length - 1; lo <= hi ;) 
        {
          register int mid = (lo + hi) >> 1;
          
          if (Vector_Buffer [mid] == K) 
            return 1;
          else if (Vector_Buffer [mid] < K) 
            lo = mid + 1;
          else 
            hi = mid - 1;
        }
      
      return 0;
    }
  
  ~Binary_Search (void) 
    {
      delete Vector_Buffer;
    }   
};

/**********************************************************************/

static int  Cmp (void *A, void *B) 
{ // Too bad we lack lambdas!
  return (*(ITEM_TYPE*)A < *(ITEM_TYPE*)B)? 
          -1 : 
         ((*(ITEM_TYPE*)A == *(ITEM_TYPE*)B) ? 0 : 1);
}

/**********************************************************************/

double return_elapsed_time (double);
double start_timer (void);
void   Gen_Rand (ITEM_TYPE Buf[], int Size);

int main (int, char *argv[]) 
{
  int        Size = atoi (argv [1]);
  ITEM_TYPE  Buf [Size];
  
  Gen_Rand (Buf, Size);
  qsort ((void *) Buf, Size, sizeof (ITEM_TYPE), Cmp);
  
  Additive_Search Foo (Buf, Size); 
  Binary_Search   Bar (Buf, Size);
  
  start_timer ();
  for (int i = 0; i < Size ; i++) 
    if (! Bar.Is_Member (Buf [i]))
      cout << "bad news, buf [" << i << "] = " << Buf [i] << "!\n";

  double Elapsed_Time = return_elapsed_time (0.0);
  cout << "Binary Time = " << Elapsed_Time << "\n";
  
  start_timer ();
  for (i = 0; i < Size ; i++) 
    if (! Foo.Is_Member (Buf [i])) 
      cout << "bad news, buf [" << i << "] = " << Buf [i] << "!\n";

  Elapsed_Time = return_elapsed_time (0.0);
  cout << "Additive Time = " << Elapsed_Time << "\n";
  
  return 0;
}

void Gen_Rand (ITEM_TYPE Buf[], int Size) 
{ // Generates some random numbers
  srand (getpid ());
  
  for (int i = 0; i < Size; i++) 
    Buf [i] = ITEM_TYPE (rand ());
  
}

