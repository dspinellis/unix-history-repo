/* Implements the PATRICIA Trie ADT.  PATRICIA stands for ``Practical
   Algorithm to Retrieve Information Coded in Alphanumeric.''  PATRICIA
   was developed by D.R. Morrison.  The current implementation is
   based upon Robert Sedgewick's description in his Algorithms book.
   
   Includes operations for
 * Initialization
 * Insertion
 * Retrieval */

/* This could change, depending on what client wants */
typedef void *CONTENTS; 

/* Record structure that *must* be visible to clients. */ 

class Trie_Record
{
private:
  char    *key;           /* Only works safely and easily for char *'s */
  CONTENTS contents;      /* Pointer to record Contents referenced by Key */

public:
  char    *get_key (void) { return key; }
  CONTENTS get_contents (void) { return contents; }
  void     set_key (char *k) { key = k; }
  void     set_contents (CONTENTS c) { contents = c; }
};

/* We globalized the class because this is done by g++ 1.40 anyway and with 
 * 1.9? this does not work at all (one has to prefix all members of the nested
 * class with the name of the enclosing class.H.S.
 */
  /* Nested class definition, should be invisible to clients with new C++ 
     scoping rules... */

  struct Trie_Node 
    {                      
      Trie_Record trie_rec;      /* Stores data in the Trie */
      int         branch_bit;    /* Stores index of next Bit to compare */
      Trie_Node  *left;          /* Pointer to left child. */
      Trie_Node  *right;         /* Pointer to right child. */
      
      Trie_Node (char *k = "", int len = 0, CONTENTS new_contents = 0, int b = 0);
    };

class Patricia_Trie 
{
private:
  const int HI_WORD = 7;         /* Hi-order bit, starting count from 0 */
  const int BIT_MASK = 07;       /* WORD_SHIFT low-order bits enabled */
  const int WORD_BITS = 8;       /* Number of Bits in a Block */
  const int WORD_SHIFT = 3;      /* i.e. lg (WORD_BITS) */

  /* Root for the entire Patricia tree, (actually a dummy header!). */
  Trie_Node *root;
  
  /* Recursively free tree memory via modified post-order traversal. */
  void dispose_trie (Trie_Node *root);

  /* Normalizes the ith Bit representation */
  inline int get_bit (int i) { return HI_WORD - i; }

  /* Returns 1 if bit is set. */
  inline int is_bit_set (int block, int shift) { return (block & (1 << shift)) != 0; }

  /* Return the ith power of 2. */
  inline int POW2 (int i) { return 1 << i; }
  
public:
  Patricia_Trie (void);
 ~Patricia_Trie (void);
  Trie_Record *find (char *key);
  void insert (char *key, CONTENTS contents, int key_len = 0);
};
