/* Implements the PATRICIA trie abstraction. */

#include <std.h>
#include "Patricia.h"

Trie_Node::Trie_Node (char *k, int len, CONTENTS new_contents, int b): 
     branch_bit (b)
{
  trie_rec.set_contents (new_contents);

  /* Dynamically compute the length, if it's not given */
  if (!len)             
    len = strlen (k) + 1;
  trie_rec.set_key (strcpy (new char[len], k));
}

/* Recursively free tree memory via modified post-order traversal */
void 
Patricia_Trie::dispose_trie (Trie_Node *root)
{
  if (root->left->branch_bit <= root->branch_bit &&
      root->right->branch_bit <= root->branch_bit)
    ;                           /* do nothing! */
  else if (root->left->branch_bit <= root->branch_bit) 
    dispose_trie (root->right);
  else if (root->right->branch_bit <= root->branch_bit) 
    dispose_trie (root->left);
  else
    {
      dispose_trie (root->left);
      dispose_trie (root->right);
    }
  delete root;
}

/* Initializes Patricia_Trie, using dummy header. */

Patricia_Trie::Patricia_Trie (void)
{                           
  root        = new Trie_Node;
  root->left  = root;
  root->right = root;
}

/* Frees all dynamic memory in Patricia Trie */
Patricia_Trie::~Patricia_Trie (void)
{                           
  dispose_trie (root->left);
  delete root;
}

/* Attempts to locate the record associated with Key.  The basic
   algorithm is abstractly similar to Sedgewick's description in his
   Algorithm's book.  However, I've modified things to speed up the Bit
   comparisons greatly, as well as to run the Branch_Bit index from
   0..whatever, since this allows arbitrarily large keys (For some
   strange reason Sedgewick goes from some predefined limit,
   Max_Branch_Bit, *downto* 0!!).

   Most of the contortions below help manage a Bit cache, which
   reduces the total amount of work required to test the next Branch_Bit.
   Empirical tests revealed that this was main bottleneck in the Find
   routine.  To make the algorithm work I needed to have the first Bit in
   the first word always be 0.  Therefore, I've ordered the bits from
   high-order Bit to low-order Bit (e.g., 8 .. 1), running from word 0
   to word K.  This fits intuitively with how you might draw the bits onn
   paper, but *not* with how it would work if you programmed it in the
   normal way (i.e., running from bit 1 to 8, from word 0 to K).  At
   any rate, that's what the BIT macro is for, it normalizes my abstract
   concept of bit-order with the actual bit-order. */
     
Trie_Record *
Patricia_Trie::find (char *key)
{
  
  Trie_Node *parent;
  Trie_Node *cur   = root->left; /* Root is *always* a dummy node, skip it */
  char      *block = key;        /* Pointer to Current Block of Bits */
  int        lower = 0;
  
  do
    {
      parent  = cur;   
      int bit = cur->branch_bit - lower;
      
      /* Oh well, time to refill the Bit cache! 
         This loop gets executed infrequently, in general */
      if (bit >= WORD_BITS)
        
        while (lower + WORD_BITS <= cur->branch_bit)
          {
            lower += WORD_BITS;
            ++block;
            bit -= WORD_BITS;
          }                 
      
      cur = (is_bit_set (*block, get_bit (bit)) ? cur->right : cur->left);
    }
  while (parent->branch_bit < cur->branch_bit);
  
  return &cur->trie_rec;                   /* Let calling routine worry whether Keys matched! */
}

/* Inserts a new KEY and its associated CONTENTS into the trie. */

void 
Patricia_Trie::insert (char *key, CONTENTS contents, int key_len)
{
  Trie_Node *parent;
  Trie_Node *cur   = root;
  char      *block = key;
  int        lower = 0;
  
  /* This loop is essentially the same as in the Find routine. */

  do
    {                           
      parent  = cur;   
      int bit = cur->branch_bit - lower;
      if (bit >= WORD_BITS)
        while (lower + WORD_BITS <= cur->branch_bit)
          {
            lower += WORD_BITS;
            ++block;
            bit -= WORD_BITS;
          }
      cur = (is_bit_set (*block, get_bit (bit)) ? cur->right : cur->left);
    } 
  while (parent->branch_bit < cur->branch_bit);
  
  /* Exclude duplicates */
  if (strcmp (key, cur->trie_rec.get_key ()))
    {                           
      char *key_block = key;
      char *cur_block = cur->trie_rec.get_key ();
      
      /* Find the first word where Bits differ, skip common prefixes. */

      for (int first_bit_diff = 0;  
           *cur_block == *key_block;         
           first_bit_diff += WORD_BITS)
        cur_block++, key_block++;

      /* Now, find location of that Bit, xor does us a favor here. */

      for (int bit = *cur_block ^ *key_block; 
           !(bit & POW2 (HI_WORD));     
           bit <<= 1)
        first_bit_diff++;
      
      /* *Not* adding at a leaf node */
      if (parent->branch_bit > first_bit_diff)
        {                       
          
          /* This is almost identical to the original Find above, however, we are */
          /* guaranteed to end up at an internal node, rather than a leaf. */
          
          for (cur = root, lower = 0, cur_block = key; ;)
            {
              parent  = cur;   
              int bit = cur->branch_bit - lower;

              if (bit >= WORD_BITS)

                while (lower + WORD_BITS <= cur->branch_bit)
                  {
                    lower += WORD_BITS;
                    ++cur_block;
                    bit -= WORD_BITS;
                  }
              
              cur = (is_bit_set (*cur_block, get_bit (bit)) ? cur->right : cur->left);
              if (cur->branch_bit >= first_bit_diff) 
                break;
            }
        }
      
      Trie_Node *t = new Trie_Node (key, key_len, contents, first_bit_diff);
      
      /* Key_Block was saved from above, this avoids some costly recomputation. */
      if (is_bit_set (*key_block, get_bit (first_bit_diff & BIT_MASK)))
        t->left = cur, t->right = t;
      else
        t->left = t, t->right = cur;
      
      /* Determine whether the new node goes to the left or right of its parent */
      block = key + (parent->branch_bit >> WORD_SHIFT);

      (is_bit_set (*block, get_bit (parent->branch_bit & BIT_MASK))
       ? parent->right : parent->left) = t;
    }
}

