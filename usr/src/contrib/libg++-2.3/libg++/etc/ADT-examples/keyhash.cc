// Date: Tue, 20 Sep 88 01:03:21 -0700
// From: "Douglas C. Schmidt" <schmidt%crimee.ics.uci.edu@PARIS.ICS.UCI.EDU>
// 
// 
// The following is an Obstack-based program, which outputs all the GCC
// reserved words in an input file redirected from cin 
// In addition, I found a neat use for
// derived types by defining a word-by-word input class that is based
// upon the class Obstack.  Finally, there is the added bonus of seeing
// how the O (1) time GCC perfect hash function recognizer from GCC 1.35
// works; I've incorporated the relevant code in this short routine.
// Feel free to fold, spindle, or mutilate this code.
// 

#include <stream.h>
#include <ctype.h>
#include <Obstack.h>

#define NIL(TYPE) ((TYPE *)0)

class Word_Read : private Obstack
{
public:
  Obstack::free; // Allow the derived type to inherit only this member function.

  // Make the default something reasonable, like 80 columns.
  Word_Read (int Default_Len = 80): Obstack(Default_Len)
    {
      ;
    }

  // A very rudimentary method for carving out the next word
  // from the input.  Ignores most error conditions.  All non-
  // words are skipped entirely.  A word is defined as a
  // letter, followed by a sequence of letters, digits, and/or
  // underbars `_'.

  char *operator ()(int& Len = 0)
    {
      char c;
      
      while (cin.get (c) && !(isalpha (c) || c == '_'))
        ;
      
      do
        Obstack::grow (c);
      while (cin.get (c) && (isalnum (c) || c == '_'));

      Len = Obstack::size ();
      return cin.good () ? (char *)Obstack::finish (0) : 0;
    }
  
  // Make the destructor print out something useful, like
  // output a diagnostic.

  ~Word_Read ()
    {
      cout << "chunk_size = " << Obstack::chunk_size () << "\n";
      cout << "size = " << Obstack::size () << "\n";
      cout << "room = " << Obstack::room () << "\n";
    }
};

// Provides a nice example of the perfect hash function used to recognize
// GCC reserved words in O(1) steps.

class Lookup_Table
{
private:
  const int    MIN_WORD_SIZE = 2;
  const int    MAX_WORD_SIZE = 12;
  const int    MIN_KEY_SIZE  = 4;
  const int    MAX_KEY_SIZE  = 64;
  
  static int Hash_Table[];

  static char *Reswords[];

  // And here it is....  Very simple, guaranteed to work!

  int Hash (char *Str,int Len)
    {
      switch (Len) 
        {
        default:
        case 3:
          Len += Hash_Table[Str[2]];
        case 2:
        case 1:
          return Len + Hash_Table[Str[0]];
        }
    }
  
public:
  
  // Carries out the O (1) lookup for GCC reserved words!
  int  operator () (char *Str,int Len)
    {      
      if (Len <= MAX_WORD_SIZE && Len >= MIN_WORD_SIZE)
        {
          register int Key = Hash (Str,Len);
          
          if (Key >= MIN_KEY_SIZE && Key <= MAX_KEY_SIZE) 
            if (*Reswords[Key] == *Str && !strcmp (Reswords[Key] + 1,Str + 1)) 
              return 1;
        }
      return 0;   
    }
};
int  Lookup_Table::Hash_Table[] = 
    {
      64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
      64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
      64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
      64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
      64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
      64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
      64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
      64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
      64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
      64,  64,  64,  64,  64,   1,  64,   9,  17,  15,
      28,  19,  29,  15,  64,   2,  64,  64,  25,   4,
      16,  22,  40,  64,  11,  23,   1,   1,  16,   2,
      64,  64,   8,  64,  64,  64,  64,  64,
};

char * Lookup_Table::Reswords[] =
{
      "", "", "", "", "if", "", "int", "", "union", "while", "__typeof", 
      "__inline", "__typeof__", "__inline__", "auto", "__asm", "asm", "__asm__",
      "return", "__alignof", "goto", "__alignof__", "void", "__const",
      "enum", "__const__", "extern", "__volatile", "char", "__volatile__",
      "do", "switch", "unsigned", "inline", "register", "double", "const",
      "sizeof", "static", "continue", "struct", "break", "case", "for",
      "signed", "long", "else", "typeof", "typedef", "volatile", "short",
      "", "", "", "", "", "float", "", "", "", "", "", "", "", "default",
};


static void Store_Buf (char *Buf)
{ 
  cout << "Storing reserved word " << Buf << "\n";
  //  Just kidding!  But you get the idea....
}

main (int argc, char *argv[])
{
  Word_Read    Get_Next_Word;
  Lookup_Table Is_Reserved_Word;
  char *Buf;
  int   Len;
  
  while (Buf = Get_Next_Word (Len)) 
    if (Is_Reserved_Word (Buf,Len)) 
      Store_Buf (Buf);          // Keep reserved words
    else                        
      Get_Next_Word.free (Buf); // Discard non-reserved words
  
  return 0;
}


