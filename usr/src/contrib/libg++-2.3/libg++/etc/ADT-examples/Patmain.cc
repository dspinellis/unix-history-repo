// Tests the Patricia tree

#include <stream.h>
#include <stdio.h>
#include "Patricia.h"

double start_timer (void);
double return_elapsed_time (double);

const int MAX_KEY_LEN = 1000;

main (int argc, char *argv[])
{
  if (argc != 3)
    {
      cerr << "usage: " << argv [0] << " file1 file2\n";
      return 1;
    }
  else
    {
      if (!freopen (argv [1], "r", stdin))
        {
          perror (argv [0]);
          return 1;
        }

      Patricia_Trie trie;
      char key [MAX_KEY_LEN];

      while (gets (key)) 
        trie.insert (key, 0);

      fclose (stdin);
      if (! freopen (argv [2], "r", stdin))
        {
          perror (argv [0]);
          return 1;
        }

      start_timer ();

      while (gets (key))
        {
          Trie_Record *t = trie.find (key);
          cout << key << ": " << (! strcmp (key, t->get_key ()) ? "is found!\n" : "is not found!\n");
        }
      
      double Elapsed_Time = return_elapsed_time (0.0);
      cout << "Time = " << Elapsed_Time << "\n";
      fclose (stdin);
      return 0;
    }
  
}
