// Generates random character strings
#include <stream.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

main (int argc, char *argv[]) 
{
  if (argc != 3) 
    {
      cout << "usage: " << argv [0] << " number-of-keys length-of-keys\n";
      return 1;
    }
  else
    {
      int Key_Number = atoi (argv [1]);     
      int Key_Length = atoi (argv [2]);

      srand (getpid());

      for (int j = 0; j < Key_Number; j++)
        {
          for (int i = 0; i < Key_Length - (rand () % 20); i++) 
            cout << char ('!' + rand () % (1+'~'-'!'));
          cout << "\n";
        }

      return 0;
    }

}
