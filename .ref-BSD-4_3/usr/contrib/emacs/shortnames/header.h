/* Header file to remap longnames to shortnames.  Note that not
   all cpp's support longnames.  In particular, most pre 5.2 UniSoft
   ports only support 16 unique characters.  To bootstrap GNU emacs
   to a 5.0 UniPlus+ system I had to port the 5.2 cpp (with FLEXNAMES)
   to the development system.  This was about a 1 hour job with sources.
   Alternatively, m4 can probably be used but this is not as clean and
   requires some changes to the default make rules (add rule to go from
   a ".cm4" file to a ".c" file and rule to go from a ".hm4" file to
   a ".h" file.  There are too many changes for sed to handle in a single
   pass.    Fred Fish, UniSoft Systems. */
