//From: "Douglas C. Schmidt" <schmidt@zola.ICS.UCI.EDU>
//Date: Fri, 28 Jul 89 11:47:11 -0700

/* Nifty little program that illustrates an implementation of the Knuth, 
   Morris, Pratt string matching algorithm.

   This program has a user interface similar to fgrep, i.e., when
   you provide it with a fixed pattern it prints out all the lines
   from the standard input (or one user-specified input file) that 
   contain at least one substring that matches the provided pattern. 
   
   Relevant options are:
   
   -i: Ignore case when performing comparisons. 
   -n: Print out lines numbers along with the matching lines.
   -v: Print out lines that *don't* match the pattern.

   The code below is extensively commented.  If these comments
   distract you, run the code through g++ -E first ;-). */
   
#include <stdio.h>
#include <std.h>
#include <GetOpt.h>

/* This array is designed for mapping upper and lower case letter
   together for a case independent comparison.  The mappings are
   based upon the ASCII character sequences. */
char charmap[] = 
{
	'\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
	'\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
	'\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
	'\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
	'\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
	'\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
	'\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
	'\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
	'\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
	'\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
	'\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
	'\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
	'\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
	'\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
	'\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
	'\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
	'\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
	'\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
	'\300', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
	'\370', '\371', '\372', '\333', '\334', '\335', '\336', '\337',
	'\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
	'\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};

/* The list of available program options. */
enum options
{
  DEBUG, FOLD_CASE, LINE_NUMBERS, INVERSE, OPTION_SIZE
};

/* Vector storing the enabled options (all initially disabled). */
char option[OPTION_SIZE];

/* Data and function members necessary to implement the KMP algorithm. */
class kmp
{
private: 

#ifdef GNUG
  const int RESET_PATTERN_FLAG = -1;
#else
#define RESET_PATTERN_FLAG -1
#endif  

  int  *fail_trans_table; /* Stores the generated nfa used when matching. */
  char *pattern;          /* Stores the user-supplied pattern. */
  int   pattern_len;      /* Pattern length. */

  void print_fail_trans_table (void);

public:
  kmp (char *pattern, int pattern_len, int debug = 0);
 ~kmp () { delete fail_trans_table; }
  int operator ()(char *text, int text_len);
};

/* Provide a debugging dump of the failure function.  Nothing fancy... */

void
kmp::print_fail_trans_table (void)
{
  int i;
  
  for (i = 0; i < pattern_len; i++)
    printf ("%3d,", i);

  putchar ('\n');

  for (i = 0; i < pattern_len; i++)
    printf ("%3d,", fail_trans_table [i]);

  putchar ('\n');
}

/* This constructor builds the transition table that encodes the failure function 
   automaton.  The table stores the PATTERN index we go to in the event of a 
   mismatch with the input text string.  This generation process runs in time 
   linear to the pattern size. 
   
   The terms SUFFIX and PREFIX used below are defined in terms of each other.  
   That is, at each state of the failure function we seek to determine the largest 
   pattern prefix that matches with the largest suffix in the actual input text.  
   This information informs us how far we can legitimately shift the pattern to the 
   right when a mismatch occurs during the string matching process. 
   
   Stated more formally, this means that for all i (0 <= i <= (PATTERN_LEN - 1))
   FAIL_TRANS_TABLE (i) is the largest j < i, such that PATTERN[1..j - 1] 
   is a suffix of PATTERN[1..i - 1] and PATTERN[i] != PATTERN[j]. */

kmp::kmp (char *pat, int len, int debug):
  pattern (pat), fail_trans_table (new int[len]), pattern_len (len)
{
  /* Points 1 beyond the rightmost potential *suffix* in the pattern (which is 
     actually simulating the behavior of an actual input text string. */
  int suffix_end = 0;

  /* Points 1 beyond the rightmost potential *prefix* in the pattern. */
  int prefix_end = fail_trans_table[suffix_end] = RESET_PATTERN_FLAG;
  
  do
    {
      /* This WHILE loop uses the precomputed failure function to look further 
         left into the pattern trying to find an index location where the pattern 
         prefix matches the pattern suffix.  However, if/when we locate the
         RESET_PATTERN_FLAG this means that we can just skip ahead to the next 
         character in the input text. */
         
      while (prefix_end != RESET_PATTERN_FLAG 
             && pattern[suffix_end] != pattern[prefix_end])
        prefix_end = fail_trans_table[prefix_end];

      /* Once SUFFIX_END and PREFIX_END are pre-incremented below we know that 
         the first PREFIX_END characters of PATTERN match the characters in 
         positions PATTERN[SUFFIX_END - PREFIX_END .. SUFFIX_END - 1], i.e.,
         the last PREFIX_END characters in the rightmost part of the first 
         SUFFIX_END - 1 characters in PATTERN.
        
         If the character at location PATTERN[SUFFIX_END] matches that at 
         PATTERN[PREFIX_END] it is silly to have the failure transition
         jump to that pattern location (since it would immediately fail to
         match, of course!). Instead, in that case we just ``borrow'' the
         previously computed transition stored at FAIL_TRANS_TABLE[PREFIX_END]
         and use it. */

      if (pattern[++suffix_end] == pattern[++prefix_end])
        fail_trans_table[suffix_end] = fail_trans_table[prefix_end];
      else
        fail_trans_table[suffix_end] = prefix_end;
    }
  /* Adding the extra 1 here is necessary since C strings are
     indexed from 0 to pattern_len - 1... */

  while (suffix_end + 1 < pattern_len);

  if (debug)
    print_fail_trans_table ();
}

/* Actually perform the KMP matching algorithm using the generated determinisitic 
   pattern matching match encoded in the failure transition table.  This version 
   is optimized on the assumption that there will be more characters in the text 
   input that *don't* match the pattern than ones that do match it. Therefore, we 
   make a special effort to keep looping through the failure function as long as 
   the text and pattern don't match. */

int
kmp::operator ()(char *text, int text_len)
{
  int suffix_end = RESET_PATTERN_FLAG;
  int prefix_end = RESET_PATTERN_FLAG;

  /* If TEXT length is shorted than PATTERN we'll bail out now... */
  if (text_len < pattern_len)
    return 0;
    
  /* Split the following two cases so that we don't pay any
     unnecessary overhead when case folding is not required. */
  if (option[FOLD_CASE])
  
  /* Note how this main loop is almost identical with the 
     `failure transition table building' algorithm used above. */

    do
      {
        /* Ignore case when matching and keep applying the failure function
           until we get a match or are forced to restart the pattern (starting
           with the *following* input text character, that is). */

        while (prefix_end != RESET_PATTERN_FLAG 
               && charmap[text[suffix_end]] != charmap[pattern[prefix_end]])
          prefix_end = fail_trans_table[prefix_end];

        if (prefix_end + 1 >= pattern_len)
          return 1;
        else
          ++suffix_end, ++prefix_end;
      }

    /* This conditional expression is used to terminate the search when it
       becomes clear that we can't match the PATTERN since the TEXT has
       fewer unexamined characters than the PATTERN length. */
    while (text_len - suffix_end >= pattern_len - prefix_end);

  else

    /* This loop is identical with the preceding one, except that we don't
       bother to fold case... */

    do
      {
        while (prefix_end != RESET_PATTERN_FLAG 
               && text[suffix_end] != pattern[prefix_end])
          prefix_end = fail_trans_table[prefix_end];

        if (prefix_end + 1 >= pattern_len)
          return 1;
        else
          ++suffix_end, ++prefix_end;
      }
    while (text_len - suffix_end >= pattern_len - prefix_end);

  return 0;
}

/* The name sez it all! */

void
print_usage_and_die (char *prog_name)
{
  fprintf (stderr, "usage: %s [-inv] pattern [file]\n", prog_name);
  exit (1);
}

/* Main driver program.  Emulates certain useful features of fgrep. */
   
int
main (int argc, char **argv)
{
  GetOpt getopt(argc, argv, "dinv");

  if (argc == 1)
    print_usage_and_die (argv[0]);

  /* A rather arbitrary limit... */
  const int MAX_LINE = 200;
  char  text[MAX_LINE];
  int   c;
      
  /* Initialize any user-specified options. */

  while ((c = getopt ()) != EOF)
    switch (c)
      {
      case 'd': option[DEBUG] = 1; break;
      case 'i': option[FOLD_CASE] = 1; break;
      case 'n': option[LINE_NUMBERS] = 1; break;
      case 'v': option[INVERSE] = 1; break;
      default:  print_usage_and_die (argv[0]);
      }
      
  /* Call the constructor to build the failure function table. */
  kmp match (argv[getopt.optind], strlen (argv[getopt.optind]), option[DEBUG]);

  /* Handle a user-specified input file. */
  if (argv[++getopt.optind] && !freopen (argv[getopt.optind], "r", stdin))
    {
      perror (argv[0]); return 1;
    }
  
  /* Split the following into two separate cases to avoid overhead 
     when line numbers are not required. */
  if (option[LINE_NUMBERS])
    {
      int line;
      
      for (line = 1; gets (text); line++)
        if (match (text, strlen (text)) != option[INVERSE])
          printf ("%d:%s\n", line, text);

    }
  else
    {

      while (gets (text))
        if (match (text, strlen (text)) != option[INVERSE])
          puts (text);

    }
  return 0;
}

