/*   -*- buffer-read-only: t -*- vi: set ro:
 *
 *  DO NOT EDIT THIS FILE   (stdin.c)
 *
 *  It has been AutoGen-ed
 *  From the definitions    stdin
 *  and the template file   str2enum
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name ``Bruce Korb'' nor the name of any other
 *    contributor may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * str2enum IS PROVIDED BY Bruce Korb ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL Bruce Korb OR ANY OTHER CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "option-xat-attribute.h"
/* ANSI-C code produced by gperf version 3.0.4 */
/* Command-line: gperf option-xat-attribute.gp  */
/* Computed positions: -k'1' */



# if 0 /* gperf build options: */
// %struct-type
// %language=ANSI-C
// %includes
// %global-table
// %omit-struct-type
// %readonly-tables
// %compare-strncmp
//
// %define slot-name               xat_name
// %define hash-function-name      option_xat_attribute_hash
// %define lookup-function-name    find_option_xat_attribute_name
// %define word-array-name         option_xat_attribute_table
// %define initializer-suffix      ,XAT_COUNT_CMD
//
# endif

#include "option-xat-attribute.h"
typedef struct {
    char const *    xat_name;
    option_xat_attribute_enum_t xat_id;
} option_xat_attribute_map_t;
#include <string.h>

/* maximum key range = 6, duplicates = 0 */

static unsigned int
option_xat_attribute_hash (register const char *str, register unsigned int len)
{
  static const unsigned char asso_values[] =
    {
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10, 0,
     10,10,10,10,10,10,10, 5,10, 0,
     10,10,10,10,10,10, 0, 0,10, 0,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10,10,10,10,10,
     10,10,10,10,10,10
    };
  return len + asso_values[(unsigned char)str[0]];
}

static const option_xat_attribute_map_t option_xat_attribute_table[] =
  {
    {"",XAT_COUNT_CMD}, {"",XAT_COUNT_CMD},
    {"",XAT_COUNT_CMD}, {"",XAT_COUNT_CMD},
    {"type",     XAT_CMD_TYPE},
    {"words",    XAT_CMD_WORDS},
    {"cooked",   XAT_CMD_COOKED},
    {"members",  XAT_CMD_MEMBERS},
    {"uncooked", XAT_CMD_UNCOOKED},
    {"keep",     XAT_CMD_KEEP}
  };

static inline const option_xat_attribute_map_t *
find_option_xat_attribute_name (register const char *str, register unsigned int len)
{
  if (len <= 8 && len >= 4)
    {
      register int key = (int)option_xat_attribute_hash (str, len);

      if (key <= 9 && key >= 0)
        {
          register const char *s = option_xat_attribute_table[key].xat_name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1) && s[len] == '\0')
            return &option_xat_attribute_table[key];
        }
    }
  return 0;
}

/**
 * Convert a command (keyword) to a option_xat_attribute_enum_t enumeration value.
 *
 * @param[in] str   a string that should start with a known key word.
 * @param[in] len   the provided length of the keyword at \a str.
 * @returns the enumeration value.
 * If not found, that value is XAT_INVALID_CMD.
 */
option_xat_attribute_enum_t
find_option_xat_attribute_cmd(char const * str, size_t len)
{
    option_xat_attribute_map_t const * map;

    map = find_option_xat_attribute_name(str, (unsigned int)len);
    return (map == NULL) ? XAT_INVALID_CMD : map->xat_id;
}

/* end of option-xat-attribute.c */
