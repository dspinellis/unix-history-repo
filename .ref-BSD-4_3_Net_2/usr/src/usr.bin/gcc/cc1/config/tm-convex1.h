/* tm.h file for a Convex C1.  */

#include "tm-convex.h"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT 1

#define CC1_SPEC "%{mc2:-mnoc1}"

/* Include Posix prototypes unless -traditional. */

#define CPP_SPEC \
"%{mc2:-D__convex_c2__}%{!mc2:-D__convex_c1__}\
 -D__NO_INLINE_MATH\
 %{!traditional:-D__stdc__ -D_POSIX_SOURCE -D_CONVEX_SOURCE}"

/* Search Posix or else backward-compatible libraries depending
   on -traditional. */

#define LIB_SPEC \
"%{mc2:-lC2}%{!mc2:-lC1}\
 %{!p:%{!pg:%{traditional:-lc_old}%{!traditional:-lc}}}\
%{p:%{traditional:-lc_old_p}%{!traditional:-lc_p}}\
%{pg:%{traditional:-lc_old_p}%{!traditional:-lc_p}}"
