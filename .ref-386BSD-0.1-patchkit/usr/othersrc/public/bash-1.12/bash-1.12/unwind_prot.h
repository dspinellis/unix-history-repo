/* unwind_prot.h - Macros and functions for hacking unwind protection. */

#if !defined (_UNWIND_PROT_H)
#define _UNWIND_PROT_H

/* Run a function without interrupts. */
void
  without_interrupts (), begin_unwind_frame (), discard_unwind_frame (),
  run_unwind_frame (), add_unwind_protect (), remove_unwind_protect (),
  run_unwind_protects ();

/* Define for people who like their code to look a certain way. */
#define end_unwind_frame()

/* How to protect an integer. */
#define unwind_protect_int(X) unwind_protect_var (&(X), (X), sizeof (int))

/* How to protect a pointer to a string. */
#define unwind_protect_string(X) \
  unwind_protect_var (&(X), (X), sizeof (char *))

/* How to protect any old pointer. */
#define unwind_protect_pointer(X) unwind_protect_string (X)

/* How to protect the contents of a jmp_buf. */
#define unwind_protect_jmp_buf(X) \
  unwind_protect_var ((int *)(X), (char *)(X), sizeof (jmp_buf))

#endif /* _UNWIND_PROT_H */

