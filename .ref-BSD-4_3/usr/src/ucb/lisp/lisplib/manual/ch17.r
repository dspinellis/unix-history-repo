






                        CHAPTER  17


                        Hash Tables






   1.1.  Overview

           A hash table is an object  that  can  efficiently
      map  a  given object to another.  Each hash table is a
      collection of entries,  each  of  which  associates  a
      unique  _k_e_y  with  a _v_a_l_u_e.  There are elemental func-
      tions to add, delete, and find entries based on a par-
      ticular key.  Finding a value in a hash table is rela-
      tively fast compared to  looking  up  values  in,  for
      example, an assoc list or property list.

           Adding a key to a hash table  modifies  the  hash
      table, and so it is a descructive operation.

           There are two different  kinds  of  hash  tables:
      those that use the function _e_q_u_a_l for the comparing of
      keys, and those that use _e_q, the default.  If a key is
      "eq"  to  another  object,  then  a  match is assumed.
      Likewise with "equal".



   1.2.  Functions

(makeht 'x_size ['s_test])

     RETURNS: A hash  table  of  x_size  hash  buckets.   If
              present, s_test is used as the test to compare
              keys in the hash table, the default being  eq.
              _E_q_u_a_l  might  be  used  to create a hash table
              where the keys are to be lists  (or  any  lisp
              object).

     NOTE: At this time, hash tables are implemented on  top
           of vectors.







9

9Hash Tables                                             17-1







Hash Tables                                             17-2


(hash-table-p 'H_arg)

     RETURNS: t if H_arg is a hash table.

     NOTE: Since hash tables are really  vectors,  the  lisp
           type of a hash table is a vector, so that given a
           vector, this function will return t.

(gethash 'g_key 'H_htable ['g_default])

     RETURNS: the value associated the  key  g_key  in  hash
              table  H_htable.   If  there  is  not an entry
              given by the key and g_default  is  specified,
              then  g_default is returned, otherwise, a sym-
              bol that is unbound is returned.  This  is  so
              that nil can be a associated with a key.

     NOTE: _s_e_t_f may be used to set the value assocaited with
           a key.

(remhash 'g_key 'H_htable)

     RETURNS: t if there was an entry for g_key in the  hash
              table H_htable, nil otherwise.  In the case of
              a match, the entry and associated  object  are
              removed from the hash table.

(maphash 'u_func 'H_htable)

     RETURNS: nil.

     NOTE: The function u_func is applied to  every  element
           in  the  hash  table  H_htable.   The function is
           called with two arguments: the key and  value  of
           an  element.   The mapped function should not add
           or delete  object  from  the  table  because  the
           results would be unpredicable.

(clrhash 'H_htable)

     RETURNS: the hash table cleared of all entries.











9

9                                       Printed: May 22, 1985







Hash Tables                                             17-3


(hash-table-count 'H_htable)

     RETURNS: the number of entries in  H_htable.   Given  a
              new  hash table with no entries, this function
              returns zero.















































9

9                                       Printed: May 22, 1985







Hash Tables                                             17-4



    ____________________________________________________

    ; make a vanilla hash table using "eq" to compare items...
    -> (setq black-box (makeht 20))
    hash-table[26]
    -> (hash-table-p black-box)
    t
    -> (hash-table-count black-box)
    0
    -> (setf (gethash 'key black-box) '(the value associated with the key))
    key
    -> (gethash 'key black-box)
    (the value associated with the key)
    -> (hash-table-count black-box)
    1
    -> (addhash 'composer black-box 'franz)
    composer
    -> (gethash 'composer black-box)
    franz
    -> (maphash '(lambda (key val) (msg "key " key " value " val N)) black-box)
    key composer value franz
    key key value (the value associated with the key)
    nil
    -> (clrhash black-box)
    hash-table[26]
    -> (hash-table-count black-box)
    0
    -> (maphash '(lambda (key val) (msg "key " key " value " val N)) black-box)
    nil

    ; here is an example using "equal" as the comparator
    -> (setq ht (makeht 10 'equal))
    hash-table[16]
    -> (setf (gethash '(this is a key) ht) '(and this is the value))
    (this is a key)
    -> (gethash '(this is a key) ht)
    (and this is the value)
    ; the reader makes a new list each time you type it...
    -> (setq x '(this is a key))
    (this is a key)
    -> (setq y '(this is a key))
    (this is a key)
    ; so these two lists are really different lists that compare "equal"
    ; not "eq"
    -> (eq x y)
    nil
    ; but since we are using "equal" to compare keys, we are OK...
    -> (gethash x ht)
    (and this is the value)
    -> (gethash y ht)
    (and this is the value)
    ____________________________________________________


                                       Printed: May 22, 1985







Hash Tables                                             17-5






















































9

9                                       Printed: May 22, 1985



