/* Date: Sun, 18 Sep 88 15:57:31 -0700 
   From: "Douglas C. Schmidt" <schmidt%crimee.ics.uci.edu@ORION.CF.UCI.EDU> 

   The following tool performs topological sorting in O(n + m) average-time
   (n == # of vertices, m == # of edges).  

   If you happen to have Andrew Koenig's Summer USENIX '88 article on
   Associative Arrays in C++ it is interesting to compare his solution to
   mine.  Basically, my version, which uses hashing as opposed to his AVL
   trees, should be *much* faster on the average (it would be interesting
   to compare the two).  In fact, if you test the code below against the
   standard UNIX tsort routine, you should be *amazed* at how much faster
   it is (several orders of magnitude on large input files)! */

#include <stream.h>
#include <std.h>
#include <builtin.h>
#include <String.h>

/* Forward class decls. */
class Vertex_Node;  
class Assoc_Array;
class Assoc_Iter;

/* Defines and implements the adjacency list abstraction that contains the list
   of vertices that are incident to given vertex.  Note that we store Vertex_Node
   pointers here, rather than vertex *Names*.  This allows us to find a particular
   vertex in the Hash_Table in O(1) time, without having to perform the usual
   hash lookup (compare this with Andrew Koenig's implementation from the Summer
   USENIX '88 proceedings!) */

class Adjacency_Node 
{
private:
  Vertex_Node    *item_ptr;
  Adjacency_Node *next_ptr;
  
public:
  
  /* The constructor simply orders the adjacency list as a stack. */
  Adjacency_Node (Vertex_Node *new_item, Adjacency_Node *head):
       item_ptr (new_item), next_ptr (head) {}
  
  Vertex_Node *item (void) 
    {
      return item_ptr;
    }
  
  Adjacency_Node *next (void) 
    {
      return next_ptr;
    }
};

/* The Hash_Table is composed of these Vertex_Nodes.  Each Vertex_Node contains
   an adjacency list of successor nodes, and an In_Degree count.  Each operation
   is extremely concise and self-explanatory, with the exception of the overloaded
   ``+='' operator, which essentially means ``Add the successor node to the successor
   list (OK, so this is an abuse of overloading!) */

class Vertex_Node 
{
friend class Assoc_Array;
friend class Assoc_Iter;

private:
  int             in_degree;
  String          name;
  Adjacency_Node *succ_list;
  Vertex_Node    *next;
  
public:
  Vertex_Node (String &item, Vertex_Node *n = 0):
       in_degree (0), name (item), next (n), succ_list (0) {}
  
  int in_degree_count (void) 
    {
      return in_degree;
    }
  
  Adjacency_Node *get_succ_list (void) 
    {
      return succ_list;
    }
  
  Vertex_Node& operator++ (void)
    {
      in_degree++;
      return *this;
    }
  
  int operator-- (void)
    {
      return --in_degree;
    }
  
  Vertex_Node& operator+= (Vertex_Node& succ_node)
    {
      succ_list = new Adjacency_Node (&succ_node,succ_list);
      return *this;
    }
  
  String &item (void) 
    {
      return name;
    }
  
  friend ostream& operator << (ostream& stream,Vertex_Node& output)
    {
      return stream << output.in_degree;
    }
  
};

/* Here's the main data structure.  It is simply a Hash_Table of Vertex_Nodes.
   I make use of hashpjw from the libg++ builtin library.  Nothing too tricky
   here....  I overload the [] operator to provide the classic ``associative
   array'' touch. */

class Assoc_Array
{
friend class Vertex_Node *Assoc_Iter::operator ()(void);

private:
  int           total_size;
  int           current_size;
  Vertex_Node **hash_table;
  
public:
  Assoc_Array (int size): total_size (size), current_size (0)
    {
      hash_table = new Vertex_Node *[size];         
      bzero (hash_table, size * sizeof *hash_table);
    }
  
  Vertex_Node& operator[](String index)
    {
      int          location = hashpjw (index) % total_size;
      Vertex_Node *head     = hash_table[location];
      
      if (!head)
        {
          current_size++;
          return *(hash_table[location] = new Vertex_Node (index));
        }
      else 
        {
          
          for ( ; head; head = head->next)
            if (head->name == index)
              return *head;
          
          current_size++;
          return *(hash_table[location] = new Vertex_Node (index, hash_table[location]));
        }
    }
  
  int array_size (void)
    {
      return current_size;
    }
  
  int max_size (void)
    {
      return total_size;
    }
};

/* This is a neat class, which implements an iterator for the Hash_Table used
   for the associative array.  It would be faster to search the table directly,
   without using an iterator, but this is a neat feature of C++ that fits too
   nicely into the overall abstraction to pass up! */

class Assoc_Iter
{
private:
  Vertex_Node *curr;
  Assoc_Array *assoc;
  int          curr_table_pos;
  
public:
  Assoc_Iter (Assoc_Array& array,int items): 
       assoc (&array), curr (0), curr_table_pos (items - 1) {}
  
  Vertex_Node *operator() (void)
    {
      if (curr_table_pos <= 0)  /* we're at the end of the table, quit. */
        return 0;
      else if (!curr)
        {                       /* need to find a new bucket list to search. */
          
          while (curr_table_pos >= 0 && !assoc->hash_table[curr_table_pos])
            curr_table_pos--;
          
          if (curr_table_pos < 0)
            return 0;
          else
            curr = assoc->hash_table[curr_table_pos];
        }
      
      Vertex_Node *temp = curr;
      if (!(curr = curr->next)) /* try to update curr for next call. */
        curr_table_pos--;
      return temp;
    }
};

/* This is a very simple stack abstraction. Doesn't perform error checking. */

class Node_Stack
{
private:
  int           top;
  int           max_node_stack_size;
  Vertex_Node **node_stack_items;
  
public:
  Node_Stack (int size): top (size), max_node_stack_size (size)
    {
      node_stack_items = new Vertex_Node *[size];
    }
  
  int empty ()
    {
      return top >= max_node_stack_size;
    }
  
  void push (Vertex_Node *item)
    {
      node_stack_items[--top] = item;
    }
  
  Vertex_Node *pop (void)
    {
      return node_stack_items[top++];
    }
};

const int DEFAULT_SIZE = 200;
const int BUF_SIZE = 200;

main (int argc, char **argv)
{
  Assoc_Array assoc_array (argc > 1 ? atoi (argv[1]) : DEFAULT_SIZE);
  String prev;
  String succ;
  
  /* The use of ``+='' below is rather obscure.  It simply means ``prepend the
     address of A[Succ] to the adjacency list of A[Prev].  See the comment in
     the member definition for class Vertex_Node.  Note that the ++ operator
     has also been overloaded.  This allows concise, extremely efficient mechanism
     to enter the Prev and Succ items into the associative array. */
  
  while (cin >> prev >> succ) 
    if (prev == succ)       /* Just record existence of identical tokens. */
      assoc_array[prev];
    else 
      assoc_array[prev] += ++assoc_array[succ];
  
  Assoc_Iter   next (assoc_array, assoc_array.max_size ());
  Node_Stack   stack (assoc_array.array_size ());
  Vertex_Node *node_ptr;
  
  /* Iterate through all the items in the associative array, performing the
     typical topological sort trick of pushing all ``sources'' onto a stack.
     This gives us an order of magnitude win over the standard UNIX tsort
     routine, which has quadratic average time complexity (in the number of
     verticies!!!!). */
  
  while (node_ptr = next ()) 
    if (node_ptr->in_degree_count () == 0) 
      stack.push (node_ptr);
  
  /* The following code is straight-forward, but the one line with the expression
     --(*List->Item ()) == 0 is rather cryptic.  It is simply dereferencing a pointer
     to a Vertex_Node, and then passing this by reference to the overloaded --
     operator from the Vertex_Node class (yes, it *is* easy to abuse this
     technique!). */
  
  for (int items = 0; !stack.empty (); items++)
    {
      node_ptr = stack.pop ();
      cout << node_ptr->item () << "\n";
      
      for (Adjacency_Node *list = node_ptr->get_succ_list (); list; list = list->next ())
        if (--*list->item () == 0) 
          stack.push (list->item ());                         
      
    }
  
  if (items != assoc_array.array_size ())
    {
      cerr << argv[0] << ": error, directed graph has a cycle!\n";
      return 1;
    }
  else
    return 0;
}

