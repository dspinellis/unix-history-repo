// From: "Douglas C. Schmidt" <schmidt@glacier.ics.uci.edu>
// Date: Sun, 25 Sep 88 16:19:35 -0700

#include <stream.h>
#include <ctype.h>
#include <String.h>
#include <generic.h>
#include <stdlib.h>

#define queue(type) name2(type,queue)
#define list(type) name2(type,list)
#define queuedeclare(type)                     \
class queue(type) {                            \
   struct list(type) {                         \
      type item;                               \
      list(type) *next;                        \
   } *head;                                    \
   int sz;                                     \
public:                                        \
   queue(type)(void) {head = 0;sz = 0;}        \
   ~queue(type)(void) {                        \
      list(type) *temp;                        \
      while (head) {                           \
         temp = head;                          \
         head = head->next;                    \
         delete temp;                          \
      }                                        \
   }                                           \
   int  empty(void) {return(!head);}           \
   int  size(void) {return(sz);}               \
   void enqueue(type new_item);                \
   type front(void) {                          \
      return(head->next->item);                \
   }                                           \
   type dequeue(void);                         \
};

#define queueimplement(type)                   \
type queue(type)::dequeue(void) {              \
   if (!head)                                  \
      abort();                                 \
   type temp = head->next->item;               \
   list(type) *temp_ptr;                       \
   if ((temp_ptr = head->next) == head) {      \
      head = 0;                                \
   }                                           \
   else {                                      \
      head->next = temp_ptr->next;             \
   }                                           \
   delete temp_ptr;                            \
   sz--;                                       \
   return(temp);                               \
}                                              \
void queue(type)::enqueue(type new_item) {     \
   if (!head) {                                \
      head       = new list(type);             \
      head->item = new_item;                   \
      head->next = head;                       \
   }                                           \
   else {                                      \
      list(type) *temp_node  = new list(type); \
      temp_node->item  = new_item;             \
      temp_node->next  = head->next;           \
      head->next       = temp_node;            \
      head             = temp_node;            \
   }                                           \
   sz++;                                       \
}                                              \ 


queuedeclare(String);
queueimplement(String);
queuedeclare(double);
queueimplement(double);

main() {
   String Buf;
   queue(String) Q_String;
   queue(double) Q_double;

   while (cin >> Buf) {
      if (Buf.matches(RXalpha)) {
         Q_String.enqueue(String(Buf));
      }
      else if (Buf.matches(RXdouble)) {
         Q_double.enqueue(atof(Buf));
      }   
   }

   while (!Q_String.empty()) {
      cout << "Size = " << Q_String.size() << ",Item = " 
           << Q_String.front() << "\n";
      void(Q_String.dequeue());
   }

   while (!Q_double.empty()) {
      cout << "Size = " << Q_double.size() << ",Item = " 
           << Q_double.front() << "\n";
      void(Q_double.dequeue());
   }
   return (0);
}

