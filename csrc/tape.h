#ifndef _Tape_H_
#define _Tape_H_

#include <glib.h>

typedef struct _Tape Tape;

struct _Tape {
   GList *currentCell;
};

Tape* tape_new();
gchar* tape_to_string(Tape *tape);
void tape_increment(Tape *tape);
void tape_decrement(Tape *tape);
void tape_move_forward(Tape *tape);
void tape_move_backward(Tape *tape);
void tape_set(Tape *tape, int value);
int tape_get(Tape *tape);


#endif
