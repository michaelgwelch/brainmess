#ifndef _Tape_H_
#define _Tape_H_

#include <glib.h>

typedef struct _Tape Tape;


Tape* tape_new();
void tape_free(Tape *tape);
gchar* tape_to_string(const Tape *tape);
void tape_increment(Tape *tape);
void tape_decrement(Tape *tape);
void tape_move_forward(Tape *tape);
void tape_move_backward(Tape *tape);
void tape_set(Tape *tape, int value);
gint tape_get(const Tape *tape);
gboolean tape_equals(const Tape *tape1, const Tape *tape2);


#endif
