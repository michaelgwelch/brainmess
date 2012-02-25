#ifndef _PROGRAM_H_
#define _PROGRAM_H_
#include <glib.h>

typedef struct _Program Program;

Program* program_new(const gchar* prog);
void program_free(Program* prog);
void program_jump_forward(Program* prog);
void program_jump_backward(Program* prog);
gboolean program_end(const Program* prog);
gchar program_fetch(Program* prog);



#endif
