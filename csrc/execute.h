#ifndef _EXECUTE_H_
#define _EXECUTE_H_

typedef gchar       (*ReadChar)     ();
typedef void        (*WriteChar)    (gchar ch);

void execute(gchar instruction, Program* program,
        Tape* tape, ReadChar reader, WriteChar writer);

#endif
