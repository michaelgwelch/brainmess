#include <stdlib.h>
#include "tape.h"
#include "program.h"
#include "execute.h"

void execute(gchar instruction, Program* program,
    Tape* tape, ReadChar reader, WriteChar writer)
{
    switch(instruction)
    {
        case '>':
            tape_move_forward(tape);
            break;

        case '<':
            tape_move_backward(tape);
            break;
        
        case '+':
            tape_increment(tape);
            break;

        case '-':
            tape_decrement(tape);
            break;

        case '.':
            writer((gchar)tape_get(tape));
            break;

        case ',':
            tape_set(tape, (gint)reader());
            break;

        case '[':
            if (tape_get(tape) == 0) 
            {
                program_jump_forward(program);
            }
            break;

        case ']':
            if (tape_get(tape) != 0)
            {
                program_jump_backward(program);
            }
            break;

        default:
            break;
    }
}
