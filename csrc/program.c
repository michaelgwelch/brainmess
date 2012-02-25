#include <stdlib.h>
#include <string.h>
#include "program.h"

struct _Program 
{
    gchar* instructions;
    size_t length;
    int index;
};

Program* program_new(const gchar* prog)
{
    // TODO: Expect prog to be null terminated. What is
    // best practice for handling?

    Program* program;
    program = malloc(sizeof(*program));

    // TODO: use a glib function later.
    // Don't leave room for NULL.
    program->length = strlen(prog);
    program->instructions = malloc(program->length * sizeof(gchar));
    strncpy(program->instructions, prog, program->length);
    program->index = 0;

    return program;

}

void program_free(Program* prog)
{
    free(prog->instructions);
    free(prog);
}

gboolean program_end(const Program* prog)
{
    return prog->index >= prog->length;
}

gchar program_fetch(Program* prog)
{
    gchar instruction = prog->instructions[prog->index];
    prog->index++;
    return instruction;
}

void program_jump_forward(Program* prog)
{
    int nestLevel = 1;
    while(nestLevel > 0)
    {
        gchar current = prog->instructions[prog->index];
        if (current == '[') nestLevel++;
        else if (current == ']') nestLevel--;
        prog->index++;
    }
}

void program_jump_backward(Program* prog)
{
    prog->index -= 2;
    int nestLevel = 1;
    while (nestLevel > 0)
    {
        gchar current = prog->instructions[prog->index];
        if (current == '[') nestLevel--;
        else if (current == ']') nestLevel++;
        prog->index--;
    }
    prog->index++;
}
