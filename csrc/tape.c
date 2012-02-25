#include <stdlib.h>
#include "tape.h"

const gpointer GPTR_ZERO = GINT_TO_POINTER(0);

Tape* tape_new()
{
    Tape* newTape;
    newTape = malloc(sizeof(*newTape));
    newTape->currentCell = g_list_append(NULL, 0);
    return newTape;
}

GList* nextCell(GList *list)
{
    GList* next = g_list_next(list);
    if (NULL == next)
    {
        GList* new_list = g_list_append(list, GPTR_ZERO); 
        next = g_list_next(new_list);
    }
    return next;
}

GList* prevCell(GList *list)
{
    GList* prev = list->prev;
    if (NULL == prev)
    {
        prev = g_list_prepend(list, GPTR_ZERO);
    }
    return prev;
}

void tape_set(Tape *tape, int value)
{
    tape->currentCell->data = GINT_TO_POINTER(value);
}

int tape_get(Tape *tape)
{
    return GPOINTER_TO_INT(tape->currentCell->data);
}

void tape_increment(Tape *tape)
{
    tape_set(tape, tape_get(tape) + 1);
}

void tape_decrement(Tape *tape)
{
    tape_set(tape, tape_get(tape) - 1);
}

void tape_move_forward(Tape *tape)
{
    tape->currentCell = nextCell(tape->currentCell);
}

void tape_move_backward(Tape *tape)
{
    tape->currentCell = prevCell(tape->currentCell);
}

void append_next(gpointer data, gpointer string)
{
    g_string_append_printf(string, ", %d", GPOINTER_TO_INT(data));
}

const gboolean RETURN_BUFFER = FALSE;
gchar* tape_to_string(Tape* tape)
{
    GString *result = g_string_new("Tape [");
    GList *current = g_list_first(tape->currentCell);
    g_string_append_printf(result, "%d", GPOINTER_TO_INT(current->data));
    g_list_foreach(current->next, (GFunc) append_next, result);
    g_string_append(result, "]");
    return g_string_free(result, RETURN_BUFFER);
}
