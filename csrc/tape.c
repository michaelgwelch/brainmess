#include <stdlib.h>
#include "tape.h"

const gpointer GPTR_ZERO = GINT_TO_POINTER(0);

// NOTES:
// What do you do in a C program if an argument is NULL
// and that isn't expected?

struct _Tape 
{
    GList *currentCell;
};

int index_of_node_in_list(GList *node)
{
    int index = 0;
    GList *current = node;

    while(current != NULL)
    {
        current = g_list_previous(current);
        index++;
    }

    return index;
}

Tape* tape_new()
{
    Tape* newTape;
    newTape = malloc(sizeof(*newTape));
    newTape->currentCell = g_list_append(NULL, 0);
    return newTape;
}

Tape* tape_load(int* nums, int length, int current)
{
    Tape* newTape = tape_new();

    int i = 0;
    GList* currentCell;
    while (i < length)
    {
        tape_set(newTape, nums[i]);
        if (i == current)
        {
            currentCell = newTape->currentCell;
        }
        i++;
    }

    newTape->currentCell = currentCell;
    return newTape;
}
void tape_free(Tape *tape)
{
    g_list_free(g_list_first(tape->currentCell));
    free(tape);
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

int tape_get(const Tape *tape)
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
gchar* tape_to_string(const Tape* tape)
{
    GString *result = g_string_new("Tape [");
    GList *current = g_list_first(tape->currentCell);
    g_string_append_printf(result, "%d", GPOINTER_TO_INT(current->data));
    g_list_foreach(current->next, (GFunc) append_next, result);
    g_string_append(result, "]");
    return g_string_free(result, RETURN_BUFFER);
}

gboolean tape_equals(const Tape *tape1, const Tape *tape2)
{
    GList* iter1 = g_list_first(tape1->currentCell);
    GList* iter2 = g_list_first(tape2->currentCell);

    gboolean equal = TRUE;
    while(iter1 != NULL && iter2 != NULL)
    {
        if (iter1->data != iter2->data)
        {
            equal = FALSE;
            break;
        }
        iter1 = g_list_next(iter1);
        iter2 = g_list_next(iter2);
    }

    if (equal)
    {
        equal = iter1 == iter2;
    }

    if (equal)
    {
        gint index1 = index_of_node_in_list(tape1->currentCell);
        gint index2 = index_of_node_in_list(tape2->currentCell);
        equal = (index1 == index2);
    }

    return equal;
}
