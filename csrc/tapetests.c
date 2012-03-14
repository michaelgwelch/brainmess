#include "tape.h"
#include <CUnit/Basic.h>


void testTapeIncrement(void)
{
    // Arrange
    int expected_numbers[] = {2,5,7};
    int actual_starting_numbers[] = {2,4,7};
    Tape* expectedTape = tape_load(expected_numbers, 3, 1);
    Tape* actualTape = tape_load(actual_starting_numbers, 3, 1);
    printf("Expected: %s, Actual: %s\n", tape_to_string(expectedTape),
        tape_to_string(actualTape));

    // Act
    tape_increment(actualTape);

    // Assert
    CU_ASSERT(tape_equals(expectedTape, actualTape));
    
    printf("Expected: %s, Actual: %s\n", tape_to_string(expectedTape),
        tape_to_string(actualTape));
    
}

int main()
{

    Tape* tape = tape_new();
    tape_increment(tape);
    tape_increment(tape);
    tape_move_forward(tape);
    tape_increment(tape);
    printf("%s\n", tape_to_string(tape));


    CU_pSuite pSuite = NULL;

    if (CUE_SUCCESS != CU_initialize_registry()) return CU_get_error();

    pSuite = CU_add_suite("Tape Tests", NULL, NULL);
    if (NULL == pSuite) 
    {
        CU_cleanup_registry();
        return CU_get_error();
    }

    CU_add_test(pSuite, "tape increment", testTapeIncrement);

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    CU_cleanup_registry();
    return CU_get_error();
}
