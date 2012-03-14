package welch.brainmess;


import org.junit.Test;
import org.jmock.*;
import static org.junit.Assert.assertEquals;

public class InstructionTests {
	
	private final Mockery mockery = new Mockery();
	private final ExecutionContext executionContext = mockery.mock(ExecutionContext.class);


	@Test
	public void moveForward_Execute() {

		// Arrange
		mockery.checking(new Expectations() {{
			oneOf(executionContext).moveForward();
		}});
		
		
		// Act
		Instruction.MOVE_FORWARD.execute(executionContext);
		
		// Assert
		mockery.assertIsSatisfied();
	}


	@Test
	public void moveBackward_Execute() {

		// Arrange
		mockery.checking(new Expectations() {{
			oneOf(executionContext).moveBackward();
		}});
		
		
		// Act
		Instruction.MOVE_BACKWARD.execute(executionContext);
		
		// Assert
		mockery.assertIsSatisfied();
	}
	
	@Test
	public void testAndJumpForward_Execute() {
		// Arrange
		mockery.checking(new Expectations() {{
			oneOf(executionContext).testAndJumpForward();
		}});
		
		// Act
		Instruction.TEST_AND_JUMP_FORWARD.execute(executionContext);
		
		// Assert
		mockery.assertIsSatisfied();
	}
	
	@Test
	public void testAndJumpBackward_Execute() {
		// Arrange
		mockery.checking(new Expectations() {{ 
			oneOf(executionContext).testAndJumpBackward();
		}});
		
		// Act
		Instruction.TEST_AND_JUMP_BACKWARD.execute(executionContext);
		
		// Assert
		mockery.assertIsSatisfied();
	}
	
	@Test
	public void increment_Execute() {
		// Arrange
		mockery.checking(new Expectations() {{ 
			oneOf(executionContext).increment();
		}});
		
		// Act
		Instruction.INCREMENT.execute(executionContext);
		
		// Assert
		mockery.assertIsSatisfied();
	}
	
	@Test
	public void decrement_Execute() {
		// Arrange
		mockery.checking(new Expectations() {{ 
			oneOf(executionContext).decrement();
		}});
		
		// Act
		Instruction.DECREMENT.execute(executionContext);
		
		// Assert
		mockery.assertIsSatisfied();
	}
	
	@Test
	public void input_Execute() {
		// Arrange
		mockery.checking(new Expectations() {{ 
			oneOf(executionContext).input();
		}});
		
		// Act
		Instruction.INPUT.execute(executionContext);
		
		// Assert
		mockery.assertIsSatisfied();
	}
	
	@Test
	public void output_Execute() {
		// Arrange
		mockery.checking(new Expectations(){{ 
			oneOf(executionContext).ouput();
		}});
		
		// Act
		Instruction.OUTPUT.execute(executionContext);
		
		// Assert
		mockery.assertIsSatisfied();
	}
	
	@Test
	public void noOperation_Execute() {
		// Arrange 
		
		
		// Act
		Instruction.NO_OPERATION.execute(executionContext);
		
		// Assert
		mockery.assertIsSatisfied();
	}
	

	@Test
	public void parseMoveForwardChar() {
		// Arrange
		Instruction expected = Instruction.MOVE_FORWARD;
		
		// Act
		Instruction actual = Instruction.parseInstruction('>');
		
		// Assert
		assertEquals(expected, actual);
	}
	
	@Test
	public void parseMoveBackward() {
		// Arrange
		Instruction expected = Instruction.MOVE_BACKWARD;
		
		// Act
		Instruction actual = Instruction.parseInstruction('<');
		
		// Assert
		assertEquals(expected, actual);
	}
	
	@Test
	public void parseIncrement() {
		// Arrange
		Instruction expected = Instruction.INCREMENT;
		
		// Act
		Instruction actual = Instruction.parseInstruction('+');
		
		// Assert
		assertEquals(expected, actual);
	}
	
	@Test
	public void parseDecrement() {
		// Arrange
		Instruction expected = Instruction.DECREMENT;
		
		// Act
		Instruction actual = Instruction.parseInstruction('-');
		
		// Assert
		assertEquals(expected, actual);
	}
	
	@Test
	public void parseInput() {
		// Arrange
		Instruction expected = Instruction.INPUT;
		
		// Act
		Instruction actual = Instruction.parseInstruction(',');
		
		// Assert
		assertEquals(expected, actual);
	}
	
	@Test
	public void parseOutput() {
		// Arrange
		Instruction expected = Instruction.OUTPUT;
		
		// Act
		Instruction actual = Instruction.parseInstruction('.');
		
		// Assert
		assertEquals(expected, actual);
	}
	
	@Test
	public void parseTestAndJumpForward() {
		// Arrange
		Instruction expected = Instruction.TEST_AND_JUMP_FORWARD;
		
		// Act
		Instruction actual = Instruction.parseInstruction('[');
		
		// Assert
		assertEquals(expected, actual);
	}
	
	@Test
	public void parseTestAndJumpBackward() {
		// Arrange
		Instruction expected = Instruction.TEST_AND_JUMP_BACKWARD;
		
		// Act
		Instruction actual = Instruction.parseInstruction(']');
		
		// Assert
		assertEquals(expected, actual);
	}
	
	@Test
	public void parseNoOperation() {
		// Arrange
		Instruction expected = Instruction.NO_OPERATION;
		
		// Act
		Instruction actual = Instruction.parseInstruction('H');
		
		// Assert
		assertEquals(expected, actual);
	}
	
	@Test
	public void toStringMoveForward() {
		assertEquals("MOVE_FORWARD", Instruction.MOVE_FORWARD.toString());
	}
	
	@Test
	public void hashCodeTest() {
		Instruction.MOVE_BACKWARD.hashCode();
	}

}
