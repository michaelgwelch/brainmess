package welch.brainmess;


import org.junit.Test;
import org.jmock.*;

public class InstructionTests {

	@Test
	public void moveForward_Execute() {

		// Arrange
		Mockery mock = new Mockery();
		final ExecutionContext context = mock.mock(ExecutionContext.class);
		
		
		mock.checking(new Expectations() {{
			oneOf(context).moveForward();
		}});
		
		
		// Act
		Instruction.MOVE_FORWARD.execute(context);
		
		// Assert
		mock.assertIsSatisfied();
	}


	@Test
	public void moveBackward_Execute() {

		// Arrange
		Mockery mock = new Mockery();
		final ExecutionContext context = mock.mock(ExecutionContext.class);
		
		
		mock.checking(new Expectations() {{
			oneOf(context).moveBackward();
		}});
		
		
		// Act
		Instruction.MOVE_BACKWARD.execute(context);
		
		// Assert
		mock.assertIsSatisfied();
	}
	
	@Test
	public void testAndJumpForward_Execute() {
		// Arrange
		Mockery mock = new Mockery();
		final ExecutionContext context = mock.mock(ExecutionContext.class);
		
		mock.checking(new Expectations() {{
			oneOf(context).testAndJumpForward();
		}});
		
		// Act
		Instruction.TEST_AND_JUMP_FORWARD.execute(context);
		
		// Assert
		mock.assertIsSatisfied();
	}
}
