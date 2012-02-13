using System;
using System.IO;
using System.Collections.Generic;

using NUnit.Framework;


namespace Welch.Brainmess
{
	[TestFixture]
	public class InstructionTests
	{
		[Test]
		public void FromInt_WithMoveForwardChar_ExpectMoveForwardInstruction()
		{
			AssertInstructionReturnedForChar(Instruction.MoveForward, '>');
		}
		
		[Test]
		public void FromInt_WithMoveBackwardChar_ExpectMoveBackwardInstruction()
		{
			AssertInstructionReturnedForChar(Instruction.MoveBackward, '<');
		}
		
		[Test]
		public void FromInt_WithOutputChar_ExpectOutputInstruction()
		{
			AssertInstructionReturnedForChar(Instruction.Output, '.');
		}
		
		[Test]
		public void FromInt_WithInputChar_ExpectInputInstruction()
		{
			AssertInstructionReturnedForChar(Instruction.Input, ',');
		}
		
		[Test]
		public void FromInt_WithIncrementChar_ExpectIncrementInstruction()
		{
			AssertInstructionReturnedForChar(Instruction.Increment, '+');
		}
		
		[Test]
		public void FromInt_WithDecrementChar_ExpectDecrementInstruction()
		{
			AssertInstructionReturnedForChar(Instruction.Decrement, '-');
		}
		
		[Test]
		public void FromInt_WithTestAndJumpForwardChar_ExpectTestAndJumpForwardInstruction()
		{
			AssertInstructionReturnedForChar(Instruction.TestAndJumpFoward, '[');
		}
		
		[Test]
		public void FromInt_WithTestAndJumpBackwardChar_ExpectTestAndJumpBackwardInstruction()
		{
			AssertInstructionReturnedForChar(Instruction.TestAndJumpBackward, ']');
		}
		
		[Test]
		public void FromInt_WithA_ExpectNoOperationInstruction()
		{
			AssertInstructionReturnedForChar(Instruction.NoOperation, 'A');
		}
		
		private static void AssertInstructionReturnedForChar(Instruction expectedInstruction, int characterValue)
		{
			// Arrange
			var expected = expectedInstruction;
			
			// Act
			var actual = Instruction.FromInt(characterValue);
			
			// Assert
			Assert.AreEqual(expected, actual);
		}
		
		[Test]
		public void Execute_MoveForward_ExpectTapeCursorMovesForward()
		{
			ExecuteInstructionAndCheckPredicate(Instruction.MoveForward, context => context.MoveForwardCalled);
		}
		
		[Test]
		public void Execute_MoveBackward_ExpectTapeCursorMovesBackward()
		{
			ExecuteInstructionAndCheckPredicate(Instruction.MoveBackward, context => context.MoveBackwardCalled);
		}
		
		[Test]
		public void Execute_Increment_ExpectIncrementCalled()
		{
			ExecuteInstructionAndCheckPredicate(Instruction.Increment, context => context.IncrementCalled);
		}
		
		[Test]
		public void Execute_Decrement_ExpectDecrementCalled()
		{
			ExecuteInstructionAndCheckPredicate(Instruction.Decrement, context => context.DecrementCalled);
		}
		
		private static void ExecuteInstructionAndCheckPredicate(Instruction instruction, Predicate<MockExecutionContext> predicate)
		{
			// Arrange
			var context = new MockExecutionContext();
			
			// Act
			instruction.Execute(context);
			
			// Assert
			Assert.IsTrue(predicate(context));
		}
		
		[Test]
		public void Execute_Input_ExpectReadCalledAndSetCurrentCalled()
		{
			// Arrange
			var context = new MockExecutionContext();
			context.NextValueForInput = 223;
			var instruction = Instruction.Input;
			
			// Act
			instruction.Execute(context);
			
			// Assert
			Assert.AreEqual(223, context.CurrentTapeValue);
			
		}
		
		[Test]
		public void Execute_Output_ExpectGetCurrentCalledAndWriteCalled()
		{
			// Arrange
			var context = new MockExecutionContext();
			context.CurrentTapeValue = 415;
			var instruction = Instruction.Output;
			
			// Act
			instruction.Execute(context);
			
			// Assert
			Assert.AreEqual(415, context.NextValueForOutput);
		}
		
		[Test]
		public void Execute_TestAndJumpForwardWithValueNotZero_ExpectNoJump()
		{
			// Arrange
			var context = new MockExecutionContext();
			context.CurrentTapeValue = 35;
			var instruction = Instruction.TestAndJumpFoward;
			
			// Act
			instruction.Execute(context);
			
			// Assert
			Assert.IsFalse(context.JumpForwardCalled);
			
		}
		
		[Test]
		public void Execute_TestAndJumpForwardWithValueZero_ExpectJump()
		{
			// Arrange
			var context = new MockExecutionContext();
			context.CurrentTapeValue = 0; // this is default value, just wanted to be explicit
			var instruction = Instruction.TestAndJumpFoward;
			
			// Act
			instruction.Execute(context);
			
			// Assert
			Assert.IsTrue(context.JumpForwardCalled);
			
		}
		
		[Test]
		public void Execute_TestAndJumpBackwardWithValueNotZero_ExpectJump()
		{
			// Arrange
			var context = new MockExecutionContext();
			context.CurrentTapeValue = 35;
			var instruction = Instruction.TestAndJumpBackward;
			
			// Act
			instruction.Execute(context);
			
			// Assert
			Assert.IsTrue(context.JumpBackwardCalled);
			
		}
		
		[Test]
		public void Execute_TestAndJumpForwardWithValueZero_ExpectNoJump()
		{
			// Arrange
			var context = new MockExecutionContext();
			context.CurrentTapeValue = 0; // this is default value, just wanted to be explicit
			var instruction = Instruction.TestAndJumpBackward;
			
			// Act
			instruction.Execute(context);
			
			// Assert
			Assert.IsFalse(context.JumpBackwardCalled);
			
		}
		
		// I really need a mock framework to make sure other stuff isn't happening
		
		private class MockExecutionContext : IExecutionContext
		{
			public bool MoveForwardCalled { get; private set; }
			public bool MoveBackwardCalled { get; private set; }
			public bool IncrementCalled { get; private set; }
			public bool DecrementCalled { get; private set; }
			
			public int NextValueForInput { get; set; }
			public char NextValueForOutput { get; set; }
			
			public bool JumpForwardCalled { get; private set; }
			public bool JumpBackwardCalled { get; private set; }
			
			public void MoveTapeForward()
			{
				MoveForwardCalled = true;
			}
			
			public void MoveTapeBackward()
			{
				MoveBackwardCalled = true;
			}

			public int CurrentTapeValue
			{
				get; set;
			}

			public void IncrementTape()
			{
				IncrementCalled = true;
			}

			public void DecrementTape()
			{
				DecrementCalled = true;
			}
			
			public int ReadFromInput()
			{
				return NextValueForInput;
			}

			public void WriteToOutput(char character)
			{
				NextValueForOutput = character;
			}

			public void JumpProgramBackward()
			{
				JumpBackwardCalled = true;
			}

			public void JumpProgramForward()
			{
				JumpForwardCalled = true;
			}
		}
	}
}

