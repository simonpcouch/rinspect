from inspect_ai import Task, task
from inspect_ai.dataset import Sample
from inspect_ai.solver import generate, use_tools
from inspect_ai.scorer import match
from inspect_ai.tool import tool

@tool
def take_sum():
    async def execute(x: int, y: int):
        """
        Sum two numbers.
        
        Args:
            x: First number to add.
            y: Second number to add.
            
        Returns:
            The sum of the two numbers.
        """
        return x + y
    
    return execute

@task
def arithmetic_with_tool():
    dataset = [
        Sample(input="What's 71+31?", target="102"),
        Sample(input="What's 91+13?", target="104")
    ]
    
    return Task(
        dataset=dataset,
        solver=[
            use_tools(take_sum()),
            generate()
        ],
        scorer=match(numeric=True),
    )
