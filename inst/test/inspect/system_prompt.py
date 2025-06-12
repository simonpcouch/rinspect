from inspect_ai import Task, task
from inspect_ai.dataset import Sample
from inspect_ai.scorer import model_graded_qa
from inspect_ai.solver import generate, system_message


@task
def system_prompt():
    dataset = [
        Sample(input="What's 2+2?", target="4"),
        Sample(input="What's 2+3?", target="5"),
    ]

    return Task(
        dataset=dataset,
        solver=[
            system_message("Be terse."),
            generate()
        ],
        scorer=model_graded_qa(),
    )
