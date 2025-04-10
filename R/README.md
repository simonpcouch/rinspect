# vitals's source

The bread and butter functionality of vitals lives in `task.R`. A substantial subset of the rest of the package implements translation to Inspect-compatible .json evaluation logs.

## Translation

Functionality to translate from `Task` objects to Inspect-compatible .json logs lives in `translate*.R`.

* Functions named `translate_to_*()` translate from Task objects (or subsets of them) to specific fields in the resulting .json. For example, `translate_to_plan()` translates to the `$plan` slot of the resulting json, and `translate_to_plan_steps()` translates to the `$plan$steps` slot.
* The Inspect viewer validates .json log files using pydantic models. The helper `validate_log()` runs generated .json through those models and forms the ground truth for unit testing translation functions.

## Utilities

Exported functions that don't operate directly on a single Task and instead take in e.g. a directory path or a list of Tasks are prefixed with `vitals_*()` and are defined in `*.R`, e.g. the definition for `vitals_bundle()` lives in `bundle.R`.

## Relationship to Inspect

vitals doesn't integrate directly with the Inspect AI through any sort of reticulate-ry in exported functionality. That said, it _does_ make use of Inspect or submodules from it in a couple places:

* Package tests use Inspect's pydantic models to validate generate .json evaluation logs via `validate_log()`. Tests will be skipped if you don't have Inspect installed--see `.github/workflows/live-api.yaml` for an example minimal viable setup.
* `vitals_view()` bundles the static Inspect log viewer via `inst/dist/`. This is a standalone .js application and doesn't require an install of Inspect.

## Cached Objects

vitals uses a number of cached objects for testing. To regenerate them, use the internal function `regenerate_example_objects()`. You will need an `ANTHROPIC_API_KEY` to do so.
