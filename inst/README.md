## Package system files

The `.json` files in `/logs` are the resulting log file from running evals in `inst/inspect-examples`, e.g. with: 

```bash
inspect eval inst/inspect-examples/basics.py  --model anthropic/claude-3-5-sonnet-latest --log-format=json
```

...or:

```bash
inspect eval inst/inspect-examples/tools.py  --model anthropic/claude-3-5-sonnet-latest --log-format=json
```

`/dist` is a bundled version of the Inspect viewer. (See [here](https://github.com/UKGovernmentBEIS/inspect_ai/blob/88d1cd98041a245c1d0cca4536d60e3244630b78/src/inspect_ai/_view/www/README.md) for more information.)
