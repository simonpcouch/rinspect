import sys
import json
from inspect_ai.log._log import EvalLog
from pydantic import ValidationError

try:
    with open(sys.argv[1], 'r') as f:
        data = json.load(f)
    
    # Validate the data against the EvalLog model
    EvalLog.model_validate(data)
    print('Validation successful')
    sys.exit(0)
except ValidationError as e:
    print(f'Validation error: {e}')
    sys.exit(1)
except Exception as e:
    print(f'Error: {e}')
    sys.exit(2)
