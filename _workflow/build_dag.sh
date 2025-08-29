#!/bin/bash
# Set path
LANG=C make -pBnd | python3 _workflow/make_p_to_json.py | python3 _workflow/json_to_dot.py | dot -Tpng -Gdpi=300 -o workflow.png
