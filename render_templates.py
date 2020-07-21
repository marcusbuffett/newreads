#!/usr/bin/env python3
import os
import pathlib
from pathlib import Path

import jinja2

templates_dir = "./docker_templates"
destination = "./docker"

print("Rendering templates...")

for dirpath, dirs, files in os.walk(templates_dir):
    for filename in files:
        #  os.system('envsubst ')
        destination_path = Path(destination) / Path(dirpath).relative_to(templates_dir) / filename
        source_path = Path(templates_dir) / Path(dirpath).relative_to(templates_dir) / filename
        destination_path.parent.mkdir(parents=True, exist_ok=True)
        rendered = jinja2.Template(open(source_path, "r").read()).render(env=os.environ)
        open(destination_path, "w").write(rendered)

print("Done rendering templates!")
