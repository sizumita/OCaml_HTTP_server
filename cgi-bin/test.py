#!/Users/sizumita/local/python-3.10.0b1/bin/python
import os
print("<h1>Hello World!</h1>")
env = os.environ["HTTP_ACCEPT"]
print(f"<p>{env}a</p>")
