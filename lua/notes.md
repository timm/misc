literate configuration

what's that hack where  u  generate settings 
from doc string?


✅ Parsing from a docstring

✅ Safe type handling (literal_eval)

✅ Command-line overrides (--key=val)

✅ Environment variable overrides (MYAPP_key=val)

not DRY, not WET

ChatGPT said:
You're thinking of a slick Python trick where you 
define configuration settings inside a docstring, 
then parse them into a dictionary — often called a 
"docstring settings hack" or "literate configuration".

🧪 Example: Settings from Docstring
python
Copy
Edit
def settings():
    """
    - bins = 5
    - min  = 0
    - file = ../data.csv
    """
    import re
    return {k: eval(v) for k,v in re.findall(r"(\w+)\s*=\s*(.+)", settings.__doc__)}
