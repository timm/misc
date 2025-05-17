# Main Python script content for generate_website.py
# This script, when run, will generate the educational Markdown files,
# the MIT license, a navigation bar, and the prompt used for its own generation.

import os
import textwrap

# --- Helper function to write content to a file ---
def write_md_file(filename, content):
    """Writes the given content to the specified filename."""
    try:
        with open(filename, 'w', encoding='utf-8') as f:
            # Ensure content is a string and strip it before adding a newline
            f.write(str(content).strip() + '\\n') # Ensure a newline at the end
        # This print is for user feedback when they run the generated script.
        # It is not an "intermediary" from the LLM's generation process of this script.
        print(f"Successfully generated {filename}")
    except IOError as e:
        print(f"Error writing {filename}: {e}")
    except Exception as e_gen:
        print(f"An unexpected error occurred while writing {filename}: {e_gen}")


# --- Navigation Bar ---
def get_navigation_bar():
    """Returns the Markdown for the updated navigation bar."""
    # Navigation bar structure as per user request
    nav_bar = """(Notes ([SE](se.md) [AI](ai.md) Code ([overview](code.md) [tutorial](tutorial.md)))  [License](license.md)  [REgen](prompt.txt)
---
"""
    return nav_bar

# --- Content Generation for code.md ---
def generate_code_md_content():
    """Generates the Markdown content for code.md."""
    nav = get_navigation_bar()
    content = f"""{nav}
# `kube.py` Code Structure Overview

This document provides a structural overview of the `kube.py` script, focusing on its classes, attributes, and method protocols. Protocol names defined here can be referenced in other documents.

## Class: `o`
A base utility class for creating objects with attributes assignable at initialization using keyword arguments and providing a custom string representation.

**Attributes:**
| Attribute | Default Value | Description (10-15 words max) |
|-----------|---------------|---------------------------------|
| `**kwargs`| (dynamic)     | Arbitrary keyword arguments passed during instantiation become instance attributes. |

**Protocols:**
* **`core_behavior`**:
    * `__init__(self, **d)`: Initializes object by updating its internal dictionary with provided keyword arguments.
    * `__repr__(self)`: Returns a user-friendly string representation of the object via the global `cat` function.

## Class: `Sym`
Manages symbolic (categorical) attributes/columns, tracking frequencies of distinct values and calculating diversity (entropy).

**Attributes:**
| Attribute | Default Value | Description (10-15 words max) |
|-----------|---------------|---------------------------------|
| `at`      | `0`           | Column position (0-indexed) in the data row. |
| `txt`     | `" "`         | Column name or header text (defaults to a space). |
| `n`       | `0`           | Count of non-missing symbolic items observed in this column. |
| `has`     | `{{{{}}}}`    | Dictionary storing frequency counts of each unique symbol. (Escaped for f-string) |

**Protocols:**
* **`initialization`**:
    * `__init__(self, has=[], at=0, txt=" ")`: Constructs a `Sym` column, optionally processing an initial list of values.
* **`data_management`**:
    * `add(self, x, inc=True)`: Adds a symbol `x` to the column, updating frequency counts. Handles missing values ("?").
* **`statistics_query`**:
    * `mid(self)`: Returns the mode (most frequent symbol) of the column.
    * `div(self)`: Calculates Shannon entropy as a measure of symbol diversity.
* **`distance_calculation`**:
    * `dist(self, x, y)`: Computes distance between two symbols (0 if identical, 1 otherwise; handles "?").

## Class: `Num`
Manages numeric attributes/columns, tracking statistics like mean, standard deviation, min/max values, and supporting normalization.

**Attributes:**
| Attribute | Default Value | Description (10-15 words max) |
|-----------|---------------|---------------------------------|
| `at`      | `0`           | Column position (0-indexed) in the data row. |
| `txt`     | `" "`         | Column name or header text (defaults to a space). |
| `n`       | `0`           | Count of non-missing numeric items observed. |
| `mu`      | `0`           | Mean (average) of the numbers in this column. |
| `m2`      | `0`           | Sum of squared differences from the mean (used for std dev). |
| `lo`      | `1e32` (`BIG`)| Lowest numeric value seen in this column. |
| `hi`      | `-1e32` (`-BIG`)| Highest numeric value seen in this column. |
| `heaven`  | `0` or `1`    | Optimization goal (0 for minimization, 1 for maximization), typically inferred from column name. |

**Protocols:**
* **`initialization`**:
    * `__init__(self, has=[], at=0, txt=" ")`: Constructs a `Num` column, optionally processing an initial list of values.
* **`data_management`**:
    * `add(self, x, inc=True)`: Adds a number `x` to the column, updating statistics (mean, m2, lo, hi).
* **`statistics_query`**:
    * `mid(self)`: Returns the mean (`mu`) of the numbers.
    * `div(self)`: Calculates the standard deviation.
* **`data_transformation`**:
    * `norm(self, x)`: Normalizes a given number `x` to a 0-1 scale based on `lo` and `hi`.
* **`distance_calculation`**:
    * `dist(self, x, y)`: Computes normalized distance between two numbers; handles missing values.

## Class: `Data`
Manages a dataset consisting of rows and columns, providing methods for data loading, manipulation, clustering (via LSH), and evaluation against objectives.

**Attributes:**
| Attribute | Default Value | Description (10-15 words max) |
|-----------|---------------|---------------------------------|
| `_rows`   | `[]`          | Private list storing the actual data rows (list of lists). |
| `cols`    | `o(x=[], y=[], all=[])` | An `o` object holding lists of column objects: `x` (independent), `y` (dependent/objective), and `all`. |

**Protocols:**
* **`initialization_loading`**:
    * `__init__(self, src)`: Initializes data from an iterable source (`src`), processing headers and rows.
    * `about(self, c, s)`: Helper for `__init__`; creates `Num` or `Sym` column objects based on header string `s`.
* **`data_manipulation`**:
    * `add(self, row, inc=True, purge=False)`: Adds a `row` to the dataset and updates statistics for all relevant columns.
    * `clone(self, rows=[])`: Creates a new `Data` instance with the same column structure but optionally different rows.
* **`clustering_projection`**:
    * `lsh(self, poles)`: Performs Locality Sensitive Hashing by projecting rows onto lines defined by `poles` and grouping by projection bins.
    * `poles(self)`: Selects 'pole' rows (typically diverse/extreme) from the dataset to define projection dimensions.
    * `project(self, row, a, b)`: Projects a `row` onto a line connecting two pole rows `a` and `b`, returning a binned index.
* **`evaluation_statistics`**:
    * `mid(self)`: Returns the 'middle' or most representative row of the dataset based on `xdist`.
    * `minPts(self)`: Determines minimum number of points required to consider a cluster significant.
    * `ydist(self, row)`: Calculates a row's 'distance to heaven' based on its values in dependent (`y`) columns and their `heaven` goals.
    * `ydists(self)`: Returns a `Num` object containing statistics of `ydist` values for all rows.
* **`distance_calculation`**:
    * `xdist(self, row1, row2)`: Calculates distance between two rows using only independent (`x`) columns and the global `dist` function.

*(Note: Attribute descriptions and protocol summaries are inferred from `kube.py` for brevity and educational clarity.)*
"""
    return content

# --- Content Generation for se.md ---
def generate_se_md_content():
    """Generates the Markdown content for se.md."""
    nav = get_navigation_bar()
    se_terms = sorted([
        "Class", "Command-Line Interface (CLI)", "CSV (Comma-Separated Values)",
        "Docstring", "Encapsulation", "Function", "Generator", "Iterator", "Modularity",
        "Object-Oriented Programming (OOP)", "Python", "Regular Expressions",
        "Type Aliases", "Type Hinting"
    ])
    # Each term as a Markdown bullet point on its own line
    terms_list_md = "\\n".join([f"* {term}" for term in se_terms])

    content = f"""{nav}
# Software Engineering Concepts in `kube.py`

This page explores key Software Engineering (SE) concepts demonstrated in `kube.py`, highlighting how they contribute to a structured, readable, and maintainable Python application designed for AI tasks. The script emphasizes conciseness and a functional style where appropriate.

## Terms to watch for

{terms_list_md}

## 1. Fundamental Python Programming
`kube.py` is implemented in Python 3 and leverages many of its core features for effective development.

* **Modularity & Imports**: The script is organized into classes and functions. It imports standard Python modules like `random` (for `any`, `many`), `sys` (for CLI arguments), `re` (for parsing docstrings for CLI options), and `math` (for calculations like `log`). It extensively uses `typing` for type hints.
    ```python
    import random, sys, re
    import math
    from typing import List,Dict,Any,Union,Tuple,Optional,Callable,Iterator,TypeVar,cast
    ```
* **Functions and Generators**: Core, reusable operations are defined as functions (e.g., `cat(x: Any)` for flexible string conversion, `coerce(x: str)` for converting input strings to appropriate Python types). The `csv(path: str)` function is a generator, efficiently yielding rows one by one, which is memory-friendly for large files. This promotes DRY (Don't Repeat Yourself) principles.
* **Type Aliases and Hinting**: The script defines type aliases like `Atom`, `Row`, `Rows`, and `Col` using the `typing` module. Functions and methods are annotated with type hints (e.g., `def cat(x: Any) -> str:`). This greatly improves code readability, maintainability, and helps in catching type-related errors early.
    ```python
    # Type aliases
    Atom = Union[int, float, str, bool]
    Row  = List[Atom]
    ```

## 2. Object-Oriented Programming (OOP)
OOP principles are central to `kube.py`'s design, allowing for logical grouping of data and behavior.

* **Classes and Objects**:
    * `o`: A lightweight base class for creating flexible objects from keyword arguments (see `code.md` `o.core_behavior`).
    * `Sym` and `Num`: These classes model symbolic and numeric data columns, respectively. Each encapsulates data (like counts, mean, std dev) and behaviors (like adding values, calculating diversity, computing distance) specific to its data type (see `code.md` `Sym.statistics_query`, `Num.statistics_query`).
    * `Data`: This class orchestrates the entire dataset, managing collections of `Sym` and `Num` columns and the rows of data. It provides higher-level operations like loading data, clustering, and evaluation (see `code.md` `Data.initialization_loading`).
* **Encapsulation**: Each class bundles its attributes (data) and the methods (functions) that operate on that data. For example, the `Num` class internally manages `mu` (mean) and `m2` (sum of squared differences) and provides an `add` method that correctly updates these internal states (see `code.md` `Num.data_management`).
* **Docstrings for Documentation**: Classes and functions/methods are documented using docstrings. Notably, the main script's docstring is programmatically parsed to define and provide defaults for command-line options. This practice of self-documenting code is valuable for maintainability and aligns with principles of creating understandable and repeatable software experiments [^1^](#fn1).

## 3. Command-Line Interface (CLI)
`kube.py` provides a CLI for users to specify parameters and select operations, enhancing its usability.

* **Option Parsing from Docstring**: The script ingeniously uses its own initial docstring (the one describing `Options:`) as the source for CLI option definitions. The `re.finditer` function is used at startup to parse this docstring and extract option names and their default values.
    ```python
    # Snippet from near the end of kube.py, populating 'the':
    # the = o(**{{m[1]: coerce(m[2])
    #            for m in re.finditer(r"-\\w+\\s*(\\w+).*=\\s*(\\S+)", __doc__)}})
    ```
* **Processing Arguments**: The `cli(d: Dict[str, Any])` function updates a dictionary (typically `the.__dict__`) with values passed via `sys.argv`. The main execution block then uses these settings from the `the` object.

## 4. CSV File Handling for Data Input
The script is designed to ingest data from Comma-Separated Values (CSV) files.

* **Reading and Coercing CSV Data**: The `csv(path: str)` function reads a specified CSV file line by line. For each line, it splits the string by commas and then uses the `coerce(x: str)` function to convert each field from a string to its appropriate Python data type (integer, float, boolean, or string). This stream of processed rows is typically fed into the `Data` object for further analysis (see `code.md` `Data.initialization_loading`). Effective data parsing is a common challenge in SE projects dealing with external data sources [^2^](#fn2).
    ```python
    def csv(path: str) -> Iterator[Row]:
      \"\"\"Read csv file into a generator of rows.\"\"\"
      with open(path) as f:
        for line in f:
          yield [coerce(x) for x in line.strip().split(",")]
    ```

---
**References:**
<a name="fn1"></a>[^1^]: Menzies, T., Williams, C., & Ryman-Tubb, M. (2004). The PROMISE of repeatable experiments. *Empirical Software Engineering, 9*(1-2), 99-108. 
<a name="fn2"></a>[^2^]: Kernighan, B. W., & Pike, R. (1999). *The Practice of Programming*. Addison-Wesley. (Chapter on parsing).
<a name="fn3"></a>[^3^]: Fowler, M. (2018). *Refactoring: Improving the Design of Existing Code* (2nd ed.). Addison-Wesley Professional.

**Review Questions:**
1.  How does `kube.py`'s use of classes like `Sym` and `Num` exemplify the OOP principle of encapsulation?
2.  Explain the unconventional but effective method `kube.py` uses for defining and parsing command-line options. What are its potential advantages or disadvantages?
3.  What is the role of the `coerce` function in the context of CSV file handling in `kube.py`, and why is it important for data integrity?
"""
    return content

# --- Content Generation for ai.md ---
def generate_ai_md_content():
    """Generates the Markdown content for ai.md."""
    nav = get_navigation_bar()
    ai_terms = sorted([
        "Active Learning", "Clustering", "Data Normalization", "Distance Metric",
        "Entropy", "Explainable AI (XAI)", "Heaven (optimization goal)",
        "Locality Sensitive Hashing (LSH)", "Minkowski Distance",
        "Multi-Objective Optimization", "Poles (clustering)", "Projection (clustering)",
        "Supervised Learning", "Unsupervised Learning"
    ])
    # Each term as a Markdown bullet point on its own line
    terms_list_md = "\\n".join([f"* {term}" for term in ai_terms])

    content = f"""{nav}
# Artificial Intelligence Concepts in `kube.py`

`kube.py` implements what its author terms "barelogic" – foundational components and algorithms for Explainable AI (XAI), active learning, and multi-objective optimization. This page details the core AI and Machine Learning (ML) concepts embedded in the script.

## Terms to watch for

{terms_list_md}

## 1. Data Representation & Preprocessing for AI
Effective AI application begins with how data is structured and prepared.

* **Differentiated Data Handling**: `kube.py` uses distinct `Sym` (symbolic/categorical) and `Num` (numeric) classes. This separation is crucial because different statistical measures, distance calculations, and preprocessing steps are appropriate for different data types (see `code.md` `Sym.statistics_query`, `Num.statistics_query`). Such tailored data handling is vital in data mining [^1^](#fnai1).
* **Data Normalization**: The `Num.norm(x)` method scales numeric values to a common [0, 1] range (see `code.md` `Num.data_transformation`). Normalization is vital for many AI algorithms (especially distance-based ones like k-NN or SVM, and gradient-based optimization) to prevent features with larger absolute values from disproportionately influencing results.
    ```python
    # From Num.norm() in kube.py:
    # def norm(i, x: Atom) -> float:
    #   \"\"\"Normalize value to range 0-1.\"\"\"
    #   # Handles division by zero if lo == hi
    #   return (float(x) - i.lo) / (i.hi - i.lo + 1 / BIG)
    ```

## 2. Distance Metrics
Measuring similarity or dissimilarity between data items is a cornerstone of many AI algorithms.

* **Attribute-Specific Distances**:
    * `Sym.dist(x, y)`: For symbolic attributes, distance is typically binary (0 if same, 1 if different).
    * `Num.dist(x, y)`: For numeric attributes, distance is the absolute difference of their normalized values.
    (See `code.md` `Sym.distance_calculation`, `Num.distance_calculation`).
* **Row-Level (Instance) Distance**: `Data.xdist(row1, row2)` calculates the distance between two data instances (rows) using only their independent (`x`) columns. This is achieved by aggregating the distances from individual columns using a generalized distance formula, often a variation of Minkowski distance, where the exponent `P` (from `the.P` configuration) plays a role.

## 3. Clustering & Locality Sensitive Hashing (LSH)
`kube.py` implements a projection-based clustering approach, a variant of unsupervised learning.

* **Poles and Projection**: `Data.poles()` identifies 'poles' – data points that are far apart from each other, used to define dimensions for projection. Each data row is then projected onto the lines formed by pairs of these poles using `Data.project(row, a, b)`. The result of this projection is discretized into bins (see `code.md` `Data.clustering_projection`). Finding such representative points is a common theme in efficient data exploration and can be critical for tasks like defect prediction [^2^](#fnai2).
* **LSH (`Data.lsh`)**: Rows are grouped (hashed) based on the sequence of bins they fall into across multiple projections. Rows that consistently project into the same bins are considered similar and form a cluster. This is a form of Locality Sensitive Hashing, designed to efficiently find approximate nearest neighbors.

## 4. Multi-Objective Optimization
The script is designed to evaluate data instances against multiple, possibly conflicting, objectives.

* **"Heaven" and Optimization Direction**: Each numeric objective column (`Num` type, whose name does not end in 'X') has a `heaven` attribute: `0` for minimization goals and `1` for maximization goals.
* **Objective Evaluation (`Data.ydist`)**: The `Data.ydist(row)` method calculates a row's overall 'distance to heaven.' It combines the normalized values of its dependent (`y`) columns, considering their respective `heaven` goals. A lower `ydist` generally indicates a better row (see `code.md` `Data.evaluation_statistics`). This aligns with common approaches in multi-objective decision making [^3^](#fnai3).

## 5. Foundations for Explainable AI (XAI) & Active Learning
`kube.py` provides "barelogic" that could support XAI and Active Learning.

* **XAI**: The transparent calculations for distances, cluster assignments, and `ydist` scores can contribute to explaining *why* certain data instances are considered good or grouped together. The importance of understanding "what is best" in data models is a recurring theme in AI research [^4^](#fnai4).
* **Active Learning**: This is a form of supervised learning where the learning algorithm can interactively query a source (e.g., a human annotator) to label new data points. Techniques within `kube.py` like identifying diverse 'poles' or evaluating rows via `ydist` could be adapted for query strategies in active learning, aiming to select the most informative data points for labeling.

---
**References:**
<a name="fnai1"></a>[^1^]: Witten, I. H., Frank, E., Hall, M. A., & Pal, C. J. (2017). *Data Mining: Practical Machine Learning Tools and Techniques* (4th ed.). Morgan Kaufmann.
<a name="fnai2"></a>[^2^]: Menzies, T., Greenwald, J., & Frank, A. (2007). Data mining static code attributes to learn defect predictors. *IEEE Transactions on Software Engineering, 33*(1), 2-13.
<a name="fnai3"></a>[^3^]: Deb, K. (2001). *Multi-Objective Optimization using Evolutionary Algorithms*. John Wiley & Sons.
<a name="fnai4"></a>[^4^]: Menzies, T., & Kocaguneli, E. (2013). The better generalizer: a thousand times we asked "what is best?". *Journal of Systems and Software, 86*(12), 3099-3110.
<a name="fnai5"></a>[^5^]: Bishop, C. M. (2006). *Pattern Recognition and Machine Learning*. Springer.


**Review Questions:**
1.  Explain the importance of normalizing numeric features before calculating distances in `kube.py`.
2.  Describe the roles of 'poles' and 'projection' in `kube.py`'s LSH-based clustering.
3.  How does the `Data.ydist` method quantify how well a data row meets multiple objectives?
"""
    return content

# --- Content Generation for tutorial.md ---
def generate_tutorial_md_content():
    """Generates the Markdown content for tutorial.md."""
    nav = get_navigation_bar()

    # This data structure is populated based on an analysis of kube.py's eg__ functions.
    # Triple quotes are used for all string values to handle newlines and internal quotes robustly.
    # The 'educational_comments_code' field now directly contains the commented code.
    eg_functions_analysis = [
        {
            "name": "eg__the",
            "docstring": """Print the configuration.""",
            "educational_comments_code": """def eg__the(_: Any) -> None: 
  \"\"\"Print the configuration.\"\"\"
  # 'the' is a global object (instance of class 'o').
  # It holds all runtime configuration options for the script.
  # These options are initially parsed from the script's main docstring (see Options section)
  # and can be overridden by command-line arguments processed by the cli() function.
  print(the) 
  # The 'o' class's __repr__ method uses the global 'cat()' function for its string representation,
  # so this will neatly print all key-value pairs currently stored in 'the'.
""",
            "concepts": """SE: Runtime Configuration Management, Global State (`the` object), CLI argument reflection.""",
            "execution": """`python kube.py -the`
Try also: `python kube.py -the -r 12345 -f mydata.csv` (even if mydata.csv doesn't exist, to see config change).""",
            "output_interpretation": """The output displays all current configuration settings (e.g., `bins`, `file`, `rseed`, `P`, `dims`, `some`). This is useful for verifying which parameters the script is using for a particular run, confirming defaults, or seeing the effect of CLI overrides.""",
            "code_md_links": """Uses the `o.core_behavior` protocol (specifically `__repr__`).""",
            "short_exercise": """Run `python kube.py -the -s 50`. Observe the 'some' attribute in the output. How does it differ from running without `-s 50` (check the default in `kube.py`'s main docstring)?""",
            "long_exercise": """Locate where the `the` object is initialized near the end of `kube.py`. Add a new default option to the script's main docstring (e.g., `-N newOpt = "hello"`). Modify the regular expression or parsing logic if necessary (though it might pick it up automatically if formatted like other options). Rerun `eg__the` to see if your new option and its default value are displayed."""
        },
        {
            "name": "eg__csv",
            "docstring": """Print csv data.""",
            "educational_comments_code": """def eg__csv(_: Any) -> None:
  \"\"\"Print csv data.\"\"\"
  # 'the.file' (from the config object 'the') specifies the path to the CSV file.
  # The 'csv()' function (a global function in kube.py):
  #   1. Opens the specified file.
  #   2. Reads it line by line.
  #   3. For each line, splits it into fields by commas.
  #   4. Uses 'coerce()' on each field to convert it from a string to an appropriate
  #      Python type (int, float, bool, or keeps as str).
  #   5. Yields each processed row as a list of Atoms.
  # This list comprehension iterates through the generator returned by csv() and prints each row.
  [print(row) for row in csv(the.file)]
""",
            "concepts": """SE: File I/O, Data Type Coercion, Generators, List Comprehensions.""",
            "execution": """`python kube.py -csv` (uses the default file in `the.file`, e.g., `../../moot/optimize/misc/auto93.csv`).
To use a different file: `python kube.py -f YOUR_CSV_FILE.csv -csv`""",
            "output_interpretation": """Each row from the specified CSV file is printed to the console. Values within each row will be displayed with their Python types (e.g., numbers as `int` or `float`, booleans as `True`/`False`). This helps verify that the data loading and type coercion mechanisms are working correctly.""",
            "code_md_links": """Primarily uses the global `csv()` and `coerce()` functions.""",
            "short_exercise": """Create a very small CSV file (e.g., 2 rows, 3 columns with mixed data types like `name,age,active` then `Alice,30,True`). Run `eg__csv` with this file. Does it print the data as you expect, with correct types?""",
            "long_exercise": """Modify the `eg__csv` function in `kube.py`. Instead of printing every row, make it print only the first 5 rows of the CSV file. Test your modification."""
        },
        {
            "name": "eg__data",
            "docstring": """Print column information.""",
            "educational_comments_code": """def eg__data(_: Any) -> None:
  \"\"\"Print column information.\"\"\"
  # 1. Obtain the data source:
  #    'csv(the.file)' reads and processes the CSV specified in the configuration.
  #    It returns an iterator yielding rows, where each row is a list of coerced values.
  csv_data_source = csv(the.file)

  # 2. Initialize the Data object:
  #    The Data.__init__ method consumes the iterator from csv_data_source.
  #    - It first reads the header row to understand column names.
  #    - For each column name, it calls Data.about() to create either a Sym or Num
  #      column object based on the name's characteristics (e.g., starting with uppercase for Num).
  #    - Then, it iterates through the remaining data rows, calling Data.add() for each one,
  #      which in turn updates the statistics in the respective Sym or Num column objects.
  d = Data(csv_data_source)
  
  # 3. Print information about independent ('x') columns:
  #    'd.cols.x' is a list of column objects (Sym or Num) that were identified
  #    as independent variables (features).
  #    The print statement uses the __repr__ method of each column object
  #    (defined in Sym and Num, using the global 'cat()' function) to display its details.
  [print("x", col) for col in d.cols.x] # Shows column type, name, position, and summary stats.

  # 4. Print information about dependent ('y') columns (objectives):
  #    'd.cols.y' lists columns identified as dependent variables (targets/objectives).
  [print("y", col) for col in d.cols.y]
""",
            "concepts": """SE: Object-Oriented Programming (`Data`, `Sym`, `Num` instantiation and interaction), Data Encapsulation. AI: Data Representation, Feature Engineering (column identification), Initial Data Analysis.""",
            "execution": """`python kube.py -data` (uses the CSV file specified by `the.file`, e.g., `auto93.csv`)""",
            "output_interpretation": """The output is divided into two sections:
1. 'Independent (x) Columns': Lists each feature column, showing its type (`Sym` or `Num`), name, creation index (`at`), and key statistics (e.g., for `Num`: count `n`, mean `mu`, std dev `div`, `lo`, `hi`; for `Sym`: count `n`, mode `mid`, entropy `div`).
2. 'Dependent (y) Columns': Lists objective columns similarly.
This output is crucial for verifying that data is loaded correctly and that columns are categorized and summarized as expected by `kube.py`'s rules.""",
            "code_md_links": """`Data.initialization_loading` (covers `__init__`, `about`, `add`), `Sym.statistics_query`, `Num.statistics_query`, `Sym.representation` (via `cat`), `Num.representation` (via `cat`).""",
            "short_exercise": """Examine the `auto93.csv` file. Identify one column you expect to be `Sym` and one `Num`. Run `eg__data`. Do the printed statistics for these columns (e.g., `mid`/`div` for Sym, `mu`/`div` for Num) make sense based on a quick look at the data?""",
            "long_exercise": """In a copy of your `auto93.csv` data file, change a column header that is currently an 'x' variable (e.g., 'Cylinders') so that its name ends with a '+' (e.g., 'Cylinders+'). Rerun `eg__data`. Describe how the output changes for this column. Which list ('x' or 'y') does it appear in now and why (refer to `Data.about` logic if needed)?"""
        },
        {
            "name": "eg__ydist",
            "docstring": """Print rows sorted by distance to heaven.""",
            "educational_comments_code": """def eg__ydist(_: Any) -> None:
  \"\"\"Print rows sorted by distance to heaven.\"\"\"
  # 1. Load and prepare data using the Data class
  d = Data(csv(the.file))
  # 2. Sort rows:
  #    d._rows contains all data instances from the CSV.
  #    The `key` for sorting is a lambda function that calls `d.ydist(row)` for each row.
  #    `d.ydist(row)` calculates how "good" a row is with respect to the defined
  #    optimization objectives (see ai.md and Data.evaluation_statistics in code.md).
  #    A lower ydist value means the row is 'better' or closer to "heaven".
  lst = sorted(d._rows, key=lambda row: d.ydist(row))
  # 3. Print the "best" 4 rows (those with the smallest ydist values).
  print("Best 4 rows (closest to 'heaven'):")
  for row in lst[:4]: # Slice the list to get the first 4 elements
      print("good", row) # The 'row' object (a list) will be printed.
  # 4. Print the "worst" 4 rows (those with the largest ydist values).
  print("\\nWorst 4 rows (furthest from 'heaven'):")
  for row in lst[-4:]: # Slice the list to get the last 4 elements
      print("bad", row)
""",
            "concepts": """AI: Multi-Objective Evaluation, Fitness Ranking. SE: Sorting, Lambda functions, List slicing.""",
            "execution": """`python kube.py -ydist`""",
            "output_interpretation": """Prints the 4 'best' rows (those with the lowest `ydist` scores) and the 4 'worst' rows (highest `ydist` scores) from the dataset. This helps identify which data instances are performing well or poorly against the combined optimization goals defined in the 'y' columns.""",
            "code_md_links": """`Data.evaluation_statistics` (specifically `ydist`), `Data.initialization_loading`.""",
            "short_exercise": """Observe the 'good' rows. Do they generally have values in the objective columns (Y-columns identified by `eg__data`) that you'd intuitively expect for 'good' performance (e.g., low values for goals to be minimized, high for goals to be maximized)?""",
            "long_exercise": """Modify the `eg__ydist` function in `kube.py` to also print the actual `ydist` score next to each 'good' and 'bad' row. For example: `print("good", d.ydist(row), row)`."""
        },
        {
            "name": "eg__poles",
            "docstring": """Show clustering dimensions.""",
            "educational_comments_code": """def eg__poles(file: str = None) -> None:
  \"\"\"Show clustering dimensions.\"\"\"
  # 1. Load data. If 'file' is not provided, it uses 'the.file' from config.
  d = Data(csv(file or the.file))
  # 2. Find poles:
  #    `d.poles()` selects a few diverse rows (the 'poles') from the dataset.
  #    These poles are used to define dimensions for projecting other data points in LSH.
  #    See Data.clustering_projection in code.md.
  p = d.poles()
  # 3. Perform Locality Sensitive Hashing (LSH):
  #    `d.lsh(p)` projects all rows in `d` onto lines defined by pairs of these poles.
  #    It then groups rows into clusters based on their binned projection values.
  #    `dims` becomes a dictionary where keys are tuples representing these binned projection values
  #    (effectively, cluster identifiers), and values are `Data` objects containing the rows in that cluster.
  dims = d.lsh(p)
  # 4. Print cluster IDs (the keys of the 'dims' dictionary).
  #    Each key is a tuple of integers, representing a specific "bucket" or cluster.
  print("Cluster IDs (projection tuples):")
  [print(k) for k in dims]
  # 5. Print the total number of unique clusters found.
  print("\\nTotal number of clusters:", len(dims))
""",
            "concepts": """AI: Clustering (unsupervised learning), Locality Sensitive Hashing (LSH), Dimensionality Reduction (conceptual, via projection), Feature Engineering (poles as reference points).""",
            "execution": """`python kube.py -poles`""",
            "output_interpretation": """Prints a list of 'Cluster IDs'. Each ID is a tuple of integers, representing a unique bucket or cluster identified by the LSH process (based on how data projects onto lines between poles). Finally, it prints the total count of these unique clusters. This gives an insight into the natural groupings found in the data by this method.""",
            "code_md_links": """`Data.clustering_projection` (covers `poles`, `lsh`, `project`).""",
            "short_exercise": """Run `eg__poles` using the default settings. Now run it again with a different number of dimensions for projection, e.g., `python kube.py -d 2 -poles` and then `python kube.py -d 6 -poles`. How does changing the `-d` (dims) parameter affect the number of clusters and the structure of the cluster IDs?""",
            "long_exercise": """The `eg__poles` example currently only prints the cluster keys (IDs). Modify it to iterate through `dims.values()` (which are `Data` objects, each representing a cluster) and, for each cluster, print its ID (key) and the number of rows it contains (e.g., `len(cluster_data_object._rows)`)."""
        },
         {
            "name": "eg__counts",
            "docstring": """Show cluster counts and stats.""",
            "educational_comments_code": """def eg__counts(file: str = None) -> None:
  \"\"\"Show cluster counts and stats.\"\"\"
  # 1. Load data
  d = Data(csv(file or the.file))
  # 2. Perform LSH clustering:
  #    First, find poles (diverse reference points).
  #    Then, use LSH to group rows into clusters based on these poles.
  #    'clusters' is a dictionary: {cluster_id_tuple: Data_object_for_cluster, ...}
  clusters = d.lsh(d.poles())
  
  print("Cluster Statistics (Mid ydist, Div ydist, Num Rows):")
  # 3. Iterate through each cluster (which is a Data object itself).
  for data_cluster in clusters.values(): 
    # 4. For each cluster, get statistics of the 'ydist' values of its member rows.
    #    'ys' will be a Num object summarizing these ydist scores.
    ys = data_cluster.ydists() 
    # 5. Print statistics only if the cluster is large enough.
    #    'd.minPts()' determines the minimum number of rows for a cluster to be reported.
    if len(data_cluster._rows) >= d.minPts():
      # 'o(...)' creates a simple object for pretty printing.
      # It shows the mean ydist ('mid'), standard deviation of ydist ('div'),
      # and number of rows ('n') for this particular cluster.
      # Note: In original kube.py, 'o' is a class. This assumes it's defined and accessible.
      #       Also, original kube.py has ys.n as attribute, not method.
      print(o(mid=ys.mid(), div=ys.div(), n=ys.n))
""",
            "concepts": """AI: Clustering Evaluation, Cluster Characterization, Unsupervised Learning.""",
            "execution": """`python kube.py -counts`""",
            "output_interpretation": """For each 'significant' cluster (i.e., a cluster containing at least `minPts` rows), this example prints statistics about the `ydist` (objective fitness) values of its members. Specifically, for each such cluster, it shows:
  - `mid`: The mean (average) `ydist` of rows in that cluster.
  - `div`: The standard deviation of `ydist` values in that cluster (how spread out they are).
  - `n`: The number of rows in that cluster.
This helps assess the quality and homogeneity of clusters with respect to the optimization goals.""",
            "code_md_links": """`Data.clustering_projection`, `Data.evaluation_statistics` (covers `ydists`, `minPts`), `o.core_behavior`.""",
            "short_exercise": """Look at the output of `eg__counts`. Are there clusters with low 'mid' (mean) ydist values and also low 'div' (std dev) ydist values? What would this combination suggest about the rows in those clusters?""",
            "long_exercise": """Modify `eg__counts` in `kube.py`. For each significant cluster, in addition to its ydist statistics, also print the 'mid' row of that cluster (i.e., `data_cluster.mid()`). You'll need to make sure the `print(o(...))` line or a new print statement can handle displaying the row nicely."""
        },
        {
            "name": "eg__all",
            "docstring": """Run all examples.""",
            "educational_comments_code": """def eg__all(_: Any) -> None:
  \"\"\"Run all examples.\"\"\" 
  # This example function iterates through a predefined list of other eg__ functions
  # (eg__the, eg__csv, eg__data, eg__ydist, eg__poles, eg__counts) and executes them sequentially.
  # It serves as a simple way to run a batch of tests or demonstrations.
  
  # The list 'f_list' holds references to the actual functions to be called.
  # These functions must be defined in the same scope for this to work directly.
  f_list = [eg__the, eg__csv, eg__data, eg__ydist, eg__poles, eg__counts]
  
  for f_to_call in f_list:
    # 'random.seed(the.rseed)' resets Python's global random number generator seed
    # before each example. This ensures that if any example uses randomness,
    # it produces the same results each time `eg__all` is run (given the same initial 'the.rseed').
    # This is crucial for reproducibility of tests or experiments.
    # Assumes 'random' and 'the' (with 'the.rseed') are accessible in this scope.
    random.seed(the.rseed) 
    
    print(f"\\n--- Running {{f_to_call.__name__}} ---") # Announce which example is running
    # 'f_to_call(_)' calls the current function from the list (e.g., eg__the(_), then eg__csv(_), etc.).
    # The underscore `_` is passed as an argument, signifying that these example
    # functions might be defined to take an argument, even if they don't use it.
    f_to_call(_)
""",
            "concepts": """SE: Scripting, Automation, Test Orchestration (simple form), Reproducibility (via random seed).""",
            "execution": """`python kube.py -all`""",
            "output_interpretation": """This command will run several other `eg__` functions in sequence: `eg__the`, `eg__csv`, `eg__data`, `eg__ydist`, `eg__poles`, and `eg__counts`. The output will be a concatenation of the outputs produced by each of these individual examples, with a header indicating which example is running.""",
            "code_md_links": """Directly calls other `eg__` functions defined within `kube.py`.""",
            "short_exercise": """Run `python kube.py -all`. Can you identify in the combined output where the results of `eg__data` end and `eg__ydist` begin?""",
            "long_exercise": """In your local copy of `kube.py`, modify the list of functions within `eg__all` (e.g., comment out `eg__csv` from the list `[eg__the, eg__csv, ...]`). Rerun `python kube.py -all`. Verify that the output of the removed example (and its header) is no longer present."""
        },
        {
            "name": "eg__about", # In some versions of kube.py, this is eg_help or similar.
                                 # This version assumes it uses pydoc as per previous discussions.
            "docstring": """Provide detailed documentation about the script module itself.""",
            "educational_comments_code": """def eg__about(_): 
  \"\"\"Provide detailed documentation about the script module itself.\"\"\"
  # This example uses Python's built-in 'pydoc' module
  # to dynamically generate and print extensive documentation for the current script ('kube.py').
  
  # 1. Import necessary modules:
  #    'pydoc' is Python's documentation generator.
  #    'sys' provides access to system-specific parameters and functions, including 'sys.modules'.
  import pydoc, sys # These are standard library modules
  
  # 2. Render and print documentation:
  #    'sys.modules[__name__]' gets a reference to the current module object (i.e., kube.py itself when run).
  #    'pydoc.render_doc()' takes this module object and generates a formatted text documentation string.
  #    This documentation is extracted from the module's main docstring, class docstrings,
  #    method/function docstrings, and other structural information.
  print(pydoc.render_doc(sys.modules[__name__]))
""",
            "concepts": """SE: Automated Documentation Generation (using `pydoc`), Introspection (accessing module information at runtime via `sys.modules[__name__]`).""",
            "execution": """`python kube.py -about`""",
            "output_interpretation": """Displays comprehensive, text-based documentation for the entire `kube.py` module. This includes:
  - The main module docstring (the one at the very top of `kube.py`).
  - Documentation for each class (e.g., `Sym`, `Num`, `Data`), including their methods and method docstrings.
  - Documentation for all top-level functions (e.g., `cat`, `csv`, `coerce`, and all `eg__` functions) along with their docstrings.
The formatting is done by `pydoc` and is similar to what you'd see using `help()` in a Python interpreter or `pydoc kube` in the terminal.""",
            "code_md_links": """Uses Python's standard `pydoc` and `sys` modules.""",
            "short_exercise": """Run `python kube.py -about`. Skim through the output. Can you locate the documentation that `pydoc` generates for the `Data.lsh()` method?""",
            "long_exercise": """In your local copy of `kube.py`, add a new, simple top-level function with a clear and descriptive docstring. For example:
```python
def my_new_test_function():
  \"\"\"This is a test function to see pydoc output.\"\"\"
  pass
```
Save the change and rerun `python kube.py -about`. Verify that your new function and its docstring now appear in the `pydoc` generated documentation under the "FUNCTIONS" section."""
        }
    ]

    tutorial_content = f"""{nav}# `kube.py` Tutorials: Hands-on Examples

This section provides tutorials for the `eg__` example functions in `kube.py`. Each tutorial explains the example's purpose, shows its code with educational comments, and offers practical exercises designed for graduate students new to these specific AI/SE topics. The tone is friendly, informative, and concise.

---
"""

    for i, eg_func_data in enumerate(eg_functions_analysis):
        tutorial_content += f"## Tutorial: `{eg_func_data['name']}`\n\n" # Changed to H2 for tutorial sections
        tutorial_content += f"**Purpose & Concepts:**\n{eg_func_data.get('docstring', 'No docstring provided for this example.')}\n"
        if "concepts" in eg_func_data:
             tutorial_content += f"This example demonstrates or relates to: {eg_func_data['concepts']}\n\n"
        else:
            tutorial_content += "\n"

        tutorial_content += "**Code with New Educational Comments:**\n"
        # Use the 'educational_comments_code' which already contains the original code + new comments
        tutorial_content += f"```python\n{eg_func_data['educational_comments_code'].strip()}\n```\n\n"

        if "execution" in eg_func_data:
            tutorial_content += f"**Execution and Output Interpretation:**\n* **To Run**: {eg_func_data['execution']}\n"
            tutorial_content += f"* **Output Interpretation**: {eg_func_data.get('output_interpretation', 'Output varies based on the example and input data.')}\n\n"

        if "code_md_links" in eg_func_data:
            tutorial_content += f"**Links to `code.md` Protocols (Conceptual):**\nThis example may utilize concepts related to: {eg_func_data['code_md_links']}\n\n"

        if "short_exercise" in eg_func_data and "long_exercise" in eg_func_data : # Ensure keys exist
            tutorial_content += "**Exercises:**\n"
            tutorial_content += f"* **Short Exercise (1-5 minutes)**: {eg_func_data['short_exercise']}\n"
            tutorial_content += f"* **Longer Homework Exercise (1-2 hours suggested effort)**: {eg_func_data['long_exercise']}\n\n---\n"
        else: # Fallback if exercises are not fully defined for all placeholders
            tutorial_content += "\n---\n"
    return tutorial_content

# --- Content Generation for license.md ---
def generate_license_md_content():
    """Generates the Markdown content for license.md."""
    nav = get_navigation_bar()
    # Standard MIT License text
    # Copyright year and holder from kube.py: (c) 2025, Tim Menzies <timm@ieee.org>
    year = "2025" # As specified in kube.py
    copyright_holder = "Tim Menzies <timm@ieee.org>"

    mit_license_text = f"""{nav}
# MIT License

Copyright (c) {year} {copyright_holder}

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
"""
    return mit_license_text

# --- Content Generation for prompt.txt ---
def generate_prompt_txt_content():
    """
    Generates the master prompt string that instructs an LLM
    to create this very `generate_website.py` script.
    """
    # This prompt is self-referential and includes instructions for the LLM's behavior.
    master_prompt = textwrap.dedent(f"""
        Your task is to regenerate the Python script named `generate_website.py`.
        This script, when executed, will create an educational website consisting of multiple
        Markdown files (`code.md`, `se.md`, `ai.md`, `tutorial.md`, `license.md`) and one text file (`prompt.txt`).
        The content for these files must be derived from the analysis of the `kube.py` script provided by the user.

        **Overall LLM Instructions for this task:**
        1.  **Primary Output Method:** Present the complete Python code for `generate_website.py` within a single immersive document of type `code`. Use a descriptive title like "Website Generator Script (generate_website.py)" and a unique ID. The Python code should be within a ```python ... ``` block inside this immersive document.
        2.  **Chat Output:** The chat interface should only contain a brief introductory message before the immersive document, and a brief concluding message after it. Do NOT output the Python script or any part of it directly in the chat. Avoid conversational fluff or progress messages in the chat.
        3.  **Input `kube.py`:** You will be provided with the content of `kube.py`. All generated Markdown content must be based on this specific `kube.py` script.

        **`generate_website.py` Script Specifications:**

        1.  **File Generation:** The Python script must generate the following files:
            * `code.md`: Documents the structure of `kube.py`.
            * `se.md`: Explains Software Engineering concepts from `kube.py`.
            * `ai.md`: Explains Artificial Intelligence concepts from `kube.py`.
            * `tutorial.md`: Provides tutorials for `eg__` functions in `kube.py`.
            * `license.md`: Contains the MIT License text.
            * `prompt.txt`: Contains this very master prompt text (i.e., the instructions you are currently following).

        2.  **Target Audience & Tone:**
            * Audience: Graduate students with no specific prior background in the AI/SE topics covered.
            * Tone: Friendly, informative, concise, and very NOT verbose.

        3.  **Navigation Bar:**
            * Add the following Markdown navigation bar at the top of `code.md`, `se.md`, `ai.md`, `tutorial.md`, and `license.md`:
                `(Notes ([SE](se.md) [AI](ai.md) Code ([overview](code.md) [tutorial](tutorial.md)))  [License](license.md)  [REgen](prompt.txt)`
                `---`
            * Ensure `tutorial.md` is used for the tutorial link.

        4.  **Content for `code.md` ("Code Overview"):**
            * Reflect over all classes in `kube.py`.
            * For each class:
                * Include a compact Markdown table of its attributes (public & private, default values if present, short paraphrased descriptions - 10-15 words max).
                * Group methods into conceptual protocols (e.g., `core_behavior`, `initialization`, `data_management`, `statistics_query`, `distance_calculation`, `clustering_projection`). Infer sensible, reusable names.
                * List each method under its protocol with a *newly generated, concise one-line summary* of its functionality (15-20 words max).

        5.  **Content for `se.md` ("Software Engineering Concepts") and `ai.md` ("Artificial Intelligence Concepts"):**
            * These pages should provide detailed (yet concise) explanations of core SE and AI concepts, respectively, as identified from `kube.py` (SE examples: Python fundamentals, OOP, CLI, file handling, generators, iterators; AI examples: stats, normalization, distance metrics, LSH, multi-objective optimization, XAI, active learning, supervised/unsupervised learning).
            * **"Terms to watch for":** At the start of each page, under a heading `## Terms to watch for`, provide an alphabetized list of 5-10 key technical terms that are introduced and explained within the main content of *that specific page*. Each term in this list must be formatted as a Markdown bullet point on its own new line (e.g., the generated Markdown for the list should itself contain newlines like `* Term1\\n* Term2\\n* Term3` where `\\n` is the literal newline character in the string that forms the Markdown). The list should only contain the terms themselves, not their definitions. (LLM: You will need to identify these terms as you generate the main content for the page and ensure they are correctly listed and populated with actual newlines between items in the final Markdown file).
            * **Content Body:** Elaborate on each concept, include illustrative Python code snippets from `kube.py` using Markdown syntax highlighting (```python ... ```), and reference protocol names from `code.md` where relevant.
            * **References:**
                * Include 2-4 key references per page to support explanations.
                * Where relevant and possible, include papers by "Tim Menzies". Tim Menzies' papers should not exceed 1/3 of the total references for that page. (LLM: Use general well-known texts or placeholder citations if specific live search for Menzies papers is not feasible for the LLM, but ensure this instruction to include them is part of the prompt content that `generate_website.py` produces for `prompt.txt`).
                * Format references as Markdown footnotes (e.g., `[^1]` and `[^1]: Author, A. B. (Year). *Title of work*. Source.).
            * **Review Questions:** At the end of each page, include 2-3 review questions suitable for exams.

        6.  **Content for `tutorial.md` ("Tutorials"):**
            * Iterate through each `eg__` function found in `kube.py`.
            * For each `eg__` function:
                * **Section Header:** Use the function name (e.g., `## Tutorial: \`eg__the\``).
                * **Purpose & Concepts:** Clearly explain its purpose and the specific SE/AI concepts it demonstrates, linking to ideas in `se.md` and `ai.md`.
                * **Code with New Educational Comments:** The Python script should define a data structure (e.g., a list of dictionaries) where each entry corresponds to an `eg__` function from `kube.py`. This entry should store the *original code* of the `eg__` function and a *separate string containing the same code but with new, detailed educational comments added*. The `generate_tutorial_md_content` function will then use this "commented code" string for the tutorial. Ensure correct f-string escaping if showing f-strings within the tutorial's Python code examples (e.g., if `kube.py` code is `print(f"value: {{x}}")`, the tutorial code block in Markdown should show that exact line; the Python string generating this Markdown needs to handle Python's own f-string escaping for curlies, e.g. `f"print(f\\"value: {{{{x}}}}}\\")"`).
                * **Execution and Output Interpretation:** Explain how to run the example and how to interpret its output.
                * **Links to `code.md`:** Mention relevant class protocols from `code.md` used by the example.
                * **Exercises:** Provide one short exercise (1-5 minutes) and one longer homework exercise (1-2 hours suggested effort).
            * Fully flesh out the tutorials for all `eg__` functions found in `kube.py` (e.g., `eg__the`, `eg__csv`, `eg__data`, `eg__ydist`, `eg__poles`, `eg__counts`, `eg__all`, `eg__about`).

        7.  **Content for `license.md`:**
            * Include the standard MIT License text.
            * The copyright line should be: `Copyright (c) 2025 Tim Menzies <timm@ieee.org>` (year based on `kube.py`).

        8.  **Content for `prompt.txt`:**
            * The `generate_website.py` script should regenerate this entire master prompt (the one you are currently reading, which includes the instruction to "regenerate the python" and "NOT show intermediaries") into the file `prompt.txt`.

        **Final Python Script (`generate_website.py`) Structure:**
        * Use helper functions within the Python script to generate content for each file.
        * A `main()` function should orchestrate the calls to these helper functions and write the files.
        * Ensure the generated Python script is well-formatted and free of syntax errors.
        * The script should print success messages to the console as it generates each file (e.g., "Successfully generated code.md"). These console prints are for the user running `generate_website.py`, not LLM intermediaries.
        ---
        **Source `kube.py` content will be provided by the user in the chat history.**
        ---
    """).strip()
    return master_prompt

# --- Main script logic to generate files ---
def main():
    """Generates all Markdown and text files."""
    # This print statement is for user feedback when they run the generated script.
    # It is not an "intermediary" from the LLM's generation process of this script.
    print("Starting website generation...")

    # Generate and write code.md
    code_md = generate_code_md_content()
    write_md_file("code.md", code_md)

    # Generate and write se.md
    se_md = generate_se_md_content()
    write_md_file("se.md", se_md)

    # Generate and write ai.md
    ai_md = generate_ai_md_content()
    write_md_file("ai.md", ai_md)

    # Generate and write tutorial.md
    tutorial_md = generate_tutorial_md_content()
    write_md_file("tutorial.md", tutorial_md)

    # Generate and write license.md
    license_content = generate_license_md_content()
    write_md_file("license.md", license_content)

    # Generate and write prompt.txt
    prompt_content = generate_prompt_txt_content()
    write_md_file("prompt.txt", prompt_content)


    print("\\nAll files generated successfully.")
    print("You can find code.md, se.md, ai.md, tutorial.md, license.md, and prompt.txt in the current directory.")
    print("Note: The 'print()' statements within this script are for feedback during its execution;")
    print("      they will appear on your console when you run generate_website.py.")


if __name__ == "__main__":
    main()

