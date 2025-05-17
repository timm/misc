(Notes ([SE](se.md) [AI](ai.md) Code ([overview](code.md) [tutorial](tutorial.md)))  [License](license.md)  [REgen](prompt.txt)
---
# `kube.py` Tutorials: Hands-on Examples

This section provides tutorials for the `eg__` example functions in `kube.py`. Each tutorial explains the example's purpose, shows its code with educational comments, and offers practical exercises designed for graduate students new to these specific AI/SE topics. The tone is friendly, informative, and concise.

---
## Tutorial: `eg__the`

**Purpose & Concepts:**
Print the configuration.
This example demonstrates or relates to: SE: Runtime Configuration Management, Global State (`the` object), CLI argument reflection.

**Code with New Educational Comments:**
```python
def eg__the(_: Any) -> None: 
  """Print the configuration."""
  # 'the' is a global object (instance of class 'o').
  # It holds all runtime configuration options for the script.
  # These options are initially parsed from the script's main docstring (see Options section)
  # and can be overridden by command-line arguments processed by the cli() function.
  print(the) 
  # The 'o' class's __repr__ method uses the global 'cat()' function for its string representation,
  # so this will neatly print all key-value pairs currently stored in 'the'.
```

**Execution and Output Interpretation:**
* **To Run**: `python kube.py -the`
Try also: `python kube.py -the -r 12345 -f mydata.csv` (even if mydata.csv doesn't exist, to see config change).
* **Output Interpretation**: The output displays all current configuration settings (e.g., `bins`, `file`, `rseed`, `P`, `dims`, `some`). This is useful for verifying which parameters the script is using for a particular run, confirming defaults, or seeing the effect of CLI overrides.

**Links to `code.md` Protocols (Conceptual):**
This example may utilize concepts related to: Uses the `o.core_behavior` protocol (specifically `__repr__`).

**Exercises:**
* **Short Exercise (1-5 minutes)**: Run `python kube.py -the -s 50`. Observe the 'some' attribute in the output. How does it differ from running without `-s 50` (check the default in `kube.py`'s main docstring)?
* **Longer Homework Exercise (1-2 hours suggested effort)**: Locate where the `the` object is initialized near the end of `kube.py`. Add a new default option to the script's main docstring (e.g., `-N newOpt = "hello"`). Modify the regular expression or parsing logic if necessary (though it might pick it up automatically if formatted like other options). Rerun `eg__the` to see if your new option and its default value are displayed.

---
## Tutorial: `eg__csv`

**Purpose & Concepts:**
Print csv data.
This example demonstrates or relates to: SE: File I/O, Data Type Coercion, Generators, List Comprehensions.

**Code with New Educational Comments:**
```python
def eg__csv(_: Any) -> None:
  """Print csv data."""
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
```

**Execution and Output Interpretation:**
* **To Run**: `python kube.py -csv` (uses the default file in `the.file`, e.g., `../../moot/optimize/misc/auto93.csv`).
To use a different file: `python kube.py -f YOUR_CSV_FILE.csv -csv`
* **Output Interpretation**: Each row from the specified CSV file is printed to the console. Values within each row will be displayed with their Python types (e.g., numbers as `int` or `float`, booleans as `True`/`False`). This helps verify that the data loading and type coercion mechanisms are working correctly.

**Links to `code.md` Protocols (Conceptual):**
This example may utilize concepts related to: Primarily uses the global `csv()` and `coerce()` functions.

**Exercises:**
* **Short Exercise (1-5 minutes)**: Create a very small CSV file (e.g., 2 rows, 3 columns with mixed data types like `name,age,active` then `Alice,30,True`). Run `eg__csv` with this file. Does it print the data as you expect, with correct types?
* **Longer Homework Exercise (1-2 hours suggested effort)**: Modify the `eg__csv` function in `kube.py`. Instead of printing every row, make it print only the first 5 rows of the CSV file. Test your modification.

---
## Tutorial: `eg__data`

**Purpose & Concepts:**
Print column information.
This example demonstrates or relates to: SE: Object-Oriented Programming (`Data`, `Sym`, `Num` instantiation and interaction), Data Encapsulation. AI: Data Representation, Feature Engineering (column identification), Initial Data Analysis.

**Code with New Educational Comments:**
```python
def eg__data(_: Any) -> None:
  """Print column information."""
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
  #    The 'print("x", col)' relies on the __repr__ method of each column object
  #    (defined in Sym and Num, using the global 'cat()' function) to display its details.
  print("Independent (x) Columns:")
  for col_obj in d.cols.x: 
      print(f"  {{{{col_obj}}}}") # Correctly escaped for f-string to output {col_obj}

  # 4. Print information about dependent ('y') columns (objectives):
  #    'd.cols.y' lists columns identified as dependent variables (targets/objectives).
  print("\nDependent (y) Columns (Objectives):")
  for col_obj in d.cols.y:
      print(f"  {{{{col_obj}}}}") # Correctly escaped
```

**Execution and Output Interpretation:**
* **To Run**: `python kube.py -data` (uses the CSV file specified by `the.file`, e.g., `auto93.csv`)
* **Output Interpretation**: The output is divided into two sections:
1. 'Independent (x) Columns': Lists each feature column, showing its type (`Sym` or `Num`), name, creation index (`at`), and key statistics (e.g., for `Num`: count `n`, mean `mu`, std dev `div`, `lo`, `hi`; for `Sym`: count `n`, mode `mid`, entropy `div`).
2. 'Dependent (y) Columns': Lists objective columns similarly.
This output is crucial for verifying that data is loaded correctly and that columns are categorized and summarized as expected by `kube.py`'s rules.

**Links to `code.md` Protocols (Conceptual):**
This example may utilize concepts related to: `Data.initialization_loading` (covers `__init__`, `about`, `add`), `Sym.statistics_query`, `Num.statistics_query`, `Sym.representation` (via `cat`), `Num.representation` (via `cat`).

**Exercises:**
* **Short Exercise (1-5 minutes)**: Examine the `auto93.csv` file. Identify one column you expect to be `Sym` and one `Num`. Run `eg__data`. Do the printed statistics for these columns (e.g., `mid`/`div` for Sym, `mu`/`div` for Num) make sense based on a quick look at the data?
* **Longer Homework Exercise (1-2 hours suggested effort)**: In a copy of your `auto93.csv` data file, change a column header that is currently an 'x' variable (e.g., 'Cylinders') so that its name ends with a '+' (e.g., 'Cylinders+'). Rerun `eg__data`. Describe how the output changes for this column. Which list ('x' or 'y') does it appear in now and why (refer to `Data.about` logic if needed)?

---
## Tutorial: `eg__ydist`

**Purpose & Concepts:**
Print rows sorted by distance to heaven.
This example demonstrates or relates to: AI: Multi-Objective Evaluation, Fitness Ranking. SE: Sorting, Lambda functions.

**Code with New Educational Comments:**
```python
def eg__ydist(_: Any) -> None:
  """Print rows sorted by distance to heaven."""
  # 1. Load and prepare data using the Data class
  d = Data(csv(the.file))
  # 2. Sort rows:
  #    d._rows contains all data instances.
  #    The key for sorting is d.ydist(row), which calculates how "good" a row is
  #    against multiple objectives (see ai.md and Data.evaluation_statistics in code.md).
  #    Lower ydist is better.
  lst = sorted(d._rows, key=lambda row: d.ydist(row))
  # 3. Print the best 4 rows
  print("Best 4 rows (closest to 'heaven'):")
  for row in lst[:4]: print("good", row)
  # 4. Print the worst 4 rows
  print("\nWorst 4 rows (furthest from 'heaven'):")
  for row in lst[-4:]: print("bad", row)
```

**Execution and Output Interpretation:**
* **To Run**: `python kube.py -ydist`
* **Output Interpretation**: Prints the 4 'best' rows (lowest `ydist`) and 4 'worst' rows (highest `ydist`) from the dataset. This shows which data instances are closest to and furthest from the defined optimization goals.

**Links to `code.md` Protocols (Conceptual):**
This example may utilize concepts related to: `Data.evaluation_statistics` (specifically `ydist`), `Data.initialization_loading`.

**Exercises:**
* **Short Exercise (1-5 minutes)**: Observe the 'good' rows. Do they generally have values in the objective columns (Y-columns from `eg__data`) that you'd expect for 'good' performance (e.g., low values for '-' goals, high for '+' goals)?
* **Longer Homework Exercise (1-2 hours suggested effort)**: Modify `eg__ydist` to also print the `ydist` score for each of the good and bad rows shown.

---
## Tutorial: `eg__poles`

**Purpose & Concepts:**
Show clustering dimensions.
This example demonstrates or relates to: AI: Clustering, LSH, Feature Engineering (poles as dimensions).

**Code with New Educational Comments:**
```python
def eg__poles(file: str = None) -> None:
  """Show clustering dimensions."""
  # 1. Load data
  d = Data(csv(file or the.file))
  # 2. Find poles:
  #    d.poles() selects a few diverse rows ('poles') from the dataset
  #    that are used to define dimensions for projection in LSH.
  p = d.poles()
  # 3. Perform LSH:
  #    d.lsh(p) projects all rows onto these pole-defined dimensions
  #    and groups them into clusters. 'dims' here is a dictionary
  #    where keys are projection tuples (cluster IDs) and values are Data objects (clusters).
  dims = d.lsh(p)
  # 4. Print cluster IDs (projection tuples)
  print("Cluster IDs (projection tuples):")
  [print(k) for k in dims]
  # 5. Print total number of clusters found
  print("\nTotal number of clusters:", len(dims))
```

**Execution and Output Interpretation:**
* **To Run**: `python kube.py -poles`
* **Output Interpretation**: Prints the 'cluster IDs' which are tuples representing the binned projections of data onto pole-defined dimensions. Also prints the total number of unique clusters (unique projection tuples) found.

**Links to `code.md` Protocols (Conceptual):**
This example may utilize concepts related to: `Data.clustering_projection` (covers `poles`, `lsh`, `project`).

**Exercises:**
* **Short Exercise (1-5 minutes)**: Run `eg__poles` multiple times with different random seeds (e.g., `-r 1 -poles`, then `-r 2 -poles`). Does the number of clusters or the cluster IDs change? Why might this be?
* **Longer Homework Exercise (1-2 hours suggested effort)**: The `eg__poles` example currently only prints cluster keys. Modify it to iterate through `dims.values()` (which are `Data` objects representing each cluster) and print the number of rows in each cluster (e.g., `len(cluster_data._rows)`).

---
## Tutorial: `eg__counts`

**Purpose & Concepts:**
Show cluster counts and stats.
This example demonstrates or relates to: AI: Clustering Evaluation, Cluster Characterization.

**Code with New Educational Comments:**
```python
def eg__counts(file: str = None) -> None:
  """Show cluster counts and stats."""
  # 1. Load data
  d = Data(csv(file or the.file))
  # 2. Perform LSH clustering (find poles, then hash)
  clusters = d.lsh(d.poles())
  # 3. Iterate through each cluster found
  print("Cluster Statistics (Mid ydist, Div ydist, Num Rows):")
  for data_cluster in clusters.values(): # data_cluster is a Data object for each cluster
    # 4. Calculate ydist statistics for rows within this cluster
    ys = data_cluster.ydists() # Num object with stats of ydist values for this cluster
    # 5. Print stats if cluster is large enough
    #    d.minPts() defines the minimum number of rows for a cluster to be considered.
    if len(data_cluster._rows) >= d.minPts():
      # Prints the mean ydist, std dev of ydist, and number of rows for this cluster.
      # Using an f-string for clarity as 'o' might not be defined in this generated script's global scope.
      # Note: Assuming ys.n is a method call as in original kube.py (ys.n() -> ys.n)
      print(f"{{'mid': {ys.mid():.3g}, 'div': {ys.div():.3g}, 'n': {ys.n}}}")
```

**Execution and Output Interpretation:**
* **To Run**: `python kube.py -counts`
* **Output Interpretation**: For each 'significant' cluster (meeting `minPts` criteria), it prints statistics about the `ydist` (objective fitness) values of its members: `mid` (mean ydist), `div` (std dev of ydist), and `n` (number of rows in that cluster). This helps assess the quality and homogeneity of clusters in terms of the optimization goals.

**Links to `code.md` Protocols (Conceptual):**
This example may utilize concepts related to: `Data.clustering_projection`, `Data.evaluation_statistics` (covers `ydists`, `minPts`).

**Exercises:**
* **Short Exercise (1-5 minutes)**: Look at the output. Are there clusters with low 'mid' ydist values? What does this suggest about the rows in those clusters?
* **Longer Homework Exercise (1-2 hours suggested effort)**: Modify `eg__counts` to also print the actual 'mid' row (using `data_cluster.mid()`) for each significant cluster, in addition to its ydist statistics. You'll need to handle the printing of the row object nicely.

---
## Tutorial: `eg__all`

**Purpose & Concepts:**
Run all examples.
This example demonstrates or relates to: SE: Scripting, Automation, Test Orchestration (simple form).

**Code with New Educational Comments:**
```python
def eg__all(_: Any) -> None:
  """Run all examples.""" 
  # This example function from kube.py iterates through a predefined list of other eg__ functions
  # (eg__the, eg__csv, eg__data, eg__ydist, eg__poles, eg__counts) and executes them.
  # It's a way to run a batch of tests or demonstrations.
  # Note: the random seed is reset before each called function in the original kube.py
  # to ensure reproducibility.
  
  # For this educational script, we describe its intent rather than trying to replicate
  # its exact execution environment here, as direct calls to other eg__ functions
  # depend on them being in the same runnable scope (like in the original kube.py).
  print("Intended behavior of eg__all:")
  print("  - Resets random.seed(the.rseed)")
  print("  - Calls eg__the(_)")
  print("  - Resets random.seed(the.rseed)")
  print("  - Calls eg__csv(_)")
  print("  - ...and so on for eg__data, eg__ydist, eg__poles, eg__counts.")
  print("To see this in action, run 'python kube.py -all' using the original kube.py script.")
```

**Execution and Output Interpretation:**
* **To Run**: `python kube.py -all`
* **Output Interpretation**: Runs several other `eg__` functions sequentially (as listed in its definition in `kube.py`). The output will be a concatenation of the outputs from each of these individual examples.

**Links to `code.md` Protocols (Conceptual):**
This example may utilize concepts related to: Calls other `eg__` functions from `kube.py`.

**Exercises:**
* **Short Exercise (1-5 minutes)**: If you run `kube.py -all` with the original script, can you identify where the output of one example ends and the next begins?
* **Longer Homework Exercise (1-2 hours suggested effort)**: In the original `kube.py` script, modify the list of functions called by `eg__all` to exclude one example (e.g., remove `eg__csv`). Rerun `python kube.py -all` and verify that the output of the removed example is missing.

---
## Tutorial: `eg__about`

**Purpose & Concepts:**
Provide detailed documentation about the script module itself.
This example demonstrates or relates to: SE: Automated Documentation Generation (using `pydoc`), Introspection.

**Code with New Educational Comments:**
```python
def eg__about(_): 
  """Provide detailed documentation about the script module itself."""
  # This example from kube.py typically uses Python's built-in 'pydoc' module
  # to generate and print extensive documentation about the 'kube.py' script itself.
  # It would extract information from docstrings of the module, classes, and functions.
  
  # For this educational script, we describe its intent:
  print("Intended behavior of eg__about:")
  print("  - Imports pydoc and sys modules.")
  print("  - Calls 'print(pydoc.render_doc(sys.modules[__name__]))'")
  print("  - This would display comprehensive, pydoc-formatted help text for kube.py.")
  print("To see this in action, ensure pydoc can run in your environment and execute 'python kube.py -about' using the original kube.py script.")
```

**Execution and Output Interpretation:**
* **To Run**: `python kube.py -about`
* **Output Interpretation**: Displays comprehensive documentation for the entire `kube.py` module, including its main docstring, class definitions (with methods and their docstrings), and function definitions (with their docstrings), all formatted by `pydoc`.

**Links to `code.md` Protocols (Conceptual):**
This example may utilize concepts related to: Uses Python's standard `pydoc` and `sys` modules.

**Exercises:**
* **Short Exercise (1-5 minutes)**: If you run `kube.py -about` with the original script, can you find the documentation for the `Data` class within the output?
* **Longer Homework Exercise (1-2 hours suggested effort)**: In the original `kube.py`, add a new, simple function with a clear docstring. Rerun `python kube.py -about`. Verify that your new function and its docstring appear in the generated documentation.

---\n