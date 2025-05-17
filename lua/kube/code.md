[SE Concepts](se.md) | [AI Concepts](ai.md) | [Code Overview](code.md) | [Tutorials](tutorial.md) | [License](license.md) | [Generation Prompt](prompt.txt)
---

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
| `has`     | `{}`          | Dictionary storing frequency counts of each unique symbol. |

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

*(Note: Attribute descriptions and protocol summaries are inferred from `kube.py` for brevity and educational clarity.)*\n