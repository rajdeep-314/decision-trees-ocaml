# Decision Tree Learning


## Overview

This project is meant to provide a simple command-line interface to train decision trees on data present in a CSV file, with options such as limiting the depth of the tree, among others that I hope to decide soon.

`main.ml` is a simple demo right now. If `sample.csv` is a "valid" CSV file, then executing `dune exec decision_trees sample.csv` should print the following:
- The stored dataset - created from the output of parsing `sample.csv`
- The original dataset with the first attribute removed
- The label value for row 5 (if it exists, otherwise 0)
- Information gain for the first attribute
- A string-representation of the trained decision tree
- The accuracy of the decision tree on the trained data


# To do

- Work on `tree.ml`
    - Add "getter" functions for the tree
        - Minimum depth
        - Maximum depth
        - Number of nodes
- Get started on making a REPL
    - Commands
        - csv csv_name csv_file_name
        - queries queries_name query_file_name
        - train tree_name csv_name
        - apply predictions_name tree_name query_name
        - save predictions_name predictions_file_name
- Proper commenting.
- A proper `README.md` with examples, etc.

