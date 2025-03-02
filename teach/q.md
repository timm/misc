## Question 1: Process, Resource, and Product in Software Engineering

**a) Definition**

Define the following terms with specific examples:

- **Process** (e.g., Agile Scrum)
- **Resource** (e.g., development team members)
- **Product** (e.g., a mobile banking application)

**b) Comparison and Prioritization**

Compare and contrast the roles of **process** (Agile vs. Waterfall) and **resource** (in-house vs. outsourced teams) in developing a large-scale ERP system. Prioritize which is more critical for project success and explain why.

**c) Design**

Design a development approach for a small startup creating a fitness tracking app. Include:

- A high-level **process framework**
- Key **resource allocation strategies**
- **Product milestones**

Explain how your approach balances these aspects to meet a 6-month time-to-market goal with a limited budget.

## Question 2: Requirements Engineering

**a) Definition**

Define the following terms and provide a specific example for each from an e-commerce website:

- **Functional Requirement**
- **Non-Functional Requirement**
- **Requirements Elicitation**

**b) Comparison and Prioritization**

Compare the advantages and disadvantages of using **interviews** versus **surveys** for requirements gathering. Which method would you prioritize for developing a new feature in an existing social media platform, and why?

**c) Design**

You are developing requirements for a smart home automation system that controls lighting, temperature, and security. Design a requirements specification document that includes:

1. A list of 5 specific functional requirements using the format: "The system shall [action]"
2. 3 non-functional requirements addressing performance, usability, and security
3. A user story for a homeowner setting up a new automation routine

Explain how your requirements specification ensures clarity for developers while addressing key smart home functionalities.


## Question 3: Modeling

a. Define Encapsulation.  
b. Contrast Encapsulation with Information Hiding.  
c. Explain when to use Encapsulation vs. Pipes and Filters.  
d. A banking application has an `Account` class that stores a user’s balance
          Instead of making `balance` a public variable, the class provides methods `deposit(amount)` and 
          `withdraw(amount)`, which check for overdrafts before modifying `balance`.  
  Does  ths encapsulation protect the integrity of the bank’s account data?  
  How can it hurt the software?

# Question 4: Data Wrangling

## a) Definition

Define the following terms in the context of data wrangling:

- **Data Cleaning**
- **Data Transformation**
- **Piping**

## b) Comparison and Prioritization

Compare and contrast the use of **`grep`** and **`awk`** for text manipulation. Which tool would you prioritize for searching through a large log file with complex patterns, and why?

## c) Design
You are given a CSV file with columns for 

      studentNames, IDs, grades

Design a command-line script using some combination of  `grep`, `awk` to:

1. Extract the lines for students with a grade above 90.
2. Transform the output to only show the student's name and grade, separated by a colon.
3. Sort the output alphabetically by student name.


## Question 5: Makefiles and Scripting

## a) Definition
Define the following terms related to Makefiles:
- **Target**
- **Dependency**
- **Command**

## b) Comparison and Prioritization
Compare and contrast the use of Makefiles versus shell scripts for automating software builds. In what situations would you prefer a Makefile, and in what situations would a shell script be more appropriate? Explain your reasoning.

## c) Debug and Fix
The following Makefile is intended to convert and intro,body and conclusions Markdown files to
HTML and combine them into a single index.html file. However, it
contains 5 subtle bugs. Identify and fix these bugs, explaining
each correction.

```makefile
MD_FILES = intro.md conclusion.md
HTML_FILES = $(MD_FILES:.md=.html) # switch the suffix md to html

all: $(HTML_FILES) index.html

%.html %.md:
    pandoc $ $@

clean:
    rm -f index.html

.PHONY: all clean

help
    @echo "Available targets: all, clean, help"
```

Explain how each bug affects the Makefile's functionality and how your fixes improve the build process.
