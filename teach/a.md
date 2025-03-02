# Answer to Question 1

## a) Definition

- **Process**: A systematic series of actions directed to some end in software development. 
  Example: Agile Scrum, which involves iterative development with regular sprints and daily stand-ups.

- **Resource**: Assets available for software development. 
  Example: Development team members, including programmers, designers, and testers.

- **Product**: The output or deliverable of the software development process. 
  Example: A mobile banking application that allows users to check balances and transfer funds.

## b) Comparison and Prioritization

Processes like Agile focus on flexibility and rapid iteration, while Waterfall emphasizes thorough planning and sequential phases. In-house teams offer more control and cultural alignment, while outsourced teams can provide specialized skills and cost savings.

For a large-scale ERP system, process is more critical. ERPs are complex, involve multiple stakeholders, and require extensive integration. An effective process ensures proper requirement gathering, manages scope creep, and facilitates necessary communication and coordination. While skilled resources are important, even the best team can fail without a suitable process for such a complex project.

## c) Design

### Process Framework:

- Adopt a Lean Startup approach with Agile elements
- 2-week sprints with MVP focus
- Regular user feedback integration

### Resource Allocation:

- Small cross-functional team (4-6 members)
- Utilize cloud services for infrastructure
- Outsource non-core functions (e.g., UI design)

### Product Milestones:

1. Month 2: MVP with basic tracking features
2. Month 4: Social features and data analysis
3. Month 6: Final release with advanced features

This approach balances rapid development (Lean) with structured iterations (Agile) to meet the tight timeline. Resource allocation focuses on core competencies while leveraging external services to reduce costs. The product roadmap prioritizes essential features for early release, allowing for user feedback and iterative improvement within the budget and time constraints.


# Answer to Question 3: Requirements Engineering

**a) Definition**

- **Functional Requirement**: Specifies a function that the system must perform.
  Example: The e-commerce website shall allow users to add items to a shopping cart.

- **Non-Functional Requirement**: Defines system attributes such as performance, security, or usability.
  Example: The e-commerce website shall load product pages within 2 seconds under normal traffic conditions.

- **Requirements Elicitation**: The process of gathering requirements from stakeholders.
  Example: Conducting user interviews to understand customer preferences for product filtering options on the e-commerce website.

**b) Comparison and Prioritization**

Interviews:

- Advantages: In-depth insights, ability to probe and clarify, captures non-verbal cues
- Disadvantages: Time-consuming, potential for interviewer bias, limited sample size

Surveys:

- Advantages: Reaches larger audience, quantifiable data, cost-effective
- Disadvantages: Limited depth, potential for misinterpretation, lower response rates

For developing a new feature in an existing social media platform, I would prioritize surveys. Reasons:

1. Social media platforms have large, diverse user bases, making broad data collection crucial.
2. Quantifiable data helps prioritize feature aspects based on user preferences.
3. Cost-effective for gathering input from a statistically significant sample size.
4. Can quickly validate initial feature ideas before deeper exploration.

** c) Design**

Requirements Specification for Smart Home Automation System:

1. Functional Requirements:
   - The system shall allow users to set schedules for lighting in each room.
   - The system shall adjust home temperature based on user-defined preferences.
   - The system shall send real-time alerts to the user's mobile device upon detecting security breaches.
   - The system shall integrate with voice assistants for hands-free control.
   - The system shall provide energy usage reports on a weekly basis.

2. Non-Functional Requirements:
   - Performance: The system shall respond to user commands within 1 second.
   - Usability: The mobile app interface shall follow material design guidelines for intuitive navigation.
   - Security: The system shall use end-to-end encryption for all data transmissions.

3. User Story:
   As a homeowner, I want to create an "Away" routine that turns off all lights, sets the thermostat to eco mode, and activates the security system, so that I can ensure energy efficiency and home security with a single command when leaving the house.

This specification ensures clarity for developers by:
- Using precise, actionable language in functional requirements.
- Specifying measurable criteria in non-functional requirements.
- Providing a concrete user scenario to illustrate system usage.
- Covering key smart home functionalities (lighting, temperature, security) with specific, implementable features.


## Answer 3 Modeling

***a) Definition of Encapsulation**

Encapsulation is the practice of bundling related data and methods into a single unit, typically a class in object-oriented programming[1][7]. It involves combining data (attributes) and the methods that operate on that data into a structured unit, while restricting direct access to some of the object's components[11].

**b) Contrast between Encapsulation and Information Hiding**

While often used interchangeably, encapsulation and information hiding have subtle differences:

- Encapsulation focuses on bundling data and methods together, providing a public interface for interaction[1][7].
- Information hiding emphasizes concealing implementation details and protecting internal data from external access[2][8].

Encapsulation can be seen as the technique used to achieve information hiding. Information hiding is the principle, while encapsulation is the mechanism to implement it[2].

**c) When to use Encapsulation vs. Pipes and Filters**

Use Encapsulation when:
- Designing object-oriented systems with closely related data and behaviors
- Protecting data integrity within a single unit of functionality
- Creating abstraction layers in your software design

Use Pipes and Filters when:
- Processing streams of data through a series of independent steps
- Building systems that require flexible composition of processing units
- Designing systems where individual components need to be easily replaceable or reusable[3][9]

** d) Encapsulation in the Banking Application Example**

The `Account` class example demonstrates encapsulation by:

- Hiding the `balance` variable from direct external access
- Providing controlled methods (`deposit` and `withdraw`) to interact with the balance

This encapsulation protects the integrity of the bank's account data by:

- Preventing unauthorized direct modifications to the balance
- Allowing for validation (e.g., overdraft checks) before balance changes

However, it can potentially hurt the software by:

- Increasing complexity and overhead for simple operations
- Potentially limiting flexibility for future changes or extensions to account functionality
- Creating a performance bottleneck if the methods are frequently called in high-volume transactions

Overall, the benefits of data protection and integrity usually outweigh these potential drawbacks in sensitive financial applications.

## Ansqer4

# Answer to Question 6: Data Wrangling

## a) Definition

- **Data Cleaning**: The process of identifying and correcting errors, inconsistencies, or inaccuracies in datasets. This may involve removing duplicates, handling missing values, or standardizing formats.

- **Data Transformation**: The process of converting data from one format or structure to another. This can include operations like aggregation, normalization, or changing data types to make the data more suitable for analysis or processing.

- **Piping**: A technique in command-line operations where the output of one command is used as the input for the next command. This allows for the creation of powerful data processing workflows by chaining multiple commands together.

## b) Comparison and Prioritization

**grep**:

- Specialized for pattern matching and searching
- Efficient for finding lines that match specific patterns
- Simple syntax for basic searches
- Supports powerful regular expressions

**awk**:

- More versatile text processing tool
- Can perform complex data manipulations and calculations
- Allows for structured processing of columnar data
- Can handle more complex logic and transformations

For searching through a large log file with complex patterns, I would prioritize **grep**. Reasons:

1. grep is optimized for pattern matching and searching, which is crucial for log file analysis
2. It's generally faster than awk for simple search operations on large files
3. grep's regular expression support is powerful enough for most complex pattern matching needs
4. Its output can be easily piped to other commands for further processing if needed

While awk is more versatile, grep's specialization in pattern matching makes it more suitable and efficient for this specific task.

## c) Design

Command-line script to process the student CSV file:

```bash
gawk -F ',' '$3 > 90 {print $1 ":" $3}' students.csv | sort
```

Explanation of each step:

1. Extract lines for students with grades above 90:
   - Using awk with field separator ',' (`-F ','`)
   - Condition `$3 > 90` checks if the third field (grade) is above 90

2. Transform output to show only name and grade:
   - awk command `{print $1 ":" $3}` prints the first field (name), a colon, and the third field (grade)

3. Sort output alphabetically:
   - Pipe the awk output to the `sort` command

This script demonstrates efficient data wrangling:
- It cleans the data by filtering out irrelevant records (grades <= 90)
- It transforms the data by extracting and reformatting specific fields
- It uses piping to connect the data extraction and sorting operations

The script is concise and efficient, processing the data in a single pass through the file. It combines all required operations into a single pipeline, showcasing the power of command-line data wrangling tools.


## Question 5: make

# Answer to Question 7: Makefiles and Scripting

## a) Definition

- **Target**: A file or action that the Makefile aims to create or execute. It's typically the left side of a rule in a Makefile.
- **Dependency**: A file or target that must exist or be up-to-date before the current target can be built or executed. Dependencies are listed after the colon in a Makefile rule.
- **Command**: The action(s) to be executed to build the target. Commands are indented and executed when their associated target needs to be updated.

## b) Comparison and Prioritization

Makefiles:
- Pros: Efficient for managing complex dependencies, automatic incremental builds, parallel execution
- Cons: Syntax can be cryptic, primarily designed for file-based operations

Shell Scripts:
- Pros: More flexible, can handle a wider range of tasks, easier to write complex logic
- Cons: Don't handle dependencies automatically, may rerun unnecessary steps

Prefer Makefiles when:
- Managing projects with complex file dependencies
- Needing efficient incremental builds
- Working in a multi-developer environment where build consistency is crucial

Prefer Shell Scripts when:
- Automating a series of commands that aren't primarily file-based
- Needing more complex control flow or system interactions
- Creating portable scripts that don't require Make to be installed

## c) Debug and Fix

Here's the corrected Makefile with explanations of the bugs:

```makefile
MD_FILES := intro.md features.md conclusion.md
HTML_FILES := $(MD_FILES:.md=.html)

all: $(HTML_FILES) index.html

%.html: %.md
	pandoc $ $@

clean:
	rm -f $(HTML_FILES) index.html

.PHONY: all clean help

help:
	@echo "Available targets: all, clean, help"
```

Bugs and fixes:

1. Missing body.md in MD\_files

2. **Incorrect output redirection**: in the panddoc ommand, `$` should be
   `$^$ and we need to control the output with `> $@`.`

3. **Missing dependency for HTML files**: The rule for %.html doesn't specify %.md as a prerequisite.
   Fix: Change `%.html:` to `%.html: %.md`

4. **Incomplete clean rule**: The clean rule doesn't remove 
    `rm -f $(HTML_FILES) index.html`.
   Explanation: This ensures all generated files, including index.html, are removed.

5. **Missing colon after help target**: The help target is not properly defined.
   Fix: Add a colon after `help`

