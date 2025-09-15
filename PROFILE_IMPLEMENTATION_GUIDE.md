# COBOL Profile Implementation Guide

## Overview
This document details all changes made to the InCollege COBOL program to implement profile functionality. Each code section is explained in detail to help understand COBOL programming concepts.

## Table of Contents
1. [File Control Section Changes](#file-control-section-changes)
2. [File Section Changes](#file-section-changes)
3. [Working Storage Changes](#working-storage-changes)
4. [Main Program Logic Changes](#main-program-logic-changes)
5. [Menu System Changes](#menu-system-changes)
6. [Profile Management Paragraphs](#profile-management-paragraphs)
7. [Profile Persistence Functions](#profile-persistence-functions)
8. [File Handling Updates](#file-handling-updates)

---

## File Control Section Changes

### Added Profile File Control
```cobol
SELECT PROFILES-FILE
    ASSIGN TO 'Profiles.dat'
    ORGANIZATION IS LINE SEQUENTIAL
    FILE STATUS IS FS-PROF.
```

**Explanation:**
- `SELECT` clause defines a file connector name (`PROFILES-FILE`)
- `ASSIGN TO` specifies the physical file name on disk (`Profiles.dat`)
- `ORGANIZATION IS LINE SEQUENTIAL` means records are stored one per line in sequential order
- `FILE STATUS IS FS-PROF` creates a status variable to check file operation results

**COBOL Concept:** File control entries are required for all files used in a COBOL program. They establish the connection between logical file names used in the program and physical files on the system.

---

## File Section Changes

### Added Profile File Definition
```cobol
FD  PROFILES-FILE.
01  Profile-Line                   PIC X(800).
```

**Explanation:**
- `FD` (File Description) defines the structure of the PROFILES-FILE
- `01 Profile-Line PIC X(800)` creates a single record layout of 800 characters
- `PIC X(800)` means 800 alphanumeric characters (letters, numbers, spaces, special chars)

**COBOL Concept:** The FD section describes the logical record structure. The PIC clause defines the data type and size - `X` means alphanumeric, `(800)` means 800 positions.

---

## Working Storage Changes

### Added File Status Variable
```cobol
01  FS-PROF                        PIC XX VALUE '00'.
01  PROF-EOF                       PIC X VALUE 'N'.
```

**Explanation:**
- `FS-PROF PIC XX` holds 2-character file status codes ('00' = success, '35' = file not found, etc.)
- `PROF-EOF PIC X` is a flag to detect end-of-file condition
- `VALUE` clauses initialize variables

### Added Profile Data Structure
```cobol
01  Current-User-Profile.
    05  Prof-First-Name            PIC X(30).
    05  Prof-Last-Name             PIC X(30).
    05  Prof-University            PIC X(50).
    05  Prof-Major                 PIC X(40).
    05  Prof-Grad-Year             PIC X(4).
    05  Prof-About-Me              PIC X(200).
    05  Prof-Experience.
        10  Prof-Exp-Entry OCCURS 3 TIMES.
            15  Prof-Exp-Title     PIC X(40).
            15  Prof-Exp-Company   PIC X(50).
            15  Prof-Exp-Dates     PIC X(30).
            15  Prof-Exp-Desc      PIC X(150).
    05  Prof-Education.
        10  Prof-Edu-Entry OCCURS 3 TIMES.
            15  Prof-Edu-Degree    PIC X(40).
            15  Prof-Edu-School    PIC X(50).
            15  Prof-Edu-Years     PIC X(20).
```

**Explanation:**
- `01` level is the highest level data item (group item)
- `05` levels are subordinate to `01` (elementary items within the group)
- `10` and `15` levels create further subdivisions
- `OCCURS 3 TIMES` creates an array with 3 elements (indexed 1, 2, 3)
- Each PIC clause specifies the maximum size for that field

**COBOL Concept:** COBOL uses hierarchical level numbers (01, 05, 10, 15, etc.) to show data relationships. Higher numbers are subordinate to lower numbers. OCCURS creates arrays/tables.

### Added Working Variables
```cobol
01  Prof-Entry-Count               PIC 9.
01  Prof-Entry-Index               PIC 9.
01  Prof-Input-Buffer              PIC X(200).
01  Profile-Exists-Flag            PIC X VALUE 'N'.
    88  Profile-Exists                   VALUE 'Y'.
    88  Profile-Not-Exists               VALUE 'N'.
01  Year-Valid-Flag                PIC X VALUE 'N'.
    88  Year-Is-Valid                    VALUE 'Y'.
    88  Year-Is-Invalid                  VALUE 'N'.
```

**Explanation:**
- `PIC 9` means single numeric digit (0-9)
- `88` level items are condition names (boolean flags)
- Condition names provide readable names for specific values
- `SET Profile-Exists TO TRUE` is more readable than `MOVE 'Y' TO Profile-Exists-Flag`

**COBOL Concept:** Level 88 items are condition names that make code more readable and self-documenting. They can only have VALUE clauses, not PIC clauses.

---

## Main Program Logic Changes

### Added Profile Loading After Login
```cobol
*> Load user profile after login
PERFORM Load-User-Profile

*> Main Application Loop
PERFORM UNTIL WS-MENU-SELECTION = "5" OR EOF-IN = "Y"
```

**Explanation:**
- `PERFORM` executes a paragraph (subroutine)
- Changed loop condition from "4" to "5" because we added menu option 4
- Profile is loaded immediately after successful login

### Updated Menu Evaluation
```cobol
WHEN "4"
    MOVE "0" TO WS-MENU-SELECTION
    PERFORM Profile-Loop
WHEN "5"
    MOVE "Logging out... Goodbye!" TO WS-MSG
    PERFORM OUT-MSG
```

**Explanation:**
- Added WHEN "4" case for profile management
- Moved exit option to WHEN "5"
- `MOVE "0"` resets menu selection for the next iteration

---

## Menu System Changes

### Updated Main Menu Display
```cobol
Show-Main-Menu.
    MOVE "===========================" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " Student Manager Menu" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 1. Job/Internship Search" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 2. Find Someone You Know" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 3. Learn a New Skill" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 4. Create/Edit My Profile" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 5. Exit" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "===========================" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "Reading choice from file..." TO WS-MSG
    PERFORM OUT-MSG
    .
```

**Explanation:**
- Added line for option 4 (Create/Edit My Profile)
- Changed Exit from option 4 to option 5
- Each MOVE/PERFORM OUT-MSG pair displays one line

### Added Profile Menu Loop
```cobol
Profile-Loop.
    PERFORM UNTIL WS-MENU-SELECTION = "6" OR EOF-IN = "Y"
        PERFORM Profile-Menu
        PERFORM READ-NEXT-INPUT
        IF EOF-IN NOT = "Y"
            MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
            MOVE SPACES TO WS-MSG
            STRING "Selected: " DELIMITED BY SIZE
                   WS-MENU-SELECTION DELIMITED BY SPACE
                   INTO WS-MSG
            PERFORM OUT-MSG
            EVALUATE WS-MENU-SELECTION
                WHEN "1"
                    PERFORM Edit-Basic-Info
                WHEN "2"
                    PERFORM Edit-Experience
                WHEN "3"
                    PERFORM Edit-Education
                WHEN "4"
                    PERFORM View-Profile
                WHEN "5"
                    PERFORM Save-Profile
                WHEN "6"
                    MOVE "Returning..." TO WS-MSG
                    PERFORM OUT-MSG
                WHEN OTHER
                    CONTINUE
                    MOVE SPACES TO WS-MSG
                    STRING "Invalid choice: " DELIMITED BY SIZE
                           WS-MENU-SELECTION DELIMITED BY SPACE
                           INTO WS-MSG
                    PERFORM OUT-MSG
            END-EVALUATE
        END-IF
    END-PERFORM
    .
```

**Explanation:**
- Similar structure to other menu loops in the program
- `PERFORM UNTIL` creates a loop that continues until condition is met
- `STRING...INTO` concatenates multiple strings into one variable
- `EVALUATE` is like a switch/case statement
- `WHEN OTHER` handles invalid selections

### Added Profile Menu Display
```cobol
Profile-Menu.
    MOVE "===========================" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " Profile Management" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 1. Edit Basic Information" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 2. Edit Experience" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 3. Edit Education" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 4. View My Profile" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 5. Save Profile" TO WS-MSG
    PERFORM OUT-MSG
    MOVE " 6. Return" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "===========================" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "Reading choice from file..." TO WS-MSG
    PERFORM OUT-MSG
    .
```

**Explanation:**
- Follows same pattern as other menu displays
- Each option corresponds to a WHEN clause in Profile-Loop
- Period (.) marks the end of the paragraph

---

## Profile Management Paragraphs

### Edit Basic Information
```cobol
Edit-Basic-Info.
    MOVE "=== Basic Information ===" TO WS-MSG
    PERFORM OUT-MSG

    MOVE "Enter First Name (required): " TO WS-MSG
    PERFORM OUT-MSG
    PERFORM READ-NEXT-INPUT
    MOVE FUNCTION TRIM(InLine) TO Prof-First-Name

    MOVE "Enter Last Name (required): " TO WS-MSG
    PERFORM OUT-MSG
    PERFORM READ-NEXT-INPUT
    MOVE FUNCTION TRIM(InLine) TO Prof-Last-Name

    MOVE "Enter University/College (required): " TO WS-MSG
    PERFORM OUT-MSG
    PERFORM READ-NEXT-INPUT
    MOVE FUNCTION TRIM(InLine) TO Prof-University

    MOVE "Enter Major (required): " TO WS-MSG
    PERFORM OUT-MSG
    PERFORM READ-NEXT-INPUT
    MOVE FUNCTION TRIM(InLine) TO Prof-Major

    PERFORM UNTIL Year-Is-Valid
        MOVE "Enter Graduation Year (4 digits, e.g. 2025): " TO WS-MSG
        PERFORM OUT-MSG
        PERFORM READ-NEXT-INPUT
        MOVE FUNCTION TRIM(InLine) TO Prof-Grad-Year
        PERFORM Validate-Graduation-Year
        IF Year-Is-Invalid
            MOVE "Invalid year. Please enter a 4-digit year." TO WS-MSG
            PERFORM OUT-MSG
        END-IF
    END-PERFORM

    MOVE "Enter About Me (optional, press enter to skip): " TO WS-MSG
    PERFORM OUT-MSG
    PERFORM READ-NEXT-INPUT
    MOVE FUNCTION TRIM(InLine) TO Prof-About-Me

    MOVE "Basic information updated." TO WS-MSG
    PERFORM OUT-MSG
    .
```

**Explanation:**
- `FUNCTION TRIM` removes leading/trailing spaces from input
- `PERFORM UNTIL Year-Is-Valid` loops until valid year is entered
- Validation is performed by calling Validate-Graduation-Year paragraph
- IF statement provides user feedback for invalid input

**COBOL Concept:** FUNCTION TRIM is an intrinsic function. PERFORM UNTIL creates a loop that continues while the condition is false.

### Edit Experience Section
```cobol
Edit-Experience.
    MOVE "=== Experience (up to 3 entries) ===" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "How many experience entries to add (1-3)? " TO WS-MSG
    PERFORM OUT-MSG
    PERFORM READ-NEXT-INPUT
    MOVE FUNCTION NUMVAL(InLine) TO Prof-Entry-Count

    IF Prof-Entry-Count > 3
        MOVE 3 TO Prof-Entry-Count
    END-IF
    IF Prof-Entry-Count < 1
        MOVE 1 TO Prof-Entry-Count
    END-IF

    PERFORM VARYING Prof-Entry-Index FROM 1 BY 1
        UNTIL Prof-Entry-Index > Prof-Entry-Count
        MOVE SPACES TO WS-MSG
        STRING "=== Experience Entry " DELIMITED BY SIZE
               Prof-Entry-Index DELIMITED BY SIZE
               " ===" DELIMITED BY SIZE
               INTO WS-MSG
        PERFORM OUT-MSG

        MOVE "Enter Job Title: " TO WS-MSG
        PERFORM OUT-MSG
        PERFORM READ-NEXT-INPUT
        MOVE FUNCTION TRIM(InLine) TO 
            Prof-Exp-Title(Prof-Entry-Index)

        MOVE "Enter Company/Organization: " TO WS-MSG
        PERFORM OUT-MSG
        PERFORM READ-NEXT-INPUT
        MOVE FUNCTION TRIM(InLine) TO 
            Prof-Exp-Company(Prof-Entry-Index)

        MOVE "Enter Dates (e.g. Summer 2024): " TO WS-MSG
        PERFORM OUT-MSG
        PERFORM READ-NEXT-INPUT
        MOVE FUNCTION TRIM(InLine) TO 
            Prof-Exp-Dates(Prof-Entry-Index)

        MOVE "Enter Description (optional): " TO WS-MSG
        PERFORM OUT-MSG
        PERFORM READ-NEXT-INPUT
        MOVE FUNCTION TRIM(InLine) TO 
            Prof-Exp-Desc(Prof-Entry-Index)
    END-PERFORM

    MOVE "Experience information updated." TO WS-MSG
    PERFORM OUT-MSG
    .
```

**Explanation:**
- `FUNCTION NUMVAL` converts text to numeric value
- IF statements enforce business rules (1-3 entries maximum)
- `PERFORM VARYING` creates a controlled loop with an index variable
- `Prof-Exp-Title(Prof-Entry-Index)` accesses array elements using the index
- Loop executes once for each experience entry the user wants to add

**COBOL Concept:** PERFORM VARYING is like a for-loop. The index automatically increments BY 1 each iteration FROM 1 UNTIL the condition is met.

### Edit Education Section
```cobol
Edit-Education.
    MOVE "=== Education (up to 3 entries) ===" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "How many education entries to add (1-3)? " TO WS-MSG
    PERFORM OUT-MSG
    PERFORM READ-NEXT-INPUT
    MOVE FUNCTION NUMVAL(InLine) TO Prof-Entry-Count

    IF Prof-Entry-Count > 3
        MOVE 3 TO Prof-Entry-Count
    END-IF
    IF Prof-Entry-Count < 1
        MOVE 1 TO Prof-Entry-Count
    END-IF

    PERFORM VARYING Prof-Entry-Index FROM 1 BY 1
        UNTIL Prof-Entry-Index > Prof-Entry-Count
        MOVE SPACES TO WS-MSG
        STRING "=== Education Entry " DELIMITED BY SIZE
               Prof-Entry-Index DELIMITED BY SIZE
               " ===" DELIMITED BY SIZE
               INTO WS-MSG
        PERFORM OUT-MSG

        MOVE "Enter Degree: " TO WS-MSG
        PERFORM OUT-MSG
        PERFORM READ-NEXT-INPUT
        MOVE FUNCTION TRIM(InLine) TO 
            Prof-Edu-Degree(Prof-Entry-Index)

        MOVE "Enter University/College: " TO WS-MSG
        PERFORM OUT-MSG
        PERFORM READ-NEXT-INPUT
        MOVE FUNCTION TRIM(InLine) TO 
            Prof-Edu-School(Prof-Entry-Index)

        MOVE "Enter Years Attended (e.g. 2023-2025): " TO WS-MSG
        PERFORM OUT-MSG
        PERFORM READ-NEXT-INPUT
        MOVE FUNCTION TRIM(InLine) TO 
            Prof-Edu-Years(Prof-Entry-Index)
    END-PERFORM

    MOVE "Education information updated." TO WS-MSG
    PERFORM OUT-MSG
    .
```

**Explanation:**
- Similar structure to Edit-Experience
- Uses the same looping and validation patterns
- Accesses education array elements: Prof-Edu-Degree(index), etc.

### View Profile Display
```cobol
View-Profile.
    MOVE "=== MY PROFILE ===" TO WS-MSG
    PERFORM OUT-MSG
    MOVE SPACES TO WS-MSG
    STRING "Name: " DELIMITED BY SIZE
           FUNCTION TRIM(Prof-First-Name) DELIMITED BY SIZE
           " " DELIMITED BY SIZE
           FUNCTION TRIM(Prof-Last-Name) DELIMITED BY SIZE
           INTO WS-MSG
    PERFORM OUT-MSG
    MOVE SPACES TO WS-MSG
    STRING "University: " DELIMITED BY SIZE
           FUNCTION TRIM(Prof-University) DELIMITED BY SIZE
           INTO WS-MSG
    PERFORM OUT-MSG
    MOVE SPACES TO WS-MSG
    STRING "Major: " DELIMITED BY SIZE
           FUNCTION TRIM(Prof-Major) DELIMITED BY SIZE
           INTO WS-MSG
    PERFORM OUT-MSG
    MOVE SPACES TO WS-MSG
    STRING "Graduation Year: " DELIMITED BY SIZE
           FUNCTION TRIM(Prof-Grad-Year) DELIMITED BY SIZE
           INTO WS-MSG
    PERFORM OUT-MSG
    IF Prof-About-Me NOT = SPACES
        MOVE SPACES TO WS-MSG
        STRING "About Me: " DELIMITED BY SIZE
               FUNCTION TRIM(Prof-About-Me) DELIMITED BY SIZE
               INTO WS-MSG
        PERFORM OUT-MSG
    END-IF
    MOVE "Profile displayed." TO WS-MSG
    PERFORM OUT-MSG
    .
```

**Explanation:**
- `MOVE SPACES` clears the message buffer before building new content
- Multiple STRING operations concatenate labels with data values
- `IF Prof-About-Me NOT = SPACES` only displays About Me section if it has content
- Each STRING/PERFORM OUT-MSG pair displays one formatted line

### Save Profile Function
```cobol
Save-Profile.
    PERFORM Save-User-Profile
    MOVE "Profile saved successfully." TO WS-MSG
    PERFORM OUT-MSG
    .
```

**Explanation:**
- Simple wrapper that calls the actual save function
- Provides user feedback after saving

---

## Profile Persistence Functions

### Year Validation
```cobol
Validate-Graduation-Year.
    SET Year-Is-Invalid TO TRUE
    IF FUNCTION LENGTH(FUNCTION TRIM(Prof-Grad-Year)) = 4
        IF Prof-Grad-Year IS NUMERIC
            SET Year-Is-Valid TO TRUE
        END-IF
    END-IF
    .
```

**Explanation:**
- `SET Year-Is-Invalid TO TRUE` assumes invalid until proven otherwise
- `FUNCTION LENGTH` returns the number of characters
- `IS NUMERIC` tests if all characters are digits 0-9
- Nested IF statements check both length and numeric content

**COBOL Concept:** SET with condition names is preferred over MOVE for boolean flags. IS NUMERIC is a class test.

### Load User Profile
```cobol
Load-User-Profile.
    CLOSE PROFILES-FILE
    OPEN INPUT PROFILES-FILE
    IF FS-PROF = "35"
        CLOSE PROFILES-FILE
        OPEN OUTPUT PROFILES-FILE
        CLOSE PROFILES-FILE
        OPEN INPUT PROFILES-FILE
    END-IF
    MOVE 'N' TO PROF-EOF
    SET Profile-Not-Exists TO TRUE

    PERFORM UNTIL PROF-EOF = 'Y' OR Profile-Exists
        READ PROFILES-FILE
            AT END
                MOVE 'Y' TO PROF-EOF
            NOT AT END
                IF Profile-Line(1:20) = UserName
                    SET Profile-Exists TO TRUE
                    PERFORM Parse-Profile-Line
                END-IF
        END-READ
    END-PERFORM

    CLOSE PROFILES-FILE
    OPEN EXTEND PROFILES-FILE
    .
```

**Explanation:**
- File status "35" means file doesn't exist, so create it
- `OPEN INPUT` for reading, `OPEN OUTPUT` for writing (overwrites), `OPEN EXTEND` for appending
- `Profile-Line(1:20)` extracts characters 1-20 (username portion)
- `AT END` clause executes when no more records to read
- `NOT AT END` executes for each successful read
- Loop continues until EOF or matching profile found

**COBOL Concept:** Reference modification (1:20) extracts substrings. AT END/NOT AT END are READ statement clauses.

### Parse Profile Line
```cobol
Parse-Profile-Line.
    MOVE Profile-Line(21:30) TO Prof-First-Name
    MOVE Profile-Line(51:30) TO Prof-Last-Name
    MOVE Profile-Line(81:50) TO Prof-University
    MOVE Profile-Line(131:40) TO Prof-Major
    MOVE Profile-Line(171:4) TO Prof-Grad-Year
    MOVE Profile-Line(175:200) TO Prof-About-Me
    .
```

**Explanation:**
- Extracts data from specific positions in the file record
- Position format: (start:length)
- Profile-Line(21:30) = characters 21-50 (30 characters)
- Each field has a predetermined position and length in the file

**COBOL Concept:** Fixed-length record parsing using reference modification. This is a common technique for reading structured data files.

### Save User Profile
```cobol
Save-User-Profile.
    CLOSE PROFILES-FILE
    OPEN OUTPUT PROFILES-FILE

    MOVE SPACES TO Profile-Line
    STRING
        UserName                    DELIMITED BY SIZE
        Prof-First-Name             DELIMITED BY SIZE
        Prof-Last-Name              DELIMITED BY SIZE
        Prof-University             DELIMITED BY SIZE
        Prof-Major                  DELIMITED BY SIZE
        Prof-Grad-Year              DELIMITED BY SIZE
        Prof-About-Me               DELIMITED BY SIZE
        Prof-Exp-Title(1)           DELIMITED BY SIZE
        Prof-Exp-Company(1)         DELIMITED BY SIZE
        Prof-Exp-Dates(1)           DELIMITED BY SIZE
        Prof-Exp-Desc(1)            DELIMITED BY SIZE
        Prof-Exp-Title(2)           DELIMITED BY SIZE
        Prof-Exp-Company(2)         DELIMITED BY SIZE
        Prof-Exp-Dates(2)           DELIMITED BY SIZE
        Prof-Exp-Desc(2)            DELIMITED BY SIZE
        Prof-Exp-Title(3)           DELIMITED BY SIZE
        Prof-Exp-Company(3)         DELIMITED BY SIZE
        Prof-Exp-Dates(3)           DELIMITED BY SIZE
        Prof-Exp-Desc(3)            DELIMITED BY SIZE
        Prof-Edu-Degree(1)          DELIMITED BY SIZE
        Prof-Edu-School(1)          DELIMITED BY SIZE
        Prof-Edu-Years(1)           DELIMITED BY SIZE
        Prof-Edu-Degree(2)          DELIMITED BY SIZE
        Prof-Edu-School(2)          DELIMITED BY SIZE
        Prof-Edu-Years(2)           DELIMITED BY SIZE
        Prof-Edu-Degree(3)          DELIMITED BY SIZE
        Prof-Edu-School(3)          DELIMITED BY SIZE
        Prof-Edu-Years(3)           DELIMITED BY SIZE
        INTO Profile-Line
    END-STRING
    WRITE Profile-Line

    CLOSE PROFILES-FILE
    OPEN EXTEND PROFILES-FILE
    .
```

**Explanation:**
- `OPEN OUTPUT` overwrites the entire file (simplified approach)
- `MOVE SPACES` initializes the output buffer
- `STRING...INTO` concatenates all profile fields into one record
- `DELIMITED BY SIZE` uses the full field length (includes trailing spaces)
- `WRITE` outputs the complete record to the file
- Reopens in EXTEND mode for future operations

**COBOL Concept:** STRING statement concatenates multiple fields. DELIMITED BY SIZE preserves field boundaries by using full field lengths.

---

## File Handling Updates

### Updated Open-Files Procedure
```cobol
*> Try to open profiles for INPUT; if missing, create it.
OPEN INPUT  PROFILES-FILE
IF FS-PROF = "35"
    OPEN OUTPUT PROFILES-FILE
    CLOSE PROFILES-FILE
    OPEN INPUT PROFILES-FILE
END-IF
CLOSE PROFILES-FILE

*> Reopen profiles in EXTEND for appends after startup load
OPEN EXTEND PROFILES-FILE
```

**Explanation:**
- Attempts to open profile file for reading
- If file doesn't exist (status "35"), creates it with OUTPUT mode
- Closes and reopens in INPUT mode to verify it exists
- Finally opens in EXTEND mode for normal operations

### Updated Close-Files Procedure
```cobol
Close-Files.
    CLOSE INPUT-FILE
    CLOSE OUTPUT-FILE
    CLOSE PROFILES-FILE
    .
```

**Explanation:**
- Added CLOSE for the new profiles file
- Ensures all files are properly closed when program terminates

---

## COBOL Programming Concepts Demonstrated

### 1. File Handling
- **Sequential File Organization**: Records stored one after another
- **File Status Codes**: Monitor file operation success/failure
- **File Modes**: INPUT (read), OUTPUT (write/overwrite), EXTEND (append)

### 2. Data Structures
- **Group Items**: 01-level items containing subordinate items
- **Elementary Items**: Lowest level items with PIC clauses
- **Arrays**: OCCURS clause creates repeating data structures
- **Condition Names**: 88-level items for readable boolean logic

### 3. Control Structures
- **PERFORM**: Execute paragraphs (subroutines)
- **PERFORM UNTIL**: Loop while condition is false
- **PERFORM VARYING**: Controlled loop with automatic indexing
- **EVALUATE**: Multi-way branching (like switch/case)
- **IF/THEN/ELSE**: Conditional execution

### 4. String Handling
- **MOVE**: Simple data transfer
- **STRING**: Concatenate multiple fields
- **FUNCTION TRIM**: Remove leading/trailing spaces
- **Reference Modification**: Extract substrings (start:length)

### 5. Data Validation
- **IS NUMERIC**: Test for numeric content
- **FUNCTION LENGTH**: Get string length
- **Condition Names**: Readable flag testing

### 6. Program Structure
- **Paragraphs**: Named code blocks ending with period
- **Sections**: Groups of related paragraphs
- **Modular Design**: Breaking complex tasks into smaller pieces

---

## Study Tips for COBOL Exam

1. **Remember the hierarchy**: 01 > 05 > 10 > 15 levels
2. **File operations always need**: SELECT, FD, OPEN, READ/WRITE, CLOSE
3. **PERFORM is COBOL's main control structure** - know all its forms
4. **PIC clauses define data types**: X=alphanumeric, 9=numeric, A=alphabetic
5. **Condition names (88-level) make code readable** - use SET, not MOVE
6. **STRING builds output, UNSTRING parses input**
7. **Always handle AT END condition when reading files**
8. **Reference modification (start:length) extracts substrings**
9. **File status codes**: 00=success, 35=file not found, 10=end of file
10. **COBOL is very verbose but self-documenting** - read it like English

## Common COBOL Patterns Used

1. **File Processing Loop**:
   ```cobol
   PERFORM UNTIL EOF-FLAG = 'Y'
       READ FILE-NAME
           AT END MOVE 'Y' TO EOF-FLAG
           NOT AT END PROCESS-RECORD
       END-READ
   END-PERFORM
   ```

2. **Menu Processing**:
   ```cobol
   PERFORM UNTIL CHOICE = EXIT-VALUE
       DISPLAY MENU
       ACCEPT CHOICE
       EVALUATE CHOICE
           WHEN "1" PERFORM OPTION-1
           WHEN "2" PERFORM OPTION-2
           WHEN OTHER DISPLAY ERROR-MESSAGE
       END-EVALUATE
   END-PERFORM
   ```

3. **Array Processing**:
   ```cobol
   PERFORM VARYING INDEX FROM 1 BY 1
       UNTIL INDEX > MAX-ENTRIES
       PROCESS-ARRAY-ELEMENT(INDEX)
   END-PERFORM
   ```

4. **Data Validation**:
   ```cobol
   PERFORM UNTIL DATA-IS-VALID
       ACCEPT INPUT-FIELD
       VALIDATE INPUT-FIELD
       IF DATA-IS-INVALID
           DISPLAY ERROR-MESSAGE
       END-IF
   END-PERFORM
   ```

This implementation demonstrates real-world COBOL programming techniques used in business applications for data management, user interfaces, and file processing.