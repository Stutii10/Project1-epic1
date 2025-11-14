# InCollege - Professional Networking Application

**Sprint:** Week 10 - System Enhancement & Bug Fixing - Final

## Overview
InCollege is a console-based professional networking application written in COBOL. It allows users to create accounts, build profiles, connect with other professionals, search for jobs, and send messages.

## Features
- User registration and authentication
- Profile creation and management (education, experience)
- User search by name
- Connection requests (send, accept, reject)
- Job posting and browsing
- Job application tracking
- Messaging system (connected users only)
- Skills learning modules

## System Requirements
- GnuCOBOL compiler (version 3.0 or higher)
- Linux/Unix environment (tested on Ubuntu 24)
- Terminal/Console access

## Installation

### 1. Install GnuCOBOL
```bash
sudo apt-get update
sudo apt-get install gnucobol
```

### 2. Clone/Download the Project
```bash
# If using git
git clone ...
cd InCollege

# Or download and extract the ZIP file
```

### 3. Verify Installation
```bash
cobc --version
# Should show GnuCOBOL version 3.x or higher
```

## Compilation

Compile the program using the following command:
```bash
cobc -x -free InCollege.cob -o InCollege
```

**Compilation flags explained:**
- `-x` : Build executable program
- `-free` : Use free-format COBOL (no column restrictions)
- `-o InCollege` : Output file name

## Running the Application

### Method 1: Interactive Mode (Manual Input)
```bash
./InCollege
```
Then enter inputs manually when prompted.

### Method 2: With Input File (Recommended for Testing)
```bash
./InCollege < InCollege-Input.txt
```

This reads all inputs from the input file automatically.

## File Structure

### Input Files
- **InCollege-Input.txt** - Contains all user inputs for testing
  - Format: One input per line
  - Each line corresponds to one prompt in the program

### Output Files
- **InCollege-Output.txt** - Contains exact copy of screen output
  - Created automatically when program runs
  - Used for testing and verification

### Data Files (Auto-created)
- **Accounts.dat** - Stores user accounts (username|password)
- **Profiles.dat** - Stores user profiles (fixed-width format, 2000 chars per line)
- **Connections.dat** - Stores connection requests (sender|recipient|status)
- **JobPostings.dat** - Stores job postings (poster|title|description|employer|location|salary)
- **Applications.dat** - Stores job applications (username|jobID)
- **Messages.dat** - Stores messages (sender|recipient|message)

## Creating Test Input Files

### Example Input File Format
```
1                     # Choice: Login
testuser1             # Username
Password1!            # Password
1                     # Main menu: Create/Edit Profile
1                     # Profile: Edit Basic Info
John                  # First name
Doe                   # Last name
University of Test    # University
Computer Science      # Major
2025                  # Graduation year
I am a test user      # About me
4                     # Profile: Save
5                     # Profile: Return
9                     # Main menu: Log out
0                     # Exit program
```

### Input File Guidelines
1. One input per line
2. Empty lines are treated as empty input (may cause issues)
3. Make sure you have enough inputs for all prompts
4. Test both valid and invalid inputs

## Sample Test Scenarios

### Test 1: Complete User Registration and Profile
```
2                     # Create account
newuser1              # Username
Password1!            # Password
1                     # Login
newuser1              # Username
Password1!            # Password
1                     # Create/Edit Profile
[... profile inputs ...]
9                     # Log out
0                     # Exit
```

### Test 2: Job Posting and Application
```
1                     # Login
employer1             # Username
Password1!            # Password
3                     # Job search
1                     # Post job
Software Engineer     # Title
Build cool apps       # Description
TechCorp             # Employer
San Francisco        # Location
120000               # Salary
4                     # Back to main menu
9                     # Log out
1                     # Login
applicant1           # Username
Password1!           # Password
3                     # Job search
2                     # Browse jobs
1                     # Select job 1
1                     # Apply
[... continue ...]
```

### Test 3: Connections and Messaging
```
1                     # Login
user1                 # Username
Password1!            # Password
4                     # Find someone
John Doe              # Search name
1                     # Send connection request
2                     # Back
9                     # Log out
1                     # Login as John Doe
johndoe               # Username
Password1!            # Password
5                     # View pending requests
1                     # Accept
7                     # Messages
1                     # Send message
user1                 # Recipient
Hello!                # Message
[... continue ...]
```

## Output Verification

The output file (`InCollege-Output.txt`) should contain:
1. All menu displays
2. All prompts
3. All confirmation messages
4. All error messages
5. All displayed data (profiles, jobs, messages, etc.)

**IMPORTANT:** The output file should be IDENTICAL to what appears on the screen.

## Known Limitations

1. Maximum 5 user accounts
2. Maximum 50 connections per system
3. Maximum 99 job postings
4. Maximum 99 applications
5. Profile data is fixed-width format (not easily human-readable in raw form)
6. No password recovery mechanism
7. Manual file deletion may cause data inconsistencies

## Troubleshooting

### Problem: "Error opening input file"
**Solution:** Create an `InCollege-Input.txt` file in the same directory as the executable.

### Problem: "Error opening output file"
**Solution:** Check file permissions. The program needs write access to create `InCollege-Output.txt`.

### Problem: Program hangs/infinite loop
**Solution:** Ensure your input file has enough lines for all prompts. Add "0" at the end to exit.

### Problem: Compilation errors
**Solution:** 
- Ensure you're using `-free` flag for free-format COBOL
- Check GnuCOBOL version (need 3.0+)
- Verify no syntax errors in the source code

### Problem: Data files corrupted
**Solution:** Delete all `.dat` files and restart. The program will recreate them.
```bash
rm *.dat
./InCollege < InCollege-Input.txt
```

## Testing Best Practices

1. **Always backup data files** before testing
2. **Use descriptive test input files** (e.g., `test-login.txt`, `test-jobs.txt`)
3. **Compare output files** to verify correctness
4. **Test edge cases:**
   - Empty inputs
   - Maximum limits (5 accounts, 50 connections)
   - Invalid inputs
   - Special characters in passwords
5. **Test all user flows** from Week 1-9

## Development Workflow

1. Make code changes in `InCollege.cob`
2. Compile: `cobc -x -free InCollege.cob -o InCollege`
3. Create test input file
4. Run: `./InCollege < test-input.txt`
5. Verify output in `InCollege-Output.txt`
6. Commit changes with descriptive message

## Git Commit Message Format
```
[JIRA-TICKET]: Brief description

Example:
SCRUM-299: Added global exit functionality to all menus
SCRUM-314: Fixed profile duplication bug
BUG-001: Resolved infinite loop in registration
```


## Version History

- **Week 10** - Bug fixes, enhanced validation, global exit, documentation
- **Week 9** - Messaging system
- **Week 8** - Job applications and tracking
- **Week 7** - Job posting and browsing
- **Week 6** - Connections and networking
- **Week 5** - User search and profiles
- **Week 1-4** - Authentication and basic features
---

