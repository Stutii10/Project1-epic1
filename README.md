# Project1-Epic6

Job/Internship Posting (Epic 6 core)

1. Post a job/internship with required fields. Persist each posting to JobPostings.dat in a single line record.

2. Job Browsing: List all posted jobs/internships from JobPostings.dat.Display title, employer, location, salary (or NONE), poster username, and description.

3. Connections – Requests and Actions, Send connection requests from profile search results.View pending requests addressed to the logged-in user.Accept or reject each pending request, with status persisted.

4. My Network View: List all accepted connections for the logged-in user. If a profile exists for the connection, show basic profile summary.

To compile and run the project, use the following commands:

```bash
cobc -x -o uid InCollege.cob     
./uid
```
This will compile the COBOL source file `InCollege.cob` and create an executable named `uid`, which you can then run.
Make sure you have a COBOL compiler installed on your system to execute these commands successfully.

PLEASE REMOVE ALL THE .dat file before each testcase.

If you would like to run the test cases all at once, ensure that you are in the test directory in your terminal, and then you can use the following command:

```bash
python3 test.py
```