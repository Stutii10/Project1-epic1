# Project1-Epic7

Core features implemented in this epic:

Job/Internship Posting

Post a job with required fields.

Each posting is persisted to JobPostings.dat as a single-line record.

Job Browsing

List all posted jobs/internships from JobPostings.dat.

Display: title, employer, location, salary (or NONE), poster username, and description.

Connections — Requests & Actions

From profile search results: send connection requests.

View pending requests addressed to the logged-in user.

Accept/Reject each pending request; status is persisted.

My Network View

List all accepted connections for the logged-in user.

If a profile exists for that connection, show a basic profile summary (name, university, major).
To compile and run the project, use the following commands:

```bash
cobc -x -o uid InCollege.cob     
./uid
```
This will compile the COBOL source file `InCollege.cob` and create an executable named `uid`, which you can then run.
Make sure you have a COBOL compiler installed on your system to execute these commands successfully.

PLEASE REMOVE ALL THE .dat file before each testcase.
