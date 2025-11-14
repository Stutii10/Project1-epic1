*> =======================================================
*> InCollege - Week 7: Browse Jobs & Applications with Report
*> Inputs  : InCollege-Input.txt
*> Outputs : InCollege-Output.txt
*> Accounts: Accounts.dat (username|password per line)
*> Profiles: Profiles.dat (user profile data)
*> Connections: Connections.dat (sender|recipient|status per line)
*> Job Postings: JobPostings.dat (poster|title|description|employer|location|salary)
*> Applications: Applications.dat (username|jobID per line)
*> =======================================================

 IDENTIFICATION DIVISION.
 PROGRAM-ID. InCollege.

 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
     SELECT INPUT-FILE
         ASSIGN TO 'InCollege-Input.txt'
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-FILE-STATUS.
     SELECT OUTPUT-FILE
         ASSIGN TO 'InCollege-Output.txt'
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS WS-OUTPUT-STATUS.
     SELECT ACCOUNTS-FILE
         ASSIGN TO 'Accounts.dat'
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS FS-ACCT.
     SELECT PROFILES-FILE
         ASSIGN TO 'Profiles.dat'
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS FS-PROF.
     SELECT CONNECTIONS-FILE
         ASSIGN TO 'Connections.dat'
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS FS-CONN.
     SELECT JOBS-FILE
         ASSIGN TO 'JobPostings.dat'
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS FS-JOBS.
     SELECT APPLICATIONS-FILE
         ASSIGN TO 'Applications.dat'
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS FS-APPS.
     SELECT MESSAGES-FILE
         ASSIGN TO 'Messages.dat'
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS FS-MSGS.

 DATA DIVISION.
 FILE SECTION.
 FD  INPUT-FILE.
 01  InLine                         PIC X(200).

 FD  OUTPUT-FILE.
 01  OutLine                        PIC X(200).

 FD  ACCOUNTS-FILE.
 01  Account-Line                   PIC X(60).

 FD  PROFILES-FILE.
 01  Profile-Line                   PIC X(2000).

 FD  CONNECTIONS-FILE.
 01  Connection-Line                PIC X(60).

 FD  JOBS-FILE.
 01  Job-Line                       PIC X(400).

 FD  APPLICATIONS-FILE.
 01  Application-Line               PIC X(80).

 FD  MESSAGES-FILE.
 01  Message-Line                   PIC X(500).

 WORKING-STORAGE SECTION.
 01 Target-Username                 PIC X(20).

 *> ------- File status variables -------
 01  WS-FILE-STATUS                 PIC XX.
 01  WS-OUTPUT-STATUS               PIC XX.
 01  FS-ACCT                        PIC XX VALUE '00'.
 01  FS-PROF                        PIC XX VALUE '00'.
 01  FS-CONN                        PIC XX VALUE '00'.
 01  FS-JOBS                        PIC XX VALUE '00'.
 01  FS-APPS                        PIC XX VALUE '00'.
 01  FS-MSGS                        PIC XX VALUE '00'.
 01  EOF-IN                         PIC X VALUE 'N'.
 01  ACCT-EOF                       PIC X VALUE 'N'.
 01  PROF-EOF                       PIC X VALUE 'N'.
 01  CONN-EOF                       PIC X VALUE 'N'.
 01  JOB-EOF                        PIC X VALUE 'N'.
 01  APP-EOF                        PIC X VALUE 'N'.
 01  MSG-EOF                        PIC X VALUE 'N'.

 *> ------- Menu / input buffers -------
 01  UserChoice                     PIC 9.
 01  UserName                       PIC X(20).
 01  UserPassword                   PIC X(20).
 01  NameLen                        PIC 99.
 01  PassLen                        PIC 99.
 01  TempChar                       PIC X.
 01  I                              PIC 99.
 01  J                              PIC 99.

 01  WS-MSG                         PIC X(200).

 *> ------- Main menu navigation -------
 01  WS-MENU-SELECTION              PIC X.
 01  WS-SKILL-SELECTION             PIC X.
 01  WS-INPUT-VALUE                 PIC X.
 01  WS-LOGGED-IN                   PIC X VALUE 'N'.
     88  USER-LOGGED-IN                   VALUE 'Y'.
     88  USER-NOT-LOGGED-IN               VALUE 'N'.

 *> ------- In-memory accounts (max 5) -------
 01  Account-Count                  PIC 9 VALUE 0.
 01  Accounts.
     05  Account OCCURS 5 TIMES.
         10  Acc-Username           PIC X(20).
         10  Acc-Password           PIC X(20).

 *> ------- Connection management (with status) -------
 01  Connection-Count               PIC 99 VALUE 0.
 01  Connections.
     05  Connection OCCURS 50 TIMES.
         10  Conn-Sender            PIC X(20).
         10  Conn-Recipient         PIC X(20).
         10  Conn-Status            PIC X.
             *> P = Pending, A = Accepted, R = Rejected

 *> ------- Flags -------
 01  UE-Flag                        PIC 9 VALUE 0.
     88  Username-Exists                  VALUE 1.
     88  Username-Not-Exists              VALUE 0.

 01  VP-Valid-Flag                  PIC 9 VALUE 0.
     88  Pass-Is-Valid                    VALUE 1.
     88  Pass-Is-Invalid                  VALUE 0.
 01  Has-Upper                      PIC 9 VALUE 0.
 01  Has-Digit                      PIC 9 VALUE 0.
 01  Has-Special                    PIC 9 VALUE 0.
 01  CountVar                       PIC 99 VALUE 0.
 01  Has-Entries                    PIC 9 VALUE 0.

 01  Connection-Valid-Flag          PIC X VALUE 'Y'.
     88  Connection-Is-Valid              VALUE 'Y'.
     88  Connection-Is-Invalid            VALUE 'N'.

 *> Special characters set
 01  Specials                       PIC X(40)
     VALUE '!@#$%^&*()-_=+[]{};:'',.<>/?'.

 *> Parse helpers
 01  U-Part                         PIC X(20).
 01  P-Part                         PIC X(20).
 01  S-Part                         PIC X.

 *> ------- Profile data structures -------
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

 *> Profile working variables
 01  Prof-Entry-Count               PIC 9.
 01  Prof-Entry-Index               PIC 9.
 01  Prof-Input-Buffer              PIC X(200).
 01  Profile-Exists-Flag            PIC X VALUE 'N'.
     88  Profile-Exists                   VALUE 'Y'.
     88  Profile-Not-Exists               VALUE 'N'.
 01  Year-Valid-Flag                PIC X VALUE 'N'.
     88  Year-Is-Valid                    VALUE 'Y'.
     88  Year-Is-Invalid                  VALUE 'N'.

 *> Search variables
 01  Search-Name                    PIC X(60).
 01  Search-Username                PIC X(20).
 01  Search-Result-Profile.
     05  Search-First-Name          PIC X(30).
     05  Search-Last-Name           PIC X(30).
     05  Search-University          PIC X(50).
     05  Search-Major               PIC X(40).
     05  Search-Grad-Year           PIC X(4).
     05  Search-About-Me            PIC X(200).
     05  Search-Experience.
         10  Search-Exp-Entry OCCURS 3 TIMES.
             15  Search-Exp-Title   PIC X(40).
             15  Search-Exp-Company PIC X(50).
             15  Search-Exp-Dates   PIC X(30).
             15  Search-Exp-Desc    PIC X(150).
     05  Search-Education.
         10  Search-Edu-Entry OCCURS 3 TIMES.
             15  Search-Edu-Degree  PIC X(40).
             15  Search-Edu-School  PIC X(50).
             15  Search-Edu-Years   PIC X(20).
 01  Search-Found-Flag              PIC X VALUE 'N'.
     88  User-Found                       VALUE 'Y'.
     88  User-Not-Found                   VALUE 'N'.

 *> Temp profile for network display
 01  Temp-Profile.
     05  Temp-First-Name            PIC X(30).
     05  Temp-Last-Name             PIC X(30).
     05  Temp-University            PIC X(50).
     05  Temp-Major                 PIC X(40).

 01  Request-Index                  PIC 99.
 01  Pending-Request-Count          PIC 99 VALUE 0.
 01  Pending-Requests.
     05  Pending-Request OCCURS 50 TIMES.
         10  Pend-Index             PIC 99.
         10  Pend-Sender            PIC X(20).

 *> ------- Job posting buffers -------
 01  Job-Title                      PIC X(60).
 01  Job-Description                PIC X(200).
 01  Job-Employer                   PIC X(60).
 01  Job-Location                   PIC X(60).
 01  Job-Salary                     PIC X(40).

 *> ------- NEW: Job browsing and application structures -------
 01  Job-Count                      PIC 99 VALUE 0.
 01  Job-List.
     05  Job-Entry OCCURS 99 TIMES.
         10  Job-ID                 PIC 99.
         10  Job-Poster             PIC X(20).
         10  Job-Title-Store        PIC X(60).
         10  Job-Description-Store  PIC X(200).
         10  Job-Employer-Store     PIC X(60).
         10  Job-Location-Store     PIC X(60).
         10  Job-Salary-Store       PIC X(40).

 01  Selected-Job-Number            PIC 99.
 01  Job-Details-Choice             PIC X.

 *> Application tracking
 01  Application-Count              PIC 99 VALUE 0.
 01  Applications.
     05  Application OCCURS 99 TIMES.
         10  App-Username           PIC X(20).
         10  App-Job-ID             PIC 99.

 01  Already-Applied-Flag           PIC X VALUE 'N'.
     88  Already-Applied                  VALUE 'Y'.
     88  Not-Applied-Yet                  VALUE 'N'.

 01  User-App-Count                 PIC 99 VALUE 0.

 *> ------- NEW: Messaging variables -------
 01  Message-Recipient              PIC X(20).
 01  Message-Content                PIC X(200).
 01  Message-Valid-Flag             PIC X VALUE 'N'.
     88  Message-Recipient-Valid          VALUE 'Y'.
     88  Message-Recipient-Invalid        VALUE 'N'.

 01  Message-Sender                 PIC X(20).
 01  Message-Text                   PIC X(200).
 01  Messages-Found-Count           PIC 99 VALUE 0.

 PROCEDURE DIVISION.
 Main.
     PERFORM Open-Files
     PERFORM Load-Accounts-From-Disk
     PERFORM Load-Connections-From-Disk
     PERFORM Load-Applications-From-Disk

     *> Main program loop to allow returning to login screen
     PERFORM UNTIL EOF-IN = "Y"
         SET USER-NOT-LOGGED-IN TO TRUE
         MOVE 'N' TO EOF-IN
         *> Authentication Loop
         PERFORM UNTIL USER-LOGGED-IN OR EOF-IN = "Y"
             PERFORM Show-Login-Menu
             PERFORM READ-NEXT-INPUT
             IF InLine = SPACES
                 MOVE '0' TO InLine(1:1)
             END-IF
             MOVE FUNCTION NUMVAL (InLine) TO UserChoice

             EVALUATE UserChoice
                 WHEN 1
                     PERFORM Do-Login
                 WHEN 2
                     PERFORM Do-Registration
                 WHEN OTHER
                     MOVE "Invalid choice. Please try again." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-PERFORM

         *> Skip main app if not logged in due to EOF
         IF USER-NOT-LOGGED-IN
             MOVE "Y" TO EOF-IN
         END-IF

         *> Load user profile after login
         IF USER-LOGGED-IN
             PERFORM Load-User-Profile
         END-IF

         *> Main Application Loop
         PERFORM UNTIL EOF-IN = "Y" OR USER-NOT-LOGGED-IN
             PERFORM Show-Main-Menu
             PERFORM READ-NEXT-INPUT
             IF EOF-IN NOT = "Y"
                 MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
                 EVALUATE WS-MENU-SELECTION
                     WHEN "1"
                         PERFORM Profile-Loop
                     WHEN "2"
                         PERFORM View-Profile
                     WHEN "3"
                         PERFORM J-Search-Loop
                     WHEN "4"
                         PERFORM Find-Someone
                     WHEN "5"
                         PERFORM View-Pending-Requests-With-Actions
                     WHEN "6"
                         PERFORM View-My-Network
                     WHEN "7"
                         PERFORM Messages-Loop
                     WHEN "8"
                         PERFORM Skill-Loop
                     WHEN "9"
                         MOVE "You have logged out." TO WS-MSG
                         PERFORM OUT-MSG
                         SET USER-NOT-LOGGED-IN TO TRUE
                     WHEN OTHER
                         MOVE "Invalid choice." TO WS-MSG
                         PERFORM OUT-MSG
                 END-EVALUATE
             END-IF
         END-PERFORM
     END-PERFORM

     PERFORM Close-Files
     STOP RUN.

 *> -----------------------------
 *> File open/close + output + input
 *> -----------------------------
 Open-Files.
     OPEN INPUT  INPUT-FILE
     IF WS-FILE-STATUS NOT = "00"
         DISPLAY "Error opening input file: " WS-FILE-STATUS
         STOP RUN
     END-IF

     OPEN OUTPUT OUTPUT-FILE
     IF WS-OUTPUT-STATUS NOT = "00"
         DISPLAY "Error opening output file: " WS-OUTPUT-STATUS
         STOP RUN
     END-IF

     *> Try to open accounts for INPUT; if missing, create it.
     OPEN INPUT  ACCOUNTS-FILE
     IF FS-ACCT = "35"
         OPEN OUTPUT ACCOUNTS-FILE
         CLOSE ACCOUNTS-FILE
         OPEN INPUT ACCOUNTS-FILE
     END-IF
     CLOSE ACCOUNTS-FILE
     OPEN EXTEND ACCOUNTS-FILE

     *> Try to open profiles for INPUT; if missing, create it.
     OPEN INPUT  PROFILES-FILE
     IF FS-PROF = "35"
         OPEN OUTPUT PROFILES-FILE
         CLOSE PROFILES-FILE
         OPEN INPUT PROFILES-FILE
     END-IF
     CLOSE PROFILES-FILE
     OPEN EXTEND PROFILES-FILE

     *> Try to open connections for INPUT; if missing, create it.
     OPEN INPUT  CONNECTIONS-FILE
     IF FS-CONN = "35"
         OPEN OUTPUT CONNECTIONS-FILE
         CLOSE CONNECTIONS-FILE
         OPEN INPUT CONNECTIONS-FILE
     END-IF
     CLOSE CONNECTIONS-FILE
     OPEN EXTEND CONNECTIONS-FILE

     *> Try to open jobs file for INPUT; if missing, create it.
     OPEN INPUT  JOBS-FILE
     IF FS-JOBS = "35"
         OPEN OUTPUT JOBS-FILE
         CLOSE JOBS-FILE
         OPEN INPUT JOBS-FILE
     END-IF
     CLOSE JOBS-FILE
     OPEN EXTEND JOBS-FILE

     *> NEW: Try to open applications file for INPUT; if missing, create it.
     OPEN INPUT  APPLICATIONS-FILE
     IF FS-APPS = "35"
         OPEN OUTPUT APPLICATIONS-FILE
         CLOSE APPLICATIONS-FILE
         OPEN INPUT APPLICATIONS-FILE
     END-IF
     CLOSE APPLICATIONS-FILE
     OPEN EXTEND APPLICATIONS-FILE

     *> NEW: Try to open messages file for INPUT; if missing, create it.
     OPEN INPUT  MESSAGES-FILE
     IF FS-MSGS = "35"
         OPEN OUTPUT MESSAGES-FILE
         CLOSE MESSAGES-FILE
         OPEN INPUT MESSAGES-FILE
     END-IF
     CLOSE MESSAGES-FILE
     OPEN EXTEND MESSAGES-FILE
     .

 Close-Files.
     CLOSE INPUT-FILE
     CLOSE OUTPUT-FILE
     CLOSE ACCOUNTS-FILE
     CLOSE PROFILES-FILE
     CLOSE CONNECTIONS-FILE
     CLOSE JOBS-FILE
     CLOSE APPLICATIONS-FILE
     CLOSE MESSAGES-FILE
     .

 *> Display to screen AND write to output file
 OUT-MSG.
     MOVE WS-MSG TO OutLine
     DISPLAY WS-MSG
     WRITE OutLine
     .

 *> Consume one line from input file per prompt
 READ-NEXT-INPUT.
     IF EOF-IN = 'Y'
         MOVE SPACES TO InLine
         EXIT PARAGRAPH
     END-IF
     READ INPUT-FILE
         AT END
             MOVE 'Y' TO EOF-IN
             MOVE "End of file reached" TO WS-MSG
             PERFORM OUT-MSG
         NOT AT END
             MOVE FUNCTION TRIM(InLine) TO WS-INPUT-VALUE
     END-READ
     .

 *> -----------------------------
 *> LOGIN MENU
 *> -----------------------------
 Show-Login-Menu.
     MOVE "Welcome to InCollege!" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "1. Log In" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "2. Create New Account" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "Enter your choice: " TO WS-MSG
     PERFORM OUT-MSG
     .

 *> -----------------------------
 *> PROFILE MANAGEMENT PARAGRAPHS
 *> -----------------------------
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

         MOVE "Enter Years Attended (e.g. 2021-2025): " TO WS-MSG
         PERFORM OUT-MSG
         PERFORM READ-NEXT-INPUT
         MOVE FUNCTION TRIM(InLine) TO
             Prof-Edu-Years(Prof-Entry-Index)
     END-PERFORM

     MOVE "Education information updated." TO WS-MSG
     PERFORM OUT-MSG
     .

 View-Profile.
     MOVE "--- Your Profile ---" TO WS-MSG
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
     PERFORM Display-Experience
     PERFORM Display-Education
     MOVE "--------------------" TO WS-MSG
     PERFORM OUT-MSG
     .

 Display-Experience.
     MOVE 0 TO Has-Entries
     PERFORM VARYING Prof-Entry-Index FROM 1 BY 1 UNTIL Prof-Entry-Index > 3
         IF Prof-Exp-Title(Prof-Entry-Index) NOT = SPACES OR
            Prof-Exp-Company(Prof-Entry-Index) NOT = SPACES OR
            Prof-Exp-Dates(Prof-Entry-Index) NOT = SPACES OR
            Prof-Exp-Desc(Prof-Entry-Index) NOT = SPACES
             ADD 1 TO Has-Entries
         END-IF
     END-PERFORM

     IF Has-Entries = 0
         MOVE "Experience: None" TO WS-MSG
         PERFORM OUT-MSG
     ELSE
         MOVE "Experience:" TO WS-MSG
         PERFORM OUT-MSG
         PERFORM VARYING Prof-Entry-Index FROM 1 BY 1 UNTIL Prof-Entry-Index > 3
             IF Prof-Exp-Title(Prof-Entry-Index) NOT = SPACES OR
                Prof-Exp-Company(Prof-Entry-Index) NOT = SPACES OR
                Prof-Exp-Dates(Prof-Entry-Index) NOT = SPACES OR
                Prof-Exp-Desc(Prof-Entry-Index) NOT = SPACES
                 IF Prof-Exp-Title(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Title: " DELIMITED BY SIZE
                            FUNCTION TRIM(Prof-Exp-Title(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Prof-Exp-Company(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Company: " DELIMITED BY SIZE
                            FUNCTION TRIM(Prof-Exp-Company(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Prof-Exp-Dates(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Dates: " DELIMITED BY SIZE
                            FUNCTION TRIM(Prof-Exp-Dates(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Prof-Exp-Desc(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Description: " DELIMITED BY SIZE
                            FUNCTION TRIM(Prof-Exp-Desc(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
             END-IF
         END-PERFORM
     END-IF
     .

 Display-Education.
     MOVE 0 TO Has-Entries
     PERFORM VARYING Prof-Entry-Index FROM 1 BY 1 UNTIL Prof-Entry-Index > 3
         IF Prof-Edu-Degree(Prof-Entry-Index) NOT = SPACES OR
            Prof-Edu-School(Prof-Entry-Index) NOT = SPACES OR
            Prof-Edu-Years(Prof-Entry-Index) NOT = SPACES
             ADD 1 TO Has-Entries
         END-IF
     END-PERFORM

     IF Has-Entries = 0
         MOVE "Education: None" TO WS-MSG
         PERFORM OUT-MSG
     ELSE
         MOVE "Education:" TO WS-MSG
         PERFORM OUT-MSG
         PERFORM VARYING Prof-Entry-Index FROM 1 BY 1 UNTIL Prof-Entry-Index > 3
             IF Prof-Edu-Degree(Prof-Entry-Index) NOT = SPACES OR
                Prof-Edu-School(Prof-Entry-Index) NOT = SPACES OR
                Prof-Edu-Years(Prof-Entry-Index) NOT = SPACES
                 IF Prof-Edu-Degree(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Degree: " DELIMITED BY SIZE
                            FUNCTION TRIM(Prof-Edu-Degree(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Prof-Edu-School(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  University: " DELIMITED BY SIZE
                            FUNCTION TRIM(Prof-Edu-School(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Prof-Edu-Years(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Years: " DELIMITED BY SIZE
                            FUNCTION TRIM(Prof-Edu-Years(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
             END-IF
         END-PERFORM
     END-IF
     .

 Save-Profile.
     PERFORM Save-User-Profile
     MOVE "Profile saved successfully." TO WS-MSG
     PERFORM OUT-MSG
     .

 Validate-Graduation-Year.
     SET Year-Is-Invalid TO TRUE
     IF FUNCTION LENGTH(FUNCTION TRIM(Prof-Grad-Year)) = 4
         IF Prof-Grad-Year IS NUMERIC
             SET Year-Is-Valid TO TRUE
         END-IF
     END-IF
     .

 *> -----------------------------
 *> PROFILE PERSISTENCE FUNCTIONS
 *> -----------------------------
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

 Parse-Profile-Line.
     MOVE Profile-Line(21:30) TO Prof-First-Name
     MOVE Profile-Line(51:30) TO Prof-Last-Name
     MOVE Profile-Line(81:50) TO Prof-University
     MOVE Profile-Line(131:40) TO Prof-Major
     MOVE Profile-Line(171:4) TO Prof-Grad-Year
     MOVE Profile-Line(175:200) TO Prof-About-Me
     MOVE Profile-Line(375:40) TO Prof-Exp-Title(1)
     MOVE Profile-Line(415:50) TO Prof-Exp-Company(1)
     MOVE Profile-Line(465:30) TO Prof-Exp-Dates(1)
     MOVE Profile-Line(495:150) TO Prof-Exp-Desc(1)
     MOVE Profile-Line(645:40) TO Prof-Exp-Title(2)
     MOVE Profile-Line(685:50) TO Prof-Exp-Company(2)
     MOVE Profile-Line(735:30) TO Prof-Exp-Dates(2)
     MOVE Profile-Line(765:150) TO Prof-Exp-Desc(2)
     MOVE Profile-Line(915:40) TO Prof-Exp-Title(3)
     MOVE Profile-Line(955:50) TO Prof-Exp-Company(3)
     MOVE Profile-Line(1005:30) TO Prof-Exp-Dates(3)
     MOVE Profile-Line(1035:150) TO Prof-Exp-Desc(3)
     MOVE Profile-Line(1185:40) TO Prof-Edu-Degree(1)
     MOVE Profile-Line(1225:50) TO Prof-Edu-School(1)
     MOVE Profile-Line(1275:20) TO Prof-Edu-Years(1)
     MOVE Profile-Line(1295:40) TO Prof-Edu-Degree(2)
     MOVE Profile-Line(1335:50) TO Prof-Edu-School(2)
     MOVE Profile-Line(1385:20) TO Prof-Edu-Years(2)
     MOVE Profile-Line(1405:40) TO Prof-Edu-Degree(3)
     MOVE Profile-Line(1445:50) TO Prof-Edu-School(3)
     MOVE Profile-Line(1495:20) TO Prof-Edu-Years(3)
     .

 Save-User-Profile.
     CLOSE PROFILES-FILE
     OPEN EXTEND PROFILES-FILE

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

 *> -----------------------------
 *> SEARCH FUNCTIONS
 *> -----------------------------
 Perform-Search.
     SET User-Not-Found TO TRUE
     CLOSE PROFILES-FILE
     OPEN INPUT PROFILES-FILE
     MOVE 'N' TO PROF-EOF

     PERFORM UNTIL PROF-EOF = 'Y'
         READ PROFILES-FILE
             AT END
                 MOVE 'Y' TO PROF-EOF
             NOT AT END
                 MOVE Profile-Line(1:20) TO Search-Username
                 MOVE Profile-Line(21:30) TO Search-First-Name
                 MOVE Profile-Line(51:30) TO Search-Last-Name
                 MOVE SPACES TO WS-MSG
                 STRING FUNCTION TRIM(Search-First-Name) DELIMITED BY SIZE
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(Search-Last-Name) DELIMITED BY SIZE
                        INTO WS-MSG
                 IF WS-MSG = Search-Name
                     SET User-Found TO TRUE
                     MOVE Profile-Line(81:50) TO Search-University
                     MOVE Profile-Line(131:40) TO Search-Major
                     MOVE Profile-Line(171:4) TO Search-Grad-Year
                     MOVE Profile-Line(175:200) TO Search-About-Me
                     MOVE Profile-Line(375:40) TO Search-Exp-Title(1)
                     MOVE Profile-Line(415:50) TO Search-Exp-Company(1)
                     MOVE Profile-Line(465:30) TO Search-Exp-Dates(1)
                     MOVE Profile-Line(495:150) TO Search-Exp-Desc(1)
                     MOVE Profile-Line(645:40) TO Search-Exp-Title(2)
                     MOVE Profile-Line(685:50) TO Search-Exp-Company(2)
                     MOVE Profile-Line(735:30) TO Search-Exp-Dates(2)
                     MOVE Profile-Line(765:150) TO Search-Exp-Desc(2)
                     MOVE Profile-Line(915:40) TO Search-Exp-Title(3)
                     MOVE Profile-Line(955:50) TO Search-Exp-Company(3)
                     MOVE Profile-Line(1005:30) TO Search-Exp-Dates(3)
                     MOVE Profile-Line(1035:150) TO Search-Exp-Desc(3)
                     MOVE Profile-Line(1185:40) TO Search-Edu-Degree(1)
                     MOVE Profile-Line(1225:50) TO Search-Edu-School(1)
                     MOVE Profile-Line(1275:20) TO Search-Edu-Years(1)
                     MOVE Profile-Line(1295:40) TO Search-Edu-Degree(2)
                     MOVE Profile-Line(1335:50) TO Search-Edu-School(2)
                     MOVE Profile-Line(1385:20) TO Search-Edu-Years(2)
                     MOVE Profile-Line(1405:40) TO Search-Edu-Degree(3)
                     MOVE Profile-Line(1445:50) TO Search-Edu-School(3)
                     MOVE Profile-Line(1495:20) TO Search-Edu-Years(3)
                     EXIT PERFORM
                 END-IF
         END-READ
     END-PERFORM

     CLOSE PROFILES-FILE
     OPEN EXTEND PROFILES-FILE
     .

 Display-Search-Profile.
     MOVE "--- Found User Profile ---" TO WS-MSG
     PERFORM OUT-MSG
     MOVE SPACES TO WS-MSG
     STRING "Name: " DELIMITED BY SIZE
            FUNCTION TRIM(Search-First-Name) DELIMITED BY SIZE
            " " DELIMITED BY SIZE
            FUNCTION TRIM(Search-Last-Name) DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG
     MOVE SPACES TO WS-MSG
     STRING "University: " DELIMITED BY SIZE
            FUNCTION TRIM(Search-University) DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG
     MOVE SPACES TO WS-MSG
     STRING "Major: " DELIMITED BY SIZE
            FUNCTION TRIM(Search-Major) DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG
     MOVE SPACES TO WS-MSG
     STRING "Graduation Year: " DELIMITED BY SIZE
            FUNCTION TRIM(Search-Grad-Year) DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG
     IF Search-About-Me NOT = SPACES
         MOVE SPACES TO WS-MSG
         STRING "About Me: " DELIMITED BY SIZE
                FUNCTION TRIM(Search-About-Me) DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
     END-IF
     PERFORM Display-Search-Experience
     PERFORM Display-Search-Education
     MOVE "-------------------------" TO WS-MSG
     PERFORM OUT-MSG
     .

 Display-Search-Experience.
     MOVE 0 TO Has-Entries
     PERFORM VARYING Prof-Entry-Index FROM 1 BY 1 UNTIL Prof-Entry-Index > 3
         IF Search-Exp-Title(Prof-Entry-Index) NOT = SPACES OR
            Search-Exp-Company(Prof-Entry-Index) NOT = SPACES OR
            Search-Exp-Dates(Prof-Entry-Index) NOT = SPACES OR
            Search-Exp-Desc(Prof-Entry-Index) NOT = SPACES
             ADD 1 TO Has-Entries
         END-IF
     END-PERFORM

     IF Has-Entries = 0
         MOVE "Experience: None" TO WS-MSG
         PERFORM OUT-MSG
     ELSE
         MOVE "Experience:" TO WS-MSG
         PERFORM OUT-MSG
         PERFORM VARYING Prof-Entry-Index FROM 1 BY 1 UNTIL Prof-Entry-Index > 3
             IF Search-Exp-Title(Prof-Entry-Index) NOT = SPACES OR
                Search-Exp-Company(Prof-Entry-Index) NOT = SPACES OR
                Search-Exp-Dates(Prof-Entry-Index) NOT = SPACES OR
                Search-Exp-Desc(Prof-Entry-Index) NOT = SPACES
                 IF Search-Exp-Title(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Title: " DELIMITED BY SIZE
                            FUNCTION TRIM(Search-Exp-Title(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Search-Exp-Company(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Company: " DELIMITED BY SIZE
                            FUNCTION TRIM(Search-Exp-Company(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Search-Exp-Dates(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Dates: " DELIMITED BY SIZE
                            FUNCTION TRIM(Search-Exp-Dates(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Search-Exp-Desc(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Description: " DELIMITED BY SIZE
                            FUNCTION TRIM(Search-Exp-Desc(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
             END-IF
         END-PERFORM
     END-IF
     .

 Display-Search-Education.
     MOVE 0 TO Has-Entries
     PERFORM VARYING Prof-Entry-Index FROM 1 BY 1 UNTIL Prof-Entry-Index > 3
         IF Search-Edu-Degree(Prof-Entry-Index) NOT = SPACES OR
            Search-Edu-School(Prof-Entry-Index) NOT = SPACES OR
            Search-Edu-Years(Prof-Entry-Index) NOT = SPACES
             ADD 1 TO Has-Entries
         END-IF
     END-PERFORM

     IF Has-Entries = 0
         MOVE "Education: None" TO WS-MSG
         PERFORM OUT-MSG
     ELSE
         MOVE "Education:" TO WS-MSG
         PERFORM OUT-MSG
         PERFORM VARYING Prof-Entry-Index FROM 1 BY 1 UNTIL Prof-Entry-Index > 3
             IF Search-Edu-Degree(Prof-Entry-Index) NOT = SPACES OR
                Search-Edu-School(Prof-Entry-Index) NOT = SPACES OR
                Search-Edu-Years(Prof-Entry-Index) NOT = SPACES
                 IF Search-Edu-Degree(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Degree: " DELIMITED BY SIZE
                            FUNCTION TRIM(Search-Edu-Degree(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Search-Edu-School(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  University: " DELIMITED BY SIZE
                            FUNCTION TRIM(Search-Edu-School(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
                 IF Search-Edu-Years(Prof-Entry-Index) NOT = SPACES
                     MOVE SPACES TO WS-MSG
                     STRING "  Years: " DELIMITED BY SIZE
                            FUNCTION TRIM(Search-Edu-Years(Prof-Entry-Index)) DELIMITED BY SIZE
                            INTO WS-MSG
                     PERFORM OUT-MSG
                 END-IF
             END-IF
         END-PERFORM
     END-IF
     .

 *> -----------------------------
 *> CONNECTION MANAGEMENT FUNCTIONS
 *> -----------------------------
 Load-Connections-From-Disk.
     CLOSE CONNECTIONS-FILE
     OPEN INPUT CONNECTIONS-FILE
     MOVE 'N' TO CONN-EOF

     PERFORM UNTIL CONN-EOF = 'Y'
         READ CONNECTIONS-FILE
             AT END
                 MOVE 'Y' TO CONN-EOF
             NOT AT END
                 MOVE SPACES TO U-Part P-Part S-Part
                 UNSTRING Connection-Line DELIMITED BY '|'
                     INTO U-Part, P-Part, S-Part
                 END-UNSTRING
                 IF U-Part NOT = SPACES AND P-Part NOT = SPACES
                     IF Connection-Count < 50
                         ADD 1 TO Connection-Count
                         MOVE U-Part TO Conn-Sender(Connection-Count)
                         MOVE P-Part TO Conn-Recipient(Connection-Count)
                         IF S-Part = SPACES
                             MOVE "P" TO Conn-Status(Connection-Count)
                         ELSE
                             MOVE S-Part(1:1) TO Conn-Status(Connection-Count)
                         END-IF
                     END-IF
                 END-IF
         END-READ
     END-PERFORM

     CLOSE CONNECTIONS-FILE
     OPEN EXTEND CONNECTIONS-FILE
     .

 Append-Connection-To-Disk.
     IF Connection-Count <= 50
         MOVE ALL SPACES TO Connection-Line
         STRING
             FUNCTION TRIM(Conn-Sender(Connection-Count))    DELIMITED BY SIZE
             "|" DELIMITED BY SIZE
             FUNCTION TRIM(Conn-Recipient(Connection-Count)) DELIMITED BY SIZE
             "|" DELIMITED BY SIZE
             Conn-Status(Connection-Count)                   DELIMITED BY SIZE
             INTO Connection-Line
         END-STRING
         WRITE Connection-Line
     ELSE
         MOVE "Max 50 connections reached, cannot save new connection." TO WS-MSG
         PERFORM OUT-MSG
     END-IF
     .

 *> -----------------------------
 *> Helper: compute username length (stop at first space)
 *> -----------------------------
 Compute-Name-Length.
     MOVE 0 TO NameLen
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
         MOVE UserName(I:1) TO TempChar
         IF TempChar = SPACE
             EXIT PERFORM
         ELSE
             ADD 1 TO NameLen
         END-IF
     END-PERFORM
     .

 *> -----------------------------
 *> Helper: check username uniqueness
 *> -----------------------------
 Check-Username-Exists.
     SET Username-Not-Exists TO TRUE
     IF Account-Count > 0
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > Account-Count
             IF UserName = Acc-Username(I)
                 SET Username-Exists TO TRUE
                 EXIT PERFORM
             END-IF
         END-PERFORM
     END-IF
     .

 *> -----------------------------
 *> Helper: password validation
 *> -----------------------------
 Validate-Password.
     SET Pass-Is-Invalid TO TRUE
     MOVE 0 TO Has-Upper Has-Digit Has-Special
     MOVE 0 TO PassLen

     *> length check (stop at first space)
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
         MOVE UserPassword(I:1) TO TempChar
         IF TempChar = SPACE
             EXIT PERFORM
         ELSE
             ADD 1 TO PassLen
         END-IF
     END-PERFORM

     IF PassLen < 8 OR PassLen > 12
         EXIT PARAGRAPH
     END-IF

     *> character checks
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > PassLen
         MOVE UserPassword(I:1) TO TempChar
         IF TempChar >= "A" AND TempChar <= "Z"
             MOVE 1 TO Has-Upper
         END-IF
         IF TempChar >= "0" AND TempChar <= "9"
             MOVE 1 TO Has-Digit
         END-IF
         MOVE 0 TO CountVar
         INSPECT Specials TALLYING CountVar FOR ALL TempChar
         IF CountVar > 0
             MOVE 1 TO Has-Special
         END-IF
     END-PERFORM

     IF Has-Upper = 1 AND Has-Digit = 1 AND Has-Special = 1
         SET Pass-Is-Valid TO TRUE
     END-IF
     .

 *> -----------------------------
 *> Load accounts from disk at startup
 *> -----------------------------
 Load-Accounts-From-Disk.
     CLOSE ACCOUNTS-FILE
     OPEN INPUT ACCOUNTS-FILE
     MOVE 'N' TO ACCT-EOF

     PERFORM UNTIL ACCT-EOF = 'Y'
         READ ACCOUNTS-FILE
             AT END
                 MOVE 'Y' TO ACCT-EOF
             NOT AT END
                 MOVE SPACES TO U-Part P-Part
                 UNSTRING Account-Line DELIMITED BY '|'
                     INTO U-Part, P-Part
                 END-UNSTRING
                 IF U-Part NOT = SPACES AND P-Part NOT = SPACES
                     IF Account-Count < 5
                         ADD 1 TO Account-Count
                         MOVE U-Part TO Acc-Username(Account-Count)
                         MOVE P-Part TO Acc-Password(Account-Count)
                     END-IF
                 END-IF
         END-READ
     END-PERFORM

     CLOSE ACCOUNTS-FILE
     OPEN EXTEND ACCOUNTS-FILE
     .

 *> -----------------------------
 *> Append account to file
 *> -----------------------------
 Append-Account-To-Disk.
     IF Account-Count <= 5
         MOVE ALL SPACES TO Account-Line
         STRING
             FUNCTION TRIM(UserName)     DELIMITED BY SIZE
             "|"                         DELIMITED BY SIZE
             FUNCTION TRIM(UserPassword) DELIMITED BY SIZE
             INTO Account-Line
         END-STRING
         WRITE Account-Line
     ELSE
         MOVE "Max 5 accounts reached, cannot save new account." TO WS-MSG
         PERFORM OUT-MSG
     END-IF
     .PERFORM OUT-MSG
     .

 *> -----------------------------
 *> LOGIN
 *> -----------------------------
 Do-Login.
     IF Account-Count = 0
         MOVE "No accounts exist. Create an account first." TO WS-MSG
         PERFORM OUT-MSG
         GOBACK
     END-IF

     SET Pass-Is-Invalid TO TRUE
     PERFORM WITH TEST AFTER UNTIL Pass-Is-Valid OR EOF-IN = "Y"
         MOVE "Please enter your username: " TO WS-MSG
         PERFORM OUT-MSG
         PERFORM READ-NEXT-INPUT
         IF EOF-IN = "Y"
             EXIT PERFORM
         END-IF
         MOVE FUNCTION TRIM(InLine) TO UserName

         MOVE "Please enter your password: " TO WS-MSG
         PERFORM OUT-MSG
         PERFORM READ-NEXT-INPUT
         IF EOF-IN = "Y"
             EXIT PERFORM
         END-IF
         MOVE FUNCTION TRIM(InLine) TO UserPassword

         PERFORM Check-Credentials
         IF Pass-Is-Valid
             MOVE "You have successfully logged in." TO WS-MSG
             PERFORM OUT-MSG
             MOVE SPACES TO WS-MSG
             STRING "Welcome, " DELIMITED BY SIZE
                    FUNCTION TRIM(UserName) DELIMITED BY SIZE
                    "!" DELIMITED BY SIZE
                    INTO WS-MSG
             PERFORM OUT-MSG
             SET USER-LOGGED-IN TO TRUE
             EXIT PERFORM
         ELSE
             MOVE "Incorrect username/password. Try again" TO WS-MSG
             PERFORM OUT-MSG
         END-IF
     END-PERFORM

     IF EOF-IN = "Y" AND NOT Pass-Is-Valid
         MOVE "N" TO WS-LOGGED-IN
     END-IF
     .

 Check-Credentials.
     SET Pass-Is-Invalid TO TRUE
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > Account-Count
         IF UserName = Acc-Username(I) AND
            UserPassword = Acc-Password(I)
             SET Pass-Is-Valid TO TRUE
             EXIT PERFORM
         END-IF
     END-PERFORM
     .

 *> -----------------------------
 *> REGISTRATION
 *> -----------------------------
 Do-Registration.
     IF Account-Count >= 5
         MOVE "Max account reached. Please come back later" TO WS-MSG
         PERFORM OUT-MSG
         EXIT PARAGRAPH
     END-IF

     SET Username-Exists TO TRUE

     *> --- Username step ---
     PERFORM UNTIL Username-Not-Exists
         MOVE "Enter a username (max 20 chars, no spaces)." TO WS-MSG
         PERFORM OUT-MSG
         MOVE "Username: " TO WS-MSG
         PERFORM OUT-MSG

         PERFORM READ-NEXT-INPUT
         MOVE FUNCTION TRIM(InLine) TO UserName

         PERFORM Compute-Name-Length
         IF NameLen = 0
             MOVE "Username cannot be empty." TO WS-MSG
             PERFORM OUT-MSG
         ELSE
             PERFORM Check-Username-Exists
             IF Username-Exists
                 MOVE "Username is already taken. Try another." TO WS-MSG
                 PERFORM OUT-MSG
             ELSE
                 SET Username-Not-Exists TO TRUE
             END-IF
         END-IF
     END-PERFORM

     *> --- Password step ---
     MOVE "Password requirements:" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "- 8 to 12 characters" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "- At least one uppercase letter (A-Z)" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "- At least one digit (0-9)" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "- At least one special character (!@#$... etc.)" TO WS-MSG
     PERFORM OUT-MSG

     SET Pass-Is-Invalid TO TRUE
     PERFORM UNTIL Pass-Is-Valid
         MOVE "Please enter your password: " TO WS-MSG
         PERFORM OUT-MSG
         PERFORM READ-NEXT-INPUT
         MOVE FUNCTION TRIM(InLine) TO UserPassword

         PERFORM Validate-Password
         IF Pass-Is-Invalid
             MOVE "It doesn't meet requirements, try again." TO WS-MSG
             PERFORM OUT-MSG
         END-IF
     END-PERFORM

     *> --- Save in-memory ---
     ADD 1 TO Account-Count
     MOVE UserName     TO Acc-Username(Account-Count)
     MOVE UserPassword TO Acc-Password(Account-Count)

     *> --- Persist (already OPEN EXTEND) ---
     PERFORM Append-Account-To-Disk

     MOVE "Account created successfully." TO WS-MSG
     PERFORM OUT-MSG
     EXIT PARAGRAPH
     .

 *> -----------------------------
 *> MAIN APPLICATION MENUS
 *> -----------------------------
 Show-Main-Menu.
     MOVE "1. Create/Edit My Profile" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "2. View My Profile" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "3. Search for a job" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "4. Find someone you know" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "5. View My Pending Connection Requests" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "6. View My Network" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "7. Messages" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "8. Learn a New Skill" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "9. Log Out" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "Enter your choice: " TO WS-MSG
     PERFORM OUT-MSG
     .

 J-Search-Loop.
     MOVE SPACES TO WS-MENU-SELECTION
     PERFORM UNTIL WS-MENU-SELECTION = "4" OR EOF-IN = "Y"
         PERFORM J-Search-Menu
         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     PERFORM Post-Job-Flow
                 WHEN "2"
                     PERFORM Browse-Jobs-Enhanced
                 WHEN "3"
                     PERFORM View-My-Applications
                 WHEN "4"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     .

 Find-Someone.
     MOVE "Enter the full name of the person you are looking for: " TO WS-MSG
     PERFORM OUT-MSG
     PERFORM READ-NEXT-INPUT
     IF EOF-IN NOT = "Y"
         MOVE FUNCTION TRIM(InLine) TO Search-Name
         PERFORM Perform-Search
         IF User-Found
             PERFORM Display-Search-Profile
             *> Add connection request option after displaying profile
             PERFORM Connection-Options-Menu
         ELSE
             MOVE "No one by that name could be found." TO WS-MSG
             PERFORM OUT-MSG
         END-IF
     END-IF
     .

 Connection-Options-Menu.
     PERFORM UNTIL WS-MENU-SELECTION = "2" OR EOF-IN = "Y"
         MOVE "1. Send Connection Request" TO WS-MSG
         PERFORM OUT-MSG
         MOVE "2. Back to Main Menu" TO WS-MSG
         PERFORM OUT-MSG
         MOVE "Enter your choice: " TO WS-MSG
         PERFORM OUT-MSG

         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     PERFORM Send-Connection-Request
                     MOVE "2" TO WS-MENU-SELECTION
                 WHEN "2"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     .

 Send-Connection-Request.
     *> Validate the connection request
     PERFORM Validate-Connection-Request
     IF Connection-Is-Valid
         *> Add to in-memory connections array
         ADD 1 TO Connection-Count
         MOVE UserName TO Conn-Sender(Connection-Count)
         MOVE Search-Username TO Conn-Recipient(Connection-Count)
         MOVE "P" TO Conn-Status(Connection-Count)
         *> Persist to file
         PERFORM Append-Connection-To-Disk

         MOVE SPACES TO WS-MSG
         STRING "Connection request sent to " DELIMITED BY SIZE
                FUNCTION TRIM(Search-First-Name) DELIMITED BY SIZE
                " " DELIMITED BY SIZE
                FUNCTION TRIM(Search-Last-Name) DELIMITED BY SIZE
                "." DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
     END-IF
     .

 Validate-Connection-Request.
     SET Connection-Is-Valid TO TRUE

     IF UserName = Search-Username
         MOVE "You cannot send yourself a connection request." TO WS-MSG
         PERFORM OUT-MSG
         SET Connection-Is-Invalid TO TRUE
         EXIT PARAGRAPH
     END-IF

     *> Check if connection already exists (either direction)
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > Connection-Count
         IF (Conn-Sender(I) = UserName AND
             Conn-Recipient(I) = Search-Username) OR
            (Conn-Sender(I) = Search-Username AND
             Conn-Recipient(I) = UserName)
             IF Conn-Sender(I) = UserName
                 MOVE "You have already sent a connection request to this user." TO WS-MSG
             ELSE
                 MOVE "This user has already sent you a connection request." TO WS-MSG
             END-IF
             PERFORM OUT-MSG
             SET Connection-Is-Invalid TO TRUE
             EXIT PERFORM
         END-IF
     END-PERFORM
     .

 *> ========================================
 *> View Pending Requests with Accept/Reject
 *> ========================================
 View-Pending-Requests-With-Actions.
     MOVE "--- Pending Connection Requests ---" TO WS-MSG
     PERFORM OUT-MSG

     *> Build list of pending requests for current user
     MOVE 0 TO Pending-Request-Count
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > Connection-Count
         IF Conn-Recipient(I) = UserName AND Conn-Status(I) = "P"
             ADD 1 TO Pending-Request-Count
             MOVE I TO Pend-Index(Pending-Request-Count)
             MOVE Conn-Sender(I) TO Pend-Sender(Pending-Request-Count)
         END-IF
     END-PERFORM

     IF Pending-Request-Count = 0
         MOVE "You have no pending connection requests at this time." TO WS-MSG
         PERFORM OUT-MSG
     ELSE
         *> Display each request with Accept/Reject options
         PERFORM VARYING J FROM 1 BY 1 UNTIL J > Pending-Request-Count
             MOVE Pend-Index(J) TO I
             PERFORM Display-Pending-Request-Details
             PERFORM Handle-Request-Action
             IF EOF-IN = "Y"
                 EXIT PERFORM
             END-IF
         END-PERFORM
     END-IF

     MOVE "-----------------------------------" TO WS-MSG
     PERFORM OUT-MSG
     .

 Display-Pending-Request-Details.
     *> Find and display the profile of the sender
     PERFORM Find-Profile-By-Connection-Sender
     IF Profile-Exists
         MOVE SPACES TO WS-MSG
         STRING "Request from: " DELIMITED BY SIZE
                FUNCTION TRIM(Temp-First-Name) DELIMITED BY SIZE
                " " DELIMITED BY SIZE
                FUNCTION TRIM(Temp-Last-Name) DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
         MOVE SPACES TO WS-MSG
         STRING "  University: " DELIMITED BY SIZE
                FUNCTION TRIM(Temp-University) DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
         MOVE SPACES TO WS-MSG
         STRING "  Major: " DELIMITED BY SIZE
                FUNCTION TRIM(Temp-Major) DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
     ELSE
         MOVE SPACES TO WS-MSG
         STRING "Request from: " DELIMITED BY SIZE
                FUNCTION TRIM(Conn-Sender(I)) DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
     END-IF
     .

 Handle-Request-Action.
     PERFORM UNTIL WS-MENU-SELECTION = "1" OR
                   WS-MENU-SELECTION = "2" OR
                   EOF-IN = "Y"
         MOVE "1. Accept" TO WS-MSG
         PERFORM OUT-MSG
         MOVE "2. Reject" TO WS-MSG
         PERFORM OUT-MSG
         MOVE "Enter your choice: " TO WS-MSG
         PERFORM OUT-MSG

         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     PERFORM Accept-Connection-Request
                 WHEN "2"
                     PERFORM Reject-Connection-Request
                 WHEN OTHER
                     MOVE "Invalid choice. Please select 1 or 2." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     MOVE SPACES TO WS-MENU-SELECTION
     .

 Accept-Connection-Request.
     *> Update status in memory
     MOVE "A" TO Conn-Status(I)

     *> Rewrite the entire connections file
     PERFORM Rewrite-Connections-File

     MOVE SPACES TO WS-MSG
     STRING "Connection request from " DELIMITED BY SIZE
            FUNCTION TRIM(Conn-Sender(I)) DELIMITED BY SIZE
            " accepted." DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG
     .

 Reject-Connection-Request.
     *> Update status in memory
     MOVE "R" TO Conn-Status(I)

     *> Rewrite the entire connections file
     PERFORM Rewrite-Connections-File

     MOVE SPACES TO WS-MSG
     STRING "Connection request from " DELIMITED BY SIZE
            FUNCTION TRIM(Conn-Sender(I)) DELIMITED BY SIZE
            " rejected." DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG
     .

 *> ========================================
 *> View My Network
 *> ========================================
 View-My-Network.
     MOVE "--- My Network ---" TO WS-MSG
     PERFORM OUT-MSG

     MOVE 0 TO Has-Entries
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > Connection-Count
         IF Conn-Status(I) = "A"
             IF Conn-Sender(I) = UserName OR Conn-Recipient(I) = UserName
                 ADD 1 TO Has-Entries
                 PERFORM Display-Network-Connection
             END-IF
         END-IF
     END-PERFORM

     IF Has-Entries = 0
         MOVE "You have no connections yet." TO WS-MSG
         PERFORM OUT-MSG
     END-IF

     MOVE "-------------------" TO WS-MSG
     PERFORM OUT-MSG
     .

 Display-Network-Connection.
     *> Determine which user is the connection
     IF Conn-Sender(I) = UserName
         MOVE Conn-Recipient(I) TO Target-Username
         PERFORM Find-Profile-For-Network
     ELSE
         MOVE Conn-Sender(I) TO Target-Username
         PERFORM Find-Profile-For-Network
     END-IF

     IF Profile-Exists
         MOVE SPACES TO WS-MSG
         STRING "- " DELIMITED BY SIZE
                FUNCTION TRIM(Temp-First-Name) DELIMITED BY SIZE
                " " DELIMITED BY SIZE
                FUNCTION TRIM(Temp-Last-Name) DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
         MOVE SPACES TO WS-MSG
         STRING "  University: " DELIMITED BY SIZE
                FUNCTION TRIM(Temp-University) DELIMITED BY SIZE
                ", Major: " DELIMITED BY SIZE
                FUNCTION TRIM(Temp-Major) DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
     ELSE
         IF Conn-Sender(I) = UserName
             MOVE SPACES TO WS-MSG
             STRING "- " DELIMITED BY SIZE
                    FUNCTION TRIM(Conn-Recipient(I)) DELIMITED BY SIZE
                    INTO WS-MSG
             PERFORM OUT-MSG
         ELSE
             MOVE SPACES TO WS-MSG
             STRING "- " DELIMITED BY SIZE
                    FUNCTION TRIM(Conn-Sender(I)) DELIMITED BY SIZE
                    INTO WS-MSG
             PERFORM OUT-MSG
         END-IF
     END-IF
     .

 Find-Profile-For-Network.
     SET Profile-Not-Exists TO TRUE
     CLOSE PROFILES-FILE
     OPEN INPUT PROFILES-FILE
     MOVE 'N' TO PROF-EOF

     PERFORM UNTIL PROF-EOF = 'Y' OR Profile-Exists
         READ PROFILES-FILE
             AT END
                 MOVE 'Y' TO PROF-EOF
             NOT AT END
                 IF Profile-Line(1:20) = Target-Username
                     SET Profile-Exists TO TRUE
                     MOVE Profile-Line(21:30) TO Temp-First-Name
                     MOVE Profile-Line(51:30) TO Temp-Last-Name
                     MOVE Profile-Line(81:50) TO Temp-University
                     MOVE Profile-Line(131:40) TO Temp-Major
                 END-IF
         END-READ
     END-PERFORM

     CLOSE PROFILES-FILE
     OPEN EXTEND PROFILES-FILE
     .

 Rewrite-Connections-File.
     *> Close and reopen in OUTPUT mode to clear the file
     CLOSE CONNECTIONS-FILE
     OPEN OUTPUT CONNECTIONS-FILE

     *> Write all connections with their current status
     PERFORM VARYING J FROM 1 BY 1 UNTIL J > Connection-Count
         IF Conn-Sender(J) NOT = SPACES AND
            Conn-Recipient(J) NOT = SPACES
             MOVE ALL SPACES TO Connection-Line
             STRING
                 FUNCTION TRIM(Conn-Sender(J)) DELIMITED BY SIZE
                 "|" DELIMITED BY SIZE
                 FUNCTION TRIM(Conn-Recipient(J)) DELIMITED BY SIZE
                 "|" DELIMITED BY SIZE
                 Conn-Status(J) DELIMITED BY SIZE
                 INTO Connection-Line
             END-STRING
             WRITE Connection-Line
         END-IF
     END-PERFORM

     CLOSE CONNECTIONS-FILE
     OPEN EXTEND CONNECTIONS-FILE
     .

 Find-Profile-By-Connection-Sender.
     SET Profile-Not-Exists TO TRUE
     CLOSE PROFILES-FILE
     OPEN INPUT PROFILES-FILE
     MOVE 'N' TO PROF-EOF

     PERFORM UNTIL PROF-EOF = 'Y' OR Profile-Exists
         READ PROFILES-FILE
             AT END
                 MOVE 'Y' TO PROF-EOF
             NOT AT END
                 IF Profile-Line(1:20) = Conn-Sender(I)
                     SET Profile-Exists TO TRUE
                     MOVE Profile-Line(21:30) TO Temp-First-Name
                     MOVE Profile-Line(51:30) TO Temp-Last-Name
                     MOVE Profile-Line(81:50) TO Temp-University
                     MOVE Profile-Line(131:40) TO Temp-Major
                 END-IF
         END-READ
     END-PERFORM

     CLOSE PROFILES-FILE
     OPEN EXTEND PROFILES-FILE
     .

 *> ================================
 *> Job Search/Internship menu
 *> ================================
 J-Search-Menu.
     MOVE "--- Job Search/Internship Menu ---" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "1. Post a Job/Internship" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "2. Browse Jobs/Internships" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "3. View My Applications" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "4. Back to Main Menu" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "Enter your choice: " TO WS-MSG
     PERFORM OUT-MSG
     .

 *> ================================
 *> Post a Job/Internship Flow
 *> ================================
 Post-Job-Flow.
     MOVE "--- Post a New Job/Internship ---" TO WS-MSG
     PERFORM OUT-MSG

     *> Job Title (required)
     MOVE "Enter Job Title: " TO WS-MSG
     PERFORM OUT-MSG
     PERFORM READ-NEXT-INPUT
     MOVE FUNCTION TRIM(InLine) TO Job-Title

     *> Description (required, max 200)
     MOVE "Enter Description (max 200 chars): " TO WS-MSG
     PERFORM OUT-MSG
     PERFORM READ-NEXT-INPUT
     MOVE FUNCTION TRIM(InLine) TO Job-Description
     IF FUNCTION LENGTH(Job-Description) > 200
         MOVE Job-Description(1:200) TO Job-Description
     END-IF

     *> Employer (required)
     MOVE "Enter Employer Name: " TO WS-MSG
     PERFORM OUT-MSG
     PERFORM READ-NEXT-INPUT
     MOVE FUNCTION TRIM(InLine) TO Job-Employer

     *> Location (required)
     MOVE "Enter Location: " TO WS-MSG
     PERFORM OUT-MSG
     PERFORM READ-NEXT-INPUT
     MOVE FUNCTION TRIM(InLine) TO Job-Location

     *> Salary (optional; 'NONE' allowed)
     MOVE "Enter Salary (optional, enter 'NONE' to skip): " TO WS-MSG
     PERFORM OUT-MSG
     PERFORM READ-NEXT-INPUT
     MOVE FUNCTION TRIM(InLine) TO Job-Salary
     IF FUNCTION LENGTH(FUNCTION TRIM(Job-Salary)) = 0
         MOVE "NONE" TO Job-Salary
     END-IF

     *> Basic required-field validation
     IF Job-Title = SPACES OR
        Job-Description = SPACES OR
        Job-Employer = SPACES OR
        Job-Location = SPACES
         MOVE "Missing required field(s). Job NOT posted." TO WS-MSG
         PERFORM OUT-MSG
         EXIT PARAGRAPH
     END-IF

     *> Persist to disk (already open in EXTEND)
     PERFORM Append-Job-To-Disk

     MOVE "Job posted successfully!" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "----------------------------------" TO WS-MSG
     PERFORM OUT-MSG
     .

 Append-Job-To-Disk.
     *> Record format: poster|title|description|employer|location|salary
     MOVE ALL SPACES TO Job-Line
     STRING
         FUNCTION TRIM(UserName)        DELIMITED BY SIZE
         "|"                            DELIMITED BY SIZE
         FUNCTION TRIM(Job-Title)       DELIMITED BY SIZE
         "|"                            DELIMITED BY SIZE
         FUNCTION TRIM(Job-Description) DELIMITED BY SIZE
         "|"                            DELIMITED BY SIZE
         FUNCTION TRIM(Job-Employer)    DELIMITED BY SIZE
         "|"                            DELIMITED BY SIZE
         FUNCTION TRIM(Job-Location)    DELIMITED BY SIZE
         "|"                            DELIMITED BY SIZE
         FUNCTION TRIM(Job-Salary)      DELIMITED BY SIZE
         INTO Job-Line
     END-STRING
     WRITE Job-Line
     .

 *> ================================
 *> NEW: Browse Jobs Enhanced with Full Details and Application
 *> ================================
 Browse-Jobs-Enhanced.
     MOVE 99 TO Selected-Job-Number
     PERFORM Load-All-Jobs-Into-Memory

     IF Job-Count = 0
         MOVE "No job postings yet." TO WS-MSG
         PERFORM OUT-MSG
         EXIT PARAGRAPH
     END-IF

     PERFORM UNTIL Selected-Job-Number = 0 OR EOF-IN = "Y"
         PERFORM Display-Job-List
         MOVE "Enter job number to view details, or 0 to go back: " TO WS-MSG
         PERFORM OUT-MSG
         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE FUNCTION NUMVAL(WS-INPUT-VALUE) TO Selected-Job-Number
             IF Selected-Job-Number > 0 AND
                Selected-Job-Number <= Job-Count
                 PERFORM Display-Job-Details-And-Apply
             ELSE IF Selected-Job-Number NOT = 0
                 MOVE "Invalid job number." TO WS-MSG
                 PERFORM OUT-MSG
             END-IF
         END-IF
     END-PERFORM
     MOVE 0 TO Selected-Job-Number
     .

 Load-All-Jobs-Into-Memory.
     MOVE 0 TO Job-Count
     CLOSE JOBS-FILE
     OPEN INPUT JOBS-FILE
     MOVE 'N' TO JOB-EOF

     PERFORM UNTIL JOB-EOF = 'Y' OR Job-Count >= 99
         READ JOBS-FILE
             AT END
                 MOVE 'Y' TO JOB-EOF
             NOT AT END
                 ADD 1 TO Job-Count
                 MOVE Job-Count TO Job-ID(Job-Count)

                 *> Parse: poster|title|description|employer|location|salary
                 MOVE SPACES TO U-Part Job-Title Job-Description
                                Job-Employer Job-Location Job-Salary
                 UNSTRING Job-Line DELIMITED BY '|'
                     INTO U-Part, Job-Title, Job-Description,
                          Job-Employer, Job-Location, Job-Salary
                 END-UNSTRING

                 MOVE U-Part TO Job-Poster(Job-Count)
                 MOVE Job-Title TO Job-Title-Store(Job-Count)
                 MOVE Job-Description TO Job-Description-Store(Job-Count)
                 MOVE Job-Employer TO Job-Employer-Store(Job-Count)
                 MOVE Job-Location TO Job-Location-Store(Job-Count)
                 MOVE Job-Salary TO Job-Salary-Store(Job-Count)
         END-READ
     END-PERFORM

     CLOSE JOBS-FILE
     OPEN EXTEND JOBS-FILE
     .

 Display-Job-List.
     MOVE "--- Available Job Listings ---" TO WS-MSG
     PERFORM OUT-MSG

     PERFORM VARYING I FROM 1 BY 1 UNTIL I > Job-Count
         MOVE SPACES TO WS-MSG
         STRING
             I DELIMITED BY SIZE
             ". " DELIMITED BY SIZE
             FUNCTION TRIM(Job-Title-Store(I)) DELIMITED BY SIZE
             " at " DELIMITED BY SIZE
             FUNCTION TRIM(Job-Employer-Store(I)) DELIMITED BY SIZE
             " (" DELIMITED BY SIZE
             FUNCTION TRIM(Job-Location-Store(I)) DELIMITED BY SIZE
             ")" DELIMITED BY SIZE
             INTO WS-MSG
         PERFORM OUT-MSG
     END-PERFORM

     MOVE "-----------------------------" TO WS-MSG
     PERFORM OUT-MSG
     .

 Display-Job-Details-And-Apply.
     MOVE "--- Job Details ---" TO WS-MSG
     PERFORM OUT-MSG

     MOVE SPACES TO WS-MSG
     STRING "Title: " DELIMITED BY SIZE
            FUNCTION TRIM(Job-Title-Store(Selected-Job-Number))
            DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG

     MOVE SPACES TO WS-MSG
     STRING "Description: " DELIMITED BY SIZE
            FUNCTION TRIM(Job-Description-Store(Selected-Job-Number))
            DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG

     MOVE SPACES TO WS-MSG
     STRING "Employer: " DELIMITED BY SIZE
            FUNCTION TRIM(Job-Employer-Store(Selected-Job-Number))
            DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG

     MOVE SPACES TO WS-MSG
     STRING "Location: " DELIMITED BY SIZE
            FUNCTION TRIM(Job-Location-Store(Selected-Job-Number))
            DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG

     MOVE SPACES TO WS-MSG
     STRING "Salary: " DELIMITED BY SIZE
            FUNCTION TRIM(Job-Salary-Store(Selected-Job-Number))
            DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG

     MOVE "-------------------" TO WS-MSG
     PERFORM OUT-MSG

     *> Show apply option
     PERFORM UNTIL Job-Details-Choice = "1" OR
                   Job-Details-Choice = "2" OR
                   EOF-IN = "Y"
         MOVE "1. Apply for this Job" TO WS-MSG
         PERFORM OUT-MSG
         MOVE "2. Back to Job List" TO WS-MSG
         PERFORM OUT-MSG
         MOVE "Enter your choice: " TO WS-MSG
         PERFORM OUT-MSG

         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO Job-Details-Choice
             EVALUATE Job-Details-Choice
                 WHEN "1"
                     PERFORM Apply-For-Job
                 WHEN "2"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     MOVE SPACES TO Job-Details-Choice
     .

 Apply-For-Job.
     *> Check if already applied
     PERFORM Check-Already-Applied

     IF Already-Applied
         MOVE "You have already applied for this job." TO WS-MSG
         PERFORM OUT-MSG
     ELSE
         *> Add application to memory
         ADD 1 TO Application-Count
         MOVE UserName TO App-Username(Application-Count)
         MOVE Job-ID(Selected-Job-Number) TO App-Job-ID(Application-Count)

         *> Persist to disk
         PERFORM Append-Application-To-Disk

         *> Confirmation message
         MOVE SPACES TO WS-MSG
         STRING "Your application for " DELIMITED BY SIZE
                FUNCTION TRIM(Job-Title-Store(Selected-Job-Number))
                DELIMITED BY SIZE
                " at " DELIMITED BY SIZE
                FUNCTION TRIM(Job-Employer-Store(Selected-Job-Number))
                DELIMITED BY SIZE
                " has been submitted." DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
     END-IF
     .

 Check-Already-Applied.
     SET Not-Applied-Yet TO TRUE
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > Application-Count
         IF App-Username(I) = UserName AND
            App-Job-ID(I) = Job-ID(Selected-Job-Number)
             SET Already-Applied TO TRUE
             EXIT PERFORM
         END-IF
     END-PERFORM
     .

 Append-Application-To-Disk.
     *> Record format: username|jobID
     MOVE ALL SPACES TO Application-Line
     STRING
         FUNCTION TRIM(UserName) DELIMITED BY SIZE
         "|" DELIMITED BY SIZE
         App-Job-ID(Application-Count) DELIMITED BY SIZE
         INTO Application-Line
     END-STRING
     WRITE Application-Line
     .

 Load-Applications-From-Disk.
     CLOSE APPLICATIONS-FILE
     OPEN INPUT APPLICATIONS-FILE
     MOVE 'N' TO APP-EOF
     MOVE 0 TO Application-Count

     PERFORM UNTIL APP-EOF = 'Y'
         READ APPLICATIONS-FILE
             AT END
                 MOVE 'Y' TO APP-EOF
             NOT AT END
                 MOVE SPACES TO U-Part
                 MOVE 0 TO Selected-Job-Number
                 UNSTRING Application-Line DELIMITED BY '|'
                     INTO U-Part, Selected-Job-Number
                 END-UNSTRING
                 IF U-Part NOT = SPACES
                     IF Application-Count < 99
                         ADD 1 TO Application-Count
                         MOVE U-Part TO App-Username(Application-Count)
                         MOVE Selected-Job-Number TO
                              App-Job-ID(Application-Count)
                     END-IF
                 END-IF
         END-READ
     END-PERFORM

     CLOSE APPLICATIONS-FILE
     OPEN EXTEND APPLICATIONS-FILE
     .

 *> ================================
 *> NEW: View My Applications Report
 *> ================================
 View-My-Applications.
     MOVE "--- Your Job Applications ---" TO WS-MSG
     PERFORM OUT-MSG

     MOVE SPACES TO WS-MSG
     STRING "Application Summary for " DELIMITED BY SIZE
            FUNCTION TRIM(UserName) DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG

     MOVE "------------------------------" TO WS-MSG
     PERFORM OUT-MSG

     *> Load jobs into memory first
     PERFORM Load-All-Jobs-Into-Memory

     *> Count and display user's applications
     MOVE 0 TO User-App-Count
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > Application-Count
         IF App-Username(I) = UserName
             ADD 1 TO User-App-Count
             PERFORM Display-Application-Entry
         END-IF
     END-PERFORM

     IF User-App-Count = 0
         MOVE "You have not applied to any jobs yet." TO WS-MSG
         PERFORM OUT-MSG
     ELSE
         MOVE "------------------------------" TO WS-MSG
         PERFORM OUT-MSG
         MOVE SPACES TO WS-MSG
         STRING "Total Applications: " DELIMITED BY SIZE
                User-App-Count DELIMITED BY SIZE
                INTO WS-MSG
         PERFORM OUT-MSG
     END-IF

     MOVE "------------------------------" TO WS-MSG
     PERFORM OUT-MSG
     .

 Display-Application-Entry.
     *> Find the job details for this application
     PERFORM VARYING J FROM 1 BY 1 UNTIL J > Job-Count
         IF Job-ID(J) = App-Job-ID(I)
             MOVE SPACES TO WS-MSG
             STRING "Job Title: " DELIMITED BY SIZE
                    FUNCTION TRIM(Job-Title-Store(J)) DELIMITED BY SIZE
                    INTO WS-MSG
             PERFORM OUT-MSG

             MOVE SPACES TO WS-MSG
             STRING "Employer: " DELIMITED BY SIZE
                    FUNCTION TRIM(Job-Employer-Store(J)) DELIMITED BY SIZE
                    INTO WS-MSG
             PERFORM OUT-MSG

             MOVE SPACES TO WS-MSG
             STRING "Location: " DELIMITED BY SIZE
                    FUNCTION TRIM(Job-Location-Store(J)) DELIMITED BY SIZE
                    INTO WS-MSG
             PERFORM OUT-MSG

             MOVE "---" TO WS-MSG
             PERFORM OUT-MSG

             EXIT PERFORM
         END-IF
     END-PERFORM
     .

 *> ===============================================================
 *> Skill menus (stubs)
 *> ===============================================================
 Skill-Loop.
     PERFORM UNTIL WS-MENU-SELECTION = "5" OR EOF-IN = "Y"
         PERFORM Skill-Menu
         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     PERFORM Web-Dev-Loop
                 WHEN "2"
                     PERFORM Deep-Learning-Loop
                 WHEN "3"
                     PERFORM Interview-Loop
                 WHEN "4"
                     PERFORM Resume-Loop
                 WHEN "5"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     .

 Web-Dev-Loop.
     PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
         PERFORM Web-Dev-Menu
         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     .

 Deep-Learning-Loop.
     PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
         PERFORM Deep-Learning-Menu
         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     .

 Interview-Loop.
     PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
         PERFORM Interview-Menu
         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     .

 Resume-Loop.
     PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
         PERFORM Resume-Menu
         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     .

 Profile-Loop.
     PERFORM UNTIL WS-MENU-SELECTION = "5" OR EOF-IN = "Y"
         PERFORM Profile-Menu
         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     PERFORM Edit-Basic-Info
                 WHEN "2"
                     PERFORM Edit-Experience
                 WHEN "3"
                     PERFORM Edit-Education
                 WHEN "4"
                     PERFORM Save-Profile
                 WHEN "5"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     .

 *> -----------------------------
 *> MENU DISPLAY PARAGRAPHS
 *> -----------------------------
 Skill-Menu.
     MOVE "1. Learn Web Development" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "2. Learn Deep Learning" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "3. Learn How To Crack Interview Questions" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "4. Learn How To Optimize Your Resume" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "5. Return" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "Enter your choice: " TO WS-MSG
     PERFORM OUT-MSG
     .

 Web-Dev-Menu.
    MOVE "Web Development - Quick Tips" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Start with HTML & CSS basics (layout, flexbox, forms)" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Learn JavaScript fundamentals (DOM, events, fetch)" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Build a simple portfolio site with 2-3 pages" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Use Git/GitHub for version control" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "1. Return" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "Enter your choice: " TO WS-MSG
    PERFORM OUT-MSG
    .

 Deep-Learning-Menu.
    MOVE "Deep Learning - Quick Path" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Brush up linear algebra, calculus, and probability" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Practice Python + NumPy; learn tensors and autodiff" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Train a small model (MNIST/CIFAR) and tune learning rate" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Read training logs; avoid overfitting with regularization" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "1. Return" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "Enter your choice: " TO WS-MSG
    PERFORM OUT-MSG
    .

 Interview-Menu.
    MOVE "Interview Prep - Checklist" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Review Big-O and core data structures/algorithms" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Solve 1-2 practice problems daily (arrays, strings, graphs)" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Prepare STAR stories for behavioral questions" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Do mock interviews and reflect on feedback" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "1. Return" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "Enter your choice: " TO WS-MSG
    PERFORM OUT-MSG
    .

 Resume-Menu.
    MOVE "Resume Optimization - Tips" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Keep it to one page (students/early career)" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Use action verbs and quantify impact (e.g., 'reduced build time 30%')" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Tailor bullets to the job description keywords" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "- Put most relevant projects/experience at the top" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "1. Return" TO WS-MSG
    PERFORM OUT-MSG
    MOVE "Enter your choice: " TO WS-MSG
    PERFORM OUT-MSG
    .

 Profile-Menu.
     MOVE "1. Edit Basic Information" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "2. Edit Experience" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "3. Edit Education" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "4. Save Profile" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "5. Return" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "Enter your choice: " TO WS-MSG
     PERFORM OUT-MSG
     .

 *> ===============================================================
 *> NEW: Messaging System (Epic #8 - Week 8)
 *> ===============================================================
 Messages-Loop.
     MOVE SPACES TO WS-MENU-SELECTION
     PERFORM UNTIL WS-MENU-SELECTION = "3" OR EOF-IN = "Y"
         PERFORM Messages-Menu
         PERFORM READ-NEXT-INPUT
         IF EOF-IN NOT = "Y"
             MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
             EVALUATE WS-MENU-SELECTION
                 WHEN "1"
                     PERFORM Send-Message
                 WHEN "2"
                     PERFORM View-My-Messages
                 WHEN "3"
                     CONTINUE
                 WHEN OTHER
                     MOVE "Invalid choice." TO WS-MSG
                     PERFORM OUT-MSG
             END-EVALUATE
         END-IF
     END-PERFORM
     .

 Messages-Menu.
     MOVE "--- Messages Menu ---" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "1. Send a New Message" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "2. View My Messages" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "3. Back to Main Menu" TO WS-MSG
     PERFORM OUT-MSG
     MOVE "Enter your choice: " TO WS-MSG
     PERFORM OUT-MSG
     .

 Send-Message.
     MOVE "Enter recipient's username (must be a connection): " TO WS-MSG
     PERFORM OUT-MSG
     PERFORM READ-NEXT-INPUT
     IF EOF-IN NOT = "Y"
         MOVE FUNCTION TRIM(InLine) TO Message-Recipient
         PERFORM Validate-Message-Recipient

         IF Message-Recipient-Valid
             MOVE "Enter your message (max 200 chars): " TO WS-MSG
             PERFORM OUT-MSG
             PERFORM READ-NEXT-INPUT
             IF EOF-IN NOT = "Y"
                 MOVE FUNCTION TRIM(InLine) TO Message-Content
                 IF FUNCTION LENGTH(Message-Content) > 200
                     MOVE Message-Content(1:200) TO Message-Content
                 END-IF

                 *> Save message to disk
                 PERFORM Append-Message-To-Disk

                 MOVE SPACES TO WS-MSG
                 STRING "Message sent to " DELIMITED BY SIZE
                        FUNCTION TRIM(Message-Recipient) DELIMITED BY SIZE
                        " successfully!" DELIMITED BY SIZE
                        INTO WS-MSG
                 PERFORM OUT-MSG
                 MOVE "---------------------" TO WS-MSG
                 PERFORM OUT-MSG
             END-IF
         END-IF
     END-IF
     .

 Validate-Message-Recipient.
     SET Message-Recipient-Invalid TO TRUE

     *> Check if recipient is a connected user (status = 'A')
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > Connection-Count
         IF Conn-Status(I) = "A"
             IF (Conn-Sender(I) = UserName AND
                 Conn-Recipient(I) = Message-Recipient) OR
                (Conn-Sender(I) = Message-Recipient AND
                 Conn-Recipient(I) = UserName)
                 SET Message-Recipient-Valid TO TRUE
                 EXIT PERFORM
             END-IF
         END-IF
     END-PERFORM

     IF Message-Recipient-Invalid
         MOVE "You can only message users you are connected with." TO WS-MSG
         PERFORM OUT-MSG
     END-IF
     .

 Append-Message-To-Disk.
     *> Record format: sender|recipient|message
     MOVE ALL SPACES TO Message-Line
     STRING
         FUNCTION TRIM(UserName) DELIMITED BY SIZE
         "|" DELIMITED BY SIZE
         FUNCTION TRIM(Message-Recipient) DELIMITED BY SIZE
         "|" DELIMITED BY SIZE
         FUNCTION TRIM(Message-Content) DELIMITED BY SIZE
         INTO Message-Line
     END-STRING
     WRITE Message-Line
     .
 View-My-Messages.
     MOVE "--- Your Messages ---" TO WS-MSG
     PERFORM OUT-MSG

     *> Initialize counter for messages found
     MOVE 0 TO Messages-Found-Count

     *> Read through messages file and display messages for current user
     CLOSE MESSAGES-FILE
     OPEN INPUT MESSAGES-FILE
     MOVE 'N' TO MSG-EOF

     PERFORM UNTIL MSG-EOF = 'Y'
         READ MESSAGES-FILE
             AT END
                 MOVE 'Y' TO MSG-EOF
             NOT AT END
                 *> Parse message: sender|recipient|message
                 MOVE SPACES TO Message-Sender Message-Recipient Message-Text
                 UNSTRING Message-Line DELIMITED BY '|'
                     INTO Message-Sender, Message-Recipient, Message-Text
                 END-UNSTRING

                 *> Check if this message is for the current user
                 IF FUNCTION TRIM(Message-Recipient) = FUNCTION TRIM(UserName)
                     ADD 1 TO Messages-Found-Count
                     PERFORM Display-Single-Message
                 END-IF
         END-READ
     END-PERFORM

     CLOSE MESSAGES-FILE
     OPEN EXTEND MESSAGES-FILE

     *> If no messages found, inform the user
     IF Messages-Found-Count = 0
         MOVE "You have no messages at this time." TO WS-MSG
         PERFORM OUT-MSG
     END-IF

     MOVE "---------------------" TO WS-MSG
     PERFORM OUT-MSG
     .

 Display-Single-Message.
     MOVE SPACES TO WS-MSG
     STRING "From: " DELIMITED BY SIZE
            FUNCTION TRIM(Message-Sender) DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG

     MOVE SPACES TO WS-MSG
     STRING "Message: " DELIMITED BY SIZE
            FUNCTION TRIM(Message-Text) DELIMITED BY SIZE
            INTO WS-MSG
     PERFORM OUT-MSG

     MOVE "---" TO WS-MSG
     PERFORM OUT-MSG
.
