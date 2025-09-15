*> =======================================================
       *> InCollege - Complete System with Authentication & Features
       *> Inputs  : InCollege-Input.txt (one line per prompt)
       *> Outputs : InCollege-Output.txt (mirrors messages)
       *> Accounts: Accounts.dat (username|password per line)
       *> =======================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO 'InCollege-Test.txt'
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

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  InLine                         PIC X(200).

       FD  OUTPUT-FILE.
       01  OutLine                        PIC X(200).

       FD  ACCOUNTS-FILE.
       01  Account-Line                   PIC X(60).

       FD  PROFILES-FILE.
       01  Profile-Line                   PIC X(800).

       WORKING-STORAGE SECTION.
       *> ------- File status variables -------
       01  WS-FILE-STATUS                 PIC XX.
       01  WS-OUTPUT-STATUS               PIC XX.
       01  FS-ACCT                        PIC XX VALUE '00'.
       01  FS-PROF                        PIC XX VALUE '00'.
       01  EOF-IN                         PIC X VALUE 'N'.
       01  ACCT-EOF                       PIC X VALUE 'N'.
       01  PROF-EOF                       PIC X VALUE 'N'.

       *> ------- Menu / input buffers -------
       01  UserChoice                     PIC 9.
       01  UserName                       PIC X(20).
       01  UserPassword                   PIC X(20).
       01  NameLen                        PIC 99.
       01  PassLen                        PIC 99.
       01  TempChar                       PIC X.
       01  I                              PIC 99.
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

       *> Special characters set (double the quote inside)
       01  Specials                       PIC X(40)
           VALUE '!@#$%^&*()-_=+[]{};:'',.<>/?'.

       *> Parse helpers
       01  U-Part                         PIC X(20).
       01  P-Part                         PIC X(20).

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

       PROCEDURE DIVISION.
       Main.
           PERFORM Open-Files
           PERFORM Load-Accounts-From-Disk

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
           PERFORM Load-User-Profile

           *> Main Application Loop
           PERFORM UNTIL WS-MENU-SELECTION = "5" OR EOF-IN = "Y"
               PERFORM Show-Main-Menu
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
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM J-Search-Loop
                       WHEN "2"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Find-Someone-Loop
                       WHEN "3"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Skill-Loop
                       WHEN "4"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Profile-Loop
                       WHEN "5"
                           MOVE "Logging out... Goodbye!" TO WS-MSG
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

           *> Reopen accounts in EXTEND for appends after startup load
           OPEN EXTEND ACCOUNTS-FILE

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
           .

       Close-Files.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           CLOSE PROFILES-FILE
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
       *> LOGIN
       *> -----------------------------
       Do-Login.
           IF Account-Count = 0
               MOVE "No accounts exist. Create an account first." TO WS-MSG
               PERFORM OUT-MSG
               GOBACK
           END-IF

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

       J-Search-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM J-Search-Menu
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

       Find-Someone-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Find-Someone-Menu
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

       Skill-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "5" OR EOF-IN = "Y"
               PERFORM Skill-Menu
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
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Web-Dev-Loop
                       WHEN "2"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Deep-Learning-Loop
                       WHEN "3"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Interview-Loop
                       WHEN "4"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Resume-Loop
                       WHEN "5"
                           MOVE "Returning.." TO WS-MSG
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

       Web-Dev-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Web-Dev-Menu
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

       Deep-Learning-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Deep-Learning-Menu
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

       Interview-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Interview-Menu
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

       Resume-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Resume-Menu
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

       *> -----------------------------
       *> MENU DISPLAY PARAGRAPHS
       *> -----------------------------
       Skill-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " Skill Menu" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 1. Learn Web Development" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 2. Learn Deep Learning" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 3. Learn How To Crack Interview Questions" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 4. Learn How To Optimize Your Resume" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 5. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       J-Search-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Job/Internship Search" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Find-Someone-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Find Someone You Know" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Web-Dev-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Learn Web Development" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Deep-Learning-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Learn Deep Learning" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction..." TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Interview-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Learn How To Crack Interview Questions" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction..." TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Resume-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Learn How To Optimize Your Resume" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction..." TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

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

               MOVE "Enter Years Attended (e.g. 2023-2025): " TO WS-MSG
               PERFORM OUT-MSG
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(InLine) TO 
                   Prof-Edu-Years(Prof-Entry-Index)
           END-PERFORM

           MOVE "Education information updated." TO WS-MSG
           PERFORM OUT-MSG
           .

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
           .

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
           SET UE-Flag            TO 0
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
       *> Load accounts from disk at startup (no GOTO)
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
           .
