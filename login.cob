identification division.
program-id. login.

environment division.
input-output section.
file-control.
   select input-file assign to "input.txt"
       organization is line sequential
       file status is ws-file-status.

data division.
file section.
fd input-file.
01 input-record pic x(20).

working-storage section.
01 ws-file-status pic xx.
01 ws-eof pic x value "N".
01 ws-menu-selection pic x.
01 ws-skill-selection pic x.
01 ws-input-value pic x.

procedure division.
main-loop.
    open input input-file
    if ws-file-status not = "00"
        display "Error opening input file: " ws-file-status
        stop run
    end-if
    
    perform until ws-menu-selection = "4" or ws-eof = "Y"
        perform main-menu
        perform read-input-file
        if ws-eof not = "Y"
            move ws-input-value to ws-menu-selection
            display "Selected: " ws-menu-selection
            evaluate ws-menu-selection
                when "1"
                    move "0" to ws-menu-selection
                    perform j-search-loop
                when "2"
                    move "0" to ws-menu-selection
                    perform find-someone-loop
                when "3"
                    move "0" to ws-menu-selection
                    perform skill-loop
                when "4"
                    display "Exiting program..."
                when other
                    display "Invalid choice: " ws-menu-selection
                    move "0" to ws-menu-selection
            end-evaluate
        end-if
    end-perform
    
    close input-file
    stop run.

read-input-file.
    read input-file into input-record
    evaluate ws-file-status
        when "00"
            move input-record(1:1) to ws-input-value
        when "10"
            move "Y" to ws-eof
            display "End of file reached"
        when other
            display "Error reading file: " ws-file-status
            move "Y" to ws-eof
    end-evaluate.

j-search-loop.
    perform until ws-menu-selection = "1" or ws-eof = "Y"
       perform j-search-menu
       perform read-input-file
       if ws-eof not = "Y"
           move ws-input-value to ws-menu-selection
           display "Selected: " ws-menu-selection
           evaluate ws-menu-selection
               when "1"
                   display "Returning..."
               when other 
                   display "Invalid choice: " ws-menu-selection
                   move "0" to ws-menu-selection
           end-evaluate
       end-if
    end-perform.

find-someone-loop.
    perform until ws-menu-selection = "1" or ws-eof = "Y"
       perform find-someone-menu
       perform read-input-file
       if ws-eof not = "Y"
           move ws-input-value to ws-menu-selection
           display "Selected: " ws-menu-selection
           evaluate ws-menu-selection
               when "1"
                   display "Returning..."
               when other 
                   display "Invalid choice: " ws-menu-selection
                   move "0" to ws-menu-selection
           end-evaluate
       end-if
    end-perform.

skill-loop.
    perform until ws-menu-selection = "5" or ws-eof = "Y"
       perform skill-menu
       perform read-input-file
       if ws-eof not = "Y"
           move ws-input-value to ws-menu-selection
           display "Selected: " ws-menu-selection
           evaluate ws-menu-selection
               when "1"
                   move "0" to ws-menu-selection
                   perform web-dev-loop
               when "2" 
                   move "0" to ws-menu-selection
                   perform deep-learning-loop
               when "3" 
                   move "0" to ws-menu-selection
                   perform interview-loop
               when "4"
                   move "0" to ws-menu-selection
                   perform resume-loop
               when "5"
                   display "Returning.."
               when other 
                   display "Invalid choice: " ws-menu-selection
                   move "0" to ws-menu-selection
           end-evaluate
       end-if
    end-perform.

web-dev-loop.
    perform until ws-menu-selection = "1" or ws-eof = "Y"
       perform web-dev-menu
       perform read-input-file
       if ws-eof not = "Y"
           move ws-input-value to ws-menu-selection
           display "Selected: " ws-menu-selection
           evaluate ws-menu-selection
               when "1"
                   display "Returning..."
               when other 
                   display "Invalid choice: " ws-menu-selection
                   move "0" to ws-menu-selection
           end-evaluate
       end-if
    end-perform.

deep-learning-loop.
    perform until ws-menu-selection = "1" or ws-eof = "Y"
       perform deep-learning-menu
       perform read-input-file
       if ws-eof not = "Y"
           move ws-input-value to ws-menu-selection
           display "Selected: " ws-menu-selection
           evaluate ws-menu-selection
               when "1"
                   display "Returning..."
               when other 
                   display "Invalid choice: " ws-menu-selection
                   move "0" to ws-menu-selection
           end-evaluate
       end-if
    end-perform.

interview-loop.
    perform until ws-menu-selection = "1" or ws-eof = "Y"
       perform interview-menu
       perform read-input-file
       if ws-eof not = "Y"
           move ws-input-value to ws-menu-selection
           display "Selected: " ws-menu-selection
           evaluate ws-menu-selection
               when "1"
                   display "Returning..."
               when other
                   display "Invalid choice: " ws-menu-selection
                   move "0" to ws-menu-selection
           end-evaluate
       end-if
    end-perform.

resume-loop.
    perform until ws-menu-selection = "1" or ws-eof = "Y"
       perform resume-menu
       perform read-input-file
       if ws-eof not = "Y"
           move ws-input-value to ws-menu-selection
           display "Selected: " ws-menu-selection
           evaluate ws-menu-selection
               when "1"
                   display "Returning..."
               when other 
                   display "Invalid choice: " ws-menu-selection
                   move "0" to ws-menu-selection
           end-evaluate
       end-if
    end-perform.

main-menu.
    display "==========================="
    display " Student Manager Menu"
    display " 1. Job/Internship Search"
    display " 2. Find Someone You Know"
    display " 3. Learn a New Skill"
    display " 4. Exit"
    display "==========================="
    display "Reading choice from file...".        

skill-menu.
    display "==========================="
    display " Skill Menu"
    display " 1. Learn Web Development"
    display " 2. Learn Deep Learning"
    display " 3. Learn How To Crack Interview Questions"
    display " 4. Learn How To Optimize Your Resume"
    display " 5. Return"
    display "==========================="
    display "Reading choice from file...".        

j-search-menu.
    display "==========================="
    display "Job/Internship Search"
    display "Under Construction"
    display "1. Return"
    display "==========================="
    display "Reading choice from file...". 

find-someone-menu.
    display "==========================="
    display "Find Someone You Know"
    display "Under Construction"
    display "1. Return"
    display "==========================="
    display "Reading choice from file...". 

web-dev-menu.
    display "==========================="
    display "Learn Web Development"
    display "Under Construction"
    display "1. Return"
    display "==========================="
    display "Reading choice from file...". 

deep-learning-menu.
    display "==========================="
    display "Learn Deep Learning"
    display "Under Construction..."
    display "1. Return"
    display "==========================="
    display "Reading choice from file...". 

interview-menu.
    display "==========================="
    display "Learn How To Crack Interview Questions"
    display "Under Construction..."
    display "1. Return"
    display "==========================="
    display "Reading choice from file...". 

resume-menu.
    display "==========================="
    display "Learn How To Optimize Your Resume"
    display "Under Construction..."
    display "1. Return"
    display "==========================="
    display "Reading choice from file...".
    