# Project1-Epic8

## Features

**Messaging System (Week 8)**
- Send messages to connected users only
- 200 char limit with auto-truncation
- Validates recipient is an accepted connection
- Messages saved to Messages.dat (sender|recipient|message)

**Previous Features**
- Job posting and browsing
- Connection requests (send/accept/reject)
- Network view with profiles

## Compile & Run
```bash
cobc -x -o incollege InCollege.cob     
./incollege
```

## Testing

Test cases cover:
- Successful messaging between connected users
- Rejection of messages to non-connected users
- Rejection of messages to pending connections
- 200 character limit validation
- Multiple users and message scenarios

**IMPORTANT**: Clear all .dat files before each test case for clean state.
