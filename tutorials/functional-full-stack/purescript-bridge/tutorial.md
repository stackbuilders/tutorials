# Connecting a Haskell Backend to a Purescript frontend
## Introduction
* Describe a functional full stack, with haskell in the backend and purescript in the frontend

### Motivation
* Decribe type discrepancies as a result of having two different places where store types
* Describe how this fails in the real world
* Describe how this would be in an ideal world

### Basic idea
* Describe main idea: Somehow translate Haskell types into equivalent Purescript types

### purescript-bridge to the rescue
* Describe how purescript-bridge generates Purescript code from Haskell code


## Simple WebApp
* Implement a simple WebApp as an example for the tutorial

### WebApp idea
* Describe the idea

### Backend
* Describe the Haskell backend and API endpoints

### Frontend
* Describe the Purescript frontend and how it uses the API endpoints

### Changing the app
* Implement a minor change on the backend that causes the frontend to fail on runtime
* Fix the frontend
* Show how clunky is having an app this way


## Tutorial
### Connecting the types on the backend to the frontend
* How to change the backend to enable it to use puescript-bridge
* How to change the frontend to use purescript-bridge generated results

### Using generics to simplify communication
* Changing backend endpoints to use generic fromJson/toJson
* Changing frontend endpoints to use generic fromJson/toJson

### Changing the app again
* Implement another minor change on the backend
* Run the new toolchain to auto-extend the change
* The frontend doesn't typecheck. Fix it.
* Everything works!

### Analysing the result
* Describe how the backend and frontend have been simplified, and how much code we have removed from both the backend and the frontend
* Describe how the types are now properly checked as a whole, and the extra safety we have gained

## Conclusion
* Describe how awesome purescript-bridge is
* Next steps, and where to read more
