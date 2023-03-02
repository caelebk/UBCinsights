---
name: User Stories
about: Use this template for user stories submission
title: "C3 Phase 1: User Stories"
labels: []
assignees: ""
---

## Frontend Selection
In two to three sentences, give a description on the frontend you are to build. Is it going to be a Web frontend? A Discord bot? What external packages, libraries, or frameworks would you need?

Do this **before** you go on to writing your user stores!

Web frontend.

## User Stories + DoDs  
Make sure to follow the *Role, Goal, Benefit* framework for the user stories and the *Given/When/Then* framework for the Definitions of Done! For the DoDs, think about both success and failure scenarios. You can also refer to the examples DoDs in [C3 spec](https://sites.google.com/view/ubc-cpsc310-22w2/project/checkpoint-3).

### User Story 1

As a Student, I want to look for courses with grade averages above 90, so that I can find the easiest courses to
boost my GPA.

#### Definitions of Done(s)

Scenario 1: Correct course query\
Given: The user is on the query course dataset page. \
When: The user selects a valid dataset ID from a dropdown, selects the course property to filter on, 
selects the filter to be applied, inputs the necessary values for the filter, clicks the add filter to query button, and clicks the query button. \
Then: The application remains on the query page and presents a table of courses that fit the criteria below the query button.

Scenario 2: Incorrect filter value for query \
Given: The user is on the query course dataset page \
When: The user selects a valid dataset ID from a dropdown, selects the grade property to filter on, selects the greater than filter,
inputs an invalid value, and selects add filter to query.\
Then: The application remains on the query page and presents a dialog informing the user an error had occured and that the filter being added had an invalid value.

### User Story 2

As a Student, I want to be able to write and submit custom query, so that find all the courses that match the query.

#### Definitions of Done(s)
Scenario 1: Valid query
Given: User types in a valid query into a text submission
When: User clicks submit button
Then: The application returns the data that matches the query to the current page

Scenario 2: Valid Complex Query
Given: User types in a valid complex query into a text submission
When: User clicks submit button
Then: Application returns the data that matches the query to the current page

Scenario 3: Invalid Query
Given: User types in an invalid query (typos, query that would have 5000+ results, etc)
When: User clicks submit button
Then: Application returns error message

### Others

You may provide any additional user stories + DoDs in this section for general TA feedback.

But these will not be graded.
