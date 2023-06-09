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

We will implement a web frontend, allowing the user to query UBC courses. Currently, we plan on using libraries like React for easier UI development and SASS to have better formatted CSS. We will potentially add
Bootstrap if we decide to use some nicely styled UI components or icons.

## User Stories + DoDs  
Make sure to follow the *Role, Goal, Benefit* framework for the user stories and the *Given/When/Then* framework for the Definitions of Done! For the DoDs, think about both success and failure scenarios. You can also refer to the examples DoDs in [C3 spec](https://sites.google.com/view/ubc-cpsc310-22w2/project/checkpoint-3).

### User Story 1

As a Student, I want to look for courses with grade averages above 90, so that I can find the easiest courses to
boost my GPA.

#### Definitions of Done(s)

Scenario 1: Correct course query\
Given: The user is on the query course dataset page. \
When: The user selects a valid dataset ID from a dropdown, selects the greater than filter to be applied,
selects the course grade avg property to filter on, inputs 90 for the filter value, and clicks the query button. \
Then: The application remains on the query page and presents a table of sections 
that fit the criteria below the query button.

Scenario 2: Incorrect filter value for query \
Given: The user is on the query course dataset page \
When: The user selects a valid dataset ID from a dropdown,
selects the greater than filter,
selects the grade avg property to filter on, 
inputs an string value, and clicks the query button.\
Then: The application remains on the query page and presents a dialog informing 
the user an error had occurred with an error message indicating invalid value.

### User Story 2

As a Professor, I want to be able to find a room that fits a classroom of 150 students for a lecture, so that I have a room for big enough for students to come to lecture.

#### Definitions of Done(s)
Scenario 1: Correct room query \
Given: User is on query room dataset page \
When: User selects a valid room dataset, 
selects the equal to filter type,
selects seats property to filter on, 
inputs 150 for the filter value, 
and clicks the query button \
Then: The application remains on the query page 
and presents a table of rooms that fit the criteria selected by the user.

Scenario 2: Incorrect filter value \
Given: User is on query room dataset page \
When: User selects a valid room dataset, 
selects equal to filter type,
selects seat property to filter on, 
inputs invalid string value, 
and clicks the query button. \
Then: Application stays on room query page and 
returns a dialog informing the user an occur has occurred
with error message indicating an invalid value.

### Others

You may provide any additional user stories + DoDs in this section for general TA feedback.

But these will not be graded.
