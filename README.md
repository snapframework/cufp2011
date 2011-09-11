# CUFP 2011 Tutorial: Web programming in Haskell with the Snap Framework

This repository contains code for the CUFP 2011 tutorial
[The Snap Framework for web applications in Haskell](http://cufp.org/conference/sessions/2011/t7-snap-framework-web-applications-haskell-gregory).
You will be building a simplified web application implementing multi-user chat.


## Getting started

From the project root directory, running `cabal install` should build the
`snap-chat` and `snap-chat-sample` executables. You can find these executables
in the `dist/build/` subdirectory. To run the sample application (to see what
the result should look like), run
`dist/build/snap-chat-sample/snap-chat-sample` and point your browser to
[`http://localhost:8000/`](http://localhost:8000/).

The sample implementation of the code you're expected to provide can be found
in the `sample-implementation/` directory; however **please don't peek** unless
you get really stuck. We'll be working through the code together.

Test code (so you can test your implementation against the expected result) can
be found in the `test/` directory; to run it:

    cd test
    cabal install
    ./runTestsAndCoverage.sh


## Specifications

Snap Chat is split into three functional components:

  * a data model, consisting of Haskell code for interacting with a chat
    channel; see `src/Snap/Chat/Types.hs`, `src/Snap/Chat/ChatRoom.hs`, and
    `src/Snap/Chat/Message.hs` for the implementation here.

  * a JSON API that provides access to the data model; this consists of HTTP
    `POST` handlers for the URL endpoints:

    * `/api/join`, to join the chat channel
    * `/api/leave`, to leave the chat channel
    * `/api/fetch`, to fetch new messages written to the chat channel (using
      long-polling)
    * `/api/write`, to write a message to the chat channel.

    These API endpoints will be described in further detail later.

  * an HTML5/Javascript front end that calls the JSON api.

For brevity and to bound the amount of work students are expected to do in the
short tutorial, most of the code is already provided, including all of the
JavaScript (this is a Haskell tutorial!), the data model code, and all of the
datatypes. More advanced students who quickly breeze through the small amount
of work provided can check out the "extra credit" section at the end of this
document.


### API documentation

Each of the four API calls (`/api/join`, `/api/leave`, `/api/fetch`,
`/api/write`) share a similar structure: they all respond only to POST requests
containing a UTF8-encoded JSON document as the request body; i.e. the
`Content-Type` of the input request is `application/json`, and they all produce
a JSON document as the result. The output responses have the following common
structure: either they succeed, producing a document like the following:

    {
      "status": "ok",
      "session": "DF1642....038A=",
      "response": { ...some json object... }
    }

When they fail, the output document looks like this:

    {
      "status": "failure",
      "statusCode": "some_failure",
      "reason": "blah blah blah blah."
    }

The "session" variable above deserves some special mention: upon successfully
joining the chat room, the user will receive an encrypted session token, which
will be used on subsequent requests to re-authenticate the user with the chat
room. Upon each response, a fresh session token will be generated. The contents
of the session token are opaque to the API user, but can be decrypted on the
server-side.

The data type for the encoded session looks like this (see
`src/Snap/Chat/Internal/API/Types.hs`):

    data EncodedSession = EncodedSession {
          _sessionToken :: UserToken
        , _sessionTime  :: EpochTime
        , _apiUser      :: UserName
        }

A session will only be considered valid if:

  * the session time is not too old.

  * the user name and user token match what is contained in the chat room data
    model.


#### /api/join

The "join" command is responsible for connecting a user to the chat room with a
given user name.

Example request:

    { "desiredUserName": "george" }

Example successful response:

    {
      "status": "ok",
      "session": "abc.....def",
      "response": {}
    }

Example unsuccessful response:

    {
      "status": "failure",
      "statusCode": "user_already_exists",
      "reason": "Cannot log in; a user with that name is already connected \
                 to the channel."
    }


#### /api/leave

The "leave" command logs the user out of the chat room.

Example request:

    {
      "session": "abc.....def",
      "requestData": {}
    }

Example successful response:

    {
      "status": "ok",
      "session": "abc.....def",
      "response": {}
    }

#### /api/fetch

The "fetch" command gets new messages from the chat room, blocking for up to 50
seconds before it returns with a list of new messages, possibly empty.

Example request:

    {
      "session": "abc.....def",
      "requestData": {}
    }

Example successful response:

    {
      "status": "ok",
      "session": "abc.....def",
      "response": { "messages": [ ...messages... ] }
    }

The JSON type of messages is as follows:

    { "contents": {
        "type": <<one of "talk", "action", "join", "leave">>,
        "text": "message text"
        },
      "user": "fred",
      "time": <<posix timestamp>>
    }


#### /api/write

The "write" command writes a message to the chat room.

Example request:

    {
      "session": "abc.....def",
      "requestData": {
          "type": <<one of "talk", "action">>,
          "text": "message text"
        }
    }

Example successful response:

    {
      "status": "ok",
      "session": "abc.....def",
      "response": {}
    }


## What students must implement

Several functions and instances in the source tree have been marked as
"toBeImplemented". Tutorial attendees must implement:

  * `ToJSON` and `FromJSON` instances for the types in
    `src/Snap/Chat/Internal/API/Types.hs`.
    
  * `ToJSON` and `FromJSON` instances for the message types in
    `src/Snap/Chat/Internal/Types.hs`.
    
  * all of the stubbed-out functions in `src/Snap/Chat/API/Handlers.hs`.

You'll need to use the functions from `src/Snap/Chat/ChatRoom.hs`, which
contains all of the "business logic" for the chat rooms.


## Extra credit

If you finish early and get bored, here are some ideas for "extra-credit"
assignments:

  * extend the chat channel with a user list

  * add private user-to-user messages

  * support multiple chat rooms
