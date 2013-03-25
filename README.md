HTTPMock
========

Haskell in-process HTTP server for testing HTTP client libraries. HTTPMock
spawns up a web server in a thread that will respond to requests however you
want it to and will record all requests that were made. HTTPMock is loosely
inspired by projects such as [webmock](https://github.com/bblimke/webmock).

Motivation
----------
I frequently write HTTP API clients in Haskell and I have always had a very
difficult time adequately testing these libraries. Most of the time the API
focuses around a few core functions that perform HTTP requests. Previous
attempts to use operational/free monads to stub out HTTP interactions proved to
be much more difficult than expected. I wanted a way to thoroughly test HTTP
clients without having a complicated testing project with an external dummy
service.

Status
------
The API is starting to stabilize. Going to use it in some real-world projects to
see where the pain points are. See the
[Expectations](src/Network/HTTPMock/Expectations.hs) module and some of the
[tests](test/Network/HTTPMockSpec.hs) for some usage examples for now.
