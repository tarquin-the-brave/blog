+++
title = "Testing Microservices with Mockserver"
date = 2020-02-25T10:07:46Z
images = []
tags = []
categories = []
draft = true
+++

[Mockserver][mockserver] is a great piece of tooling that takes a huge amount
of heavy lifting away from testing microservices.

There's no need to mock out downstream dependencies or client APIs.  There's no
need to write complex test code to synchronously handle the mechanics of
requests going back and forward while also trying to embody some comprehensible
declaration of the designed behaviour.  It does about as much of the mechanics
of testing for you as it's possible for it to do.

In all honesty, it was my favourite piece of technology I came across in 2019.

In this post I'll walk through a patten I've used a few times to quickly get
rather effective testing of a microservice that drives its API.

# Test Setup

The test deployment has three elements:
* Your microservice,
* A test driver containing/running your test code, and
* A [mockserver][mockserver] instance.

I've used [docker-compose][doco] to run the containers and manage the routing
between them, but any container runtime[^1] would do the job.

[![](https://mermaid.ink/img/eyJjb2RlIjoiZ3JhcGggQlRcbnN1YmdyYXBoIGRvY2tlci1jb21wb3NlXG50ZXN0LXJ1bm5lci0tPnxcInRlc3QtcnVubmVycHJvZ3JhbXMgbW9ja3NlcnZlclwifG1vY2tzZXJ2ZXJcbm1vY2tzZXJ2ZXItLS18XCJtb2Nrc2VydmVyIG1vY2tzIG91dCB0aGUgbWljcm9zZXJ2aWNlIGRlcGVuZGVuY2llcyBhbmQgcmVjb3JkcyBtZXNzYWdlc1wifGEobWljcm9zZXJ2aWNlKVxudGVzdC1ydW5uZXItLT58XCJ0ZXN0LXJ1bm5lciBraWNrcyBtaWNyb3NlcnZpY2UgQVBJIHRvIHN0YXJ0IHRlc3RcInxhKG1pY3Jvc2VydmljZSlcbmVuZFxuXHRcdCIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0In0sInVwZGF0ZUVkaXRvciI6ZmFsc2V9)](https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiZ3JhcGggQlRcbnN1YmdyYXBoIGRvY2tlci1jb21wb3NlXG50ZXN0LXJ1bm5lci0tPnxcInRlc3QtcnVubmVycHJvZ3JhbXMgbW9ja3NlcnZlclwifG1vY2tzZXJ2ZXJcbm1vY2tzZXJ2ZXItLS18XCJtb2Nrc2VydmVyIG1vY2tzIG91dCB0aGUgbWljcm9zZXJ2aWNlIGRlcGVuZGVuY2llcyBhbmQgcmVjb3JkcyBtZXNzYWdlc1wifGEobWljcm9zZXJ2aWNlKVxudGVzdC1ydW5uZXItLT58XCJ0ZXN0LXJ1bm5lciBraWNrcyBtaWNyb3NlcnZpY2UgQVBJIHRvIHN0YXJ0IHRlc3RcInxhKG1pY3Jvc2VydmljZSlcbmVuZFxuXHRcdCIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0In0sInVwZGF0ZUVkaXRvciI6ZmFsc2V9)

## Mockserver

The key element here is [mockserver][mockserver]. It looks after the mechanics
of mocking out the dependencies of a service.  It's controlled by your test
code over its API. You tell it what messages to expect an how to respond to
them ahead of time. It records what messages have come and gone during the
test.  Your tests can send a request to verify the expectations have been met
or partially met.

The result is that each test case ends up looking like:
* A bunch of requests to mockserver, via the client you've chosen, or generated
  from the [openAPI spec][openapispec], to establish the requests that you
  expect to see and how to respond to each,
* A call to an endpoint on your microservice's API, and
* A call to mockserver to verify that the expected messages were received.

Mockserver takes away the need to define in the test code all the mechanics
around what messages should be where, when. You define what to expect upfront,
mockserver records if there's anything the matter with the messages, and at the
end, you can ask it whether the expectations were met.

## The Test Runner

We run the test code in its own container to make it easy to control the
environment and to make networking easier when we introduce
[docker-compose][doco] to manage the setup.

We define a `Dockerfile` for our test runner that builds a container with the
test code and its dependencies. `docker-compose` can then rebuild the test
container and run the tests in one [command](#running-the-tests).

A result of using mockserver, and this test setup, is that the test code is
fairly minimal and there's no real "test framework" required.  Once you have a
mockserver client library[^2], and a client library for your microservice (or
decide that defining the requests to kick off a test on the fly) there's not
much more to the tests.

I've written the code for this test runner in Python using [pytest][pytest].
It worked quite well.  But, as the test code only makes a series of requests,
it's not important.

## Your Microservice

Configure your microservice with mockserver as the target for all downstream
traffic.

The URL of mockserver corresponds to the service name and the port it exposes,
defined in the [docker-compose file](#docker-compose-example).

# Docker Compose Example

@@TODO Example docker-compose file here...

In this example, under `services:` there's an entry for each of the containers
in this setup.
* The test runner entry defines the directory containing the Dockerfile and
  build context, and the command to run the tests.
* The microservice entry mounts a config file in the container containing data
  to set mockserver as the downstream domains, and allows the microservice
  image to be defined as an environment variable.
* The mockserver entry exposes a port and passes that as an argument to
  mockserver.

_There's a further `networks:` section which defines the subnet which is
necessary because the default network for docker is `172.<n>.0.0/16`, where
`<n>` goes up from 16, and those subnets clash with our internal LAN._

# Running The Tests

@@TODO docker compose commands...

[^1]: You could do this in [kubernetes](https://kubernetes.io/). There are
  miniature kubernetes implementations that you can install on your dev machine
  or run in CI: [minikube](https://github.com/kubernetes/minikube),
  [k3s](https://k3s.io/), [microk8s](https://microk8s.io/). You could put the
  [elements](#test-setup) in
  [Pods](https://kubernetes.io/docs/concepts/workloads/pods/pod-overview/) or
  [Deployments](https://kubernetes.io/docs/concepts/workloads/controllers/deployment/)
  and route them to each other via
  [Services](https://kubernetes.io/docs/concepts/services-networking/service/).
  None of the test code would have to change. To me that seemed more effort
  than writing a short docker-compose file for this small setup.  Mockserver
  published [a helm chart](http://www.mock-server.com/where/kubernetes.html)
  which could be useful.  Mockserver could be used in a larger test setup on a
  persistent kubernetes cluster.
[^2]: There's plenty of client libraries out there for mockserver.  [Python has
  a few](https://pypi.org/search/?q=mockserver), I used a couple and they
  didn't actually work, so I had to patch them.  I'd recommending generating
  you're own from mockserver's [openAPI
  spec](https://app.swaggerhub.com/apis/jamesdbloom/mock-server-openapi/5.0.x)
  using [openAPI Generator](https://github.com/OpenAPITools/openapi-generator).

[mockserver]: http://www.mock-server.com/
[openapispec]: https://app.swaggerhub.com/apis/jamesdbloom/mock-server-openapi/5.0.x
[doco]: https://docs.docker.com/compose/
[pytest]: https://docs.pytest.org/en/latest/

