# Reactive Poker

This is an attempt at building an online poker game using Scala Play, Akka Actors,
WebSockets and AngularJS for the frontend.

## Current state: Proof of Concept

The current state is that I proofed my point of using de technologies I choose to use. Taking turns and
checking to end your turn works, including a mechanism to timeout after 10 seconds. Joining a table also
works (currently there is 1 table). On the poker-side, there's some code to extract poker hands using
the marvelous Scala pattern matching mechanism. It of course needs some serious tests to make sure it
works as expected in every situation.

The application can be run, you can join with a name and start the game. In the end the game will show
you the hand combination that you got. 