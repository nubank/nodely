# Nodely Env Profiling Concerns

## Correctly measuring both sync and async eval

## Profiling demands a mutable place to put the profiled timings

## Profiling requires creating a single shot env

## We didn't scratch at any other interesting opportunities

# Classes of approach

## Shove everything in Metadata

Feels expedient, doesn't feel principled

## Change the implementation of engines so that they accomodate profiling

Feels expensive, most principled

## Sneak in a new implementation of the map data structure that lets us smuggle in timing

Hybrid feel, we're trying to change timing by changing the data structure we reduce in

# How to change the engine impls?

They already embody a bad lower-case p protocol.

Give them a Protocol, each engine implements the protocol between api.v0 and the engine.

Add another protocol that affords us the opportunity to capture eval start and end times for profiling (maybe).

Each engine implements the API protocol and the profiling protocol.

Can compose profiling choice and engine choice by having a no-op impl of the profiling protocol.

## key -> value installation per engine

lazy: assoc the k (node name) onto a derived version of env (derivation from node->value)

manifold: eager assoc futures to result env, futures are lazily evaled on deref, everything must be dereffed!

virtual_workers: eager assoc vfutures to result env, futures start running eagerly, everything must be dereffed!

lazy_scheduling: make a new map, access key lazy initializes k <promise-of v>. Get "evaled" map is get all scheduled keys and wait on them all materializing

applicative: make a new map, access key lazy initialized k <monadic context of expr>. Eval node is extract of the monadic context of that one key, eval full map is poke one key and then extract all monadic contexts lazy initialized
