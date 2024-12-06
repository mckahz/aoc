interface Module
  exposes [Name, Module, Pulse, Level, isConjunction, isFlipFlop, isBroadcast]
  imports []

Name : Str
Switch : [On, Off]
Register : Level
Memory : Dict Name Register
Module :
  { name: Name
  , outputs: List Name
  , type: [FlipFlop Switch, Conjunction Memory, Broadcast]
  }
Pulse :
  { from: Name
  , to: Name
  , level: Level
  }

Level : [High, Low]

isBroadcast : Module -> Bool
isBroadcast = \module ->
  when module is
    { type: Broadcast } -> Bool.true
    _ -> Bool.false

isConjunction : Module -> Bool
isConjunction = \module ->
  when module is
    { type: Conjunction _ } -> Bool.true
    _ -> Bool.false

isFlipFlop : Module -> Bool
isFlipFlop = \module ->
  when module is
    { type: FlipFlop _ } -> Bool.true
    _ -> Bool.false
