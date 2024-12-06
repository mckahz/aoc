interface Automaton
  exposes [Automaton, findSubautomata, pushButton, pushButtonN, show, buttonPressesTilConjunctionFires]
  imports
    [ Module.{ Name, Module, Pulse, Level, Memory }
    , ansi.Color.{ fg, bg }
    ]

Automaton : List Module
Sent : { low: Nat, high: Nat }

# unsafe version of get for brevity
get : Automaton, Name -> Module
get = \automaton, name ->
  when automaton |> List.findFirst \module -> module.name == name is
    Ok module -> module
    Err _ -> crash "Automaton doesn't contain module \(name)"

# for part 1 we need to know how many signals were sent.
# This is what this function calculates
pushButtonN : Automaton, Nat -> Sent
pushButtonN = \automaton, n ->
  iter :
    { automaton: Automaton
    , sent: Sent
    , remainingButtonPresses: Nat
    } -> { automaton: Automaton, sent: Sent }
  iter = \state ->
    if state.remainingButtonPresses == 0 then
      { automaton: state.machine
      , sent: state.sent
      }
    else
      { automaton: newAutomaton, sent: newSent } = pushButton state.machine

      iter
        { automaton: newAutomaton
        , remainingButtonPresses: state.remainingButtonPresses - 1
        , sent:
          { low: state.sent.low + newSent.low
          , high: state.sent.high + newSent.high
          }
        }

  { sent } = iter { automaton, sent: { low: 0, high: 0 }, remainingButtonPresses: n }

  sent

# there are 4 flip flop chains, each connected to a central conjunction module.
# Each of these conjunction modules need to fire so that the intermediary node
# between them and gf will fire high, so that gf fires low and rx is activated.
# We don't need the intermediary modules in the subautomaton since the only
# important detail is when the central conjunction modules fire.
findSubautomata : Automaton -> List Automaton
findSubautomata = \automaton ->
  unfold : List Name -> List Name
  unfold = \names ->
    names
    |> List.joinMap \name ->
        get automaton name
        |> .outputs
        |> List.keepIf \output ->
            Module.isFlipFlop (get automaton output)
        |> unfold
    |> List.concat names

  get automaton "broadcaster"
  |> .outputs
  |> List.map \start ->
      flipFlopLoop =
        List.reverse (unfold [start])

      centralModuleName =
        flipFlopLoop
        |> List.map \name -> automaton |> get name
        |> List.joinMap .outputs
        |> List.findFirst \name -> Module.isConjunction (get automaton name)
        |> Result.withDefault ""

      [centralModuleName, "broadcaster"]
      |> List.concat flipFlopLoop
      |> List.map \name -> get automaton name

remembersOnlyHigh : Memory -> Bool
remembersOnlyHigh = \inputs ->
  inputs
  |> Dict.values
  |> List.all \register ->
      register == High

send : Automaton, Pulse -> { automaton: Automaton, pulses: List Pulse }
send = \automaton, pulse ->
  destModuleIdx =
    automaton
    |> List.findFirstIndex \m -> m.name == pulse.to

  when destModuleIdx is
    Err _ -> { automaton, pulses: [] }
    Ok idx ->
      newAutomaton =
        List.update automaton idx \module ->
          { module
          & type:
              when module.type is
                Broadcast -> Broadcast
                FlipFlop _ if pulse.level == High -> module.type
                FlipFlop On -> FlipFlop Off
                FlipFlop Off -> FlipFlop On
                FlipFlop _ -> crash "compiler bug"
                Conjunction inputs ->
                  updatedMemory =
                    inputs
                    |> Dict.insert pulse.from pulse.level
                  Conjunction updatedMemory
          }

      newPulses =
        module = get newAutomaton pulse.to

        outputLevel =
          when module.type is
            Broadcast -> Ok Low

            FlipFlop _ if pulse.level == High ->
                Err FlipFlopDoesntRespondToHigh
            FlipFlop On -> Ok High
            FlipFlop Off -> Ok Low
            FlipFlop _ -> crash "compiler bug"

            Conjunction inputs if remembersOnlyHigh inputs -> Ok Low
            Conjunction _ -> Ok High

        when outputLevel is
          Err _ -> []
          Ok level ->
            module.outputs
            |> List.map \to -> { level, from: module.name, to }

      { automaton: newAutomaton
      , pulses: newPulses
      }

pushButton : Automaton -> { automaton: Automaton, sent: Sent, conjunctionOutputsLow: Bool }
pushButton = \automaton ->
  iter :
    { automaton: Automaton
    , pulses: List Pulse
    , sent: Sent
    , conjunctionOutputsLow: Bool
    } ->
      { automaton: Automaton
      , sent: Sent
      , conjunctionOutputsLow: Bool
      }
  iter = \state ->
    (newAutomaton, newConjunctionOutputsLow, newPulses) =
      List.walk state.pulses (state.automaton, state.conjunctionOutputsLow, [])
        \(oldautomaton, oldConjunctionOutputsLow, oldPulses), pulse ->
          { automaton: newM, pulses: newP } = send oldAutomaton pulse

          # for part 2
          newCOL : Bool
          newCOL =
            conjunctionOutputsLow =
              oldAutomaton
              |> List.any \module ->
                  when module.type is
                    Conjunction inputs if remembersOnlyHigh inputs -> Bool.true
                    _ -> Bool.false
            conjunctionOutputsLow || oldConjunctionOutputsLow

          (newM, newCOL, oldPulses |> List.concat newP)

    # for part 1
    newSent : Sent
    newSent =
      { low: state.sent.low
          + List.countIf newPulses \{ level } -> level == Low
      , high: state.sent.high
          + List.countIf newPulses \{ level } -> level == High
      }

    if newPulses |> List.isEmpty then
      { automaton: newAutomaton
      , sent: newSent
      , conjunctionOutputsLow: newConjunctionOutputsLow
      }
    else
      iter
        { automaton: newAutomaton
        , pulses: newPulses
        , sent: newSent
        , conjunctionOutputsLow: newConjunctionOutputsLow
        }

  iter
    { automaton
    , pulses: [{ level: Low, from: "button", to: "broadcaster" }]
    , sent: { low: 1, high: 0 }
    , conjunctionOutputsLow: Bool.false
    }

# since the only conjunction modules in the subautomatons are the central
# conjunction modules, we only need to find out how many button presses
# are needed to make 1 conjunction module fire- the central conjunction
# module.
buttonPressesTilConjunctionFires : Automaton -> Nat
buttonPressesTilConjunctionFires = \automaton ->
  iter : Automaton, Nat -> Nat
  iter = \oldautomaton, buttonPresses ->
    { automaton: newAutomaton, conjunctionOutputsLow } = pushButton oldAutomaton

    if conjunctionOutputsLow then
      buttonPresses + 1
    else
      iter newAutomaton (buttonPresses + 1)

  iter automaton 0

show : Automaton -> Str
show = \automaton ->
  colored : Name -> Str
  colored = \name ->
    when automaton |> List.findFirst \module -> module.name == name is
      Err _ -> "  " |> bg White
      Ok module ->
        fgColor =
          when module.type is
            Broadcast -> Red

            FlipFlop On -> Green
            FlipFlop Off -> Red

            Conjunction inputs if remembersOnlyHigh inputs -> Green
            Conjunction _ -> Red

        module.name
        |> fg fgColor

  [ ""
  , "┌────────┬─broad──┬────────┐"
  , "│        │        │        │"
  , "v        v        v        v"
  , "\(colored "bq")X─┐    \(colored "dz")X─┐    \(colored "ff")X─┐    \(colored "kg")X─┐"
  , "\(colored "mg")<─┤    \(colored "dc")<─┤    \(colored "vl")>─┤    \(colored "pt")>─┤"
  , "\(colored "dp")<─┤    \(colored "fk")<─┤    \(colored "vx")<─┤    \(colored "vv")>─┤"
  , "\(colored "mh")<─┤    \(colored "sl")<─┤    \(colored "cv")>─┤    \(colored "nc")<─┤"
  , "\(colored "rz")<─┤    \(colored "rp")>─┤    \(colored "jp")>─┤    \(colored "gb")>─┤"
  , "\(colored "tj")>─┼\(colored "qr")┐ \(colored "jb")>─┼\(colored "bf")┐ \(colored "kt")>─┼\(colored "cx")┐ \(colored "ls")>─┼\(colored "gm")┐"
  , "\(colored "nd")<─┤  │ \(colored "kp")<─┤  │ \(colored "hm")>─┤  │ \(colored "lf")<─┤  │"
  , "\(colored "jx")>─┤  │ \(colored "pz")>─┤  │ \(colored "tz")>─┤  │ \(colored "hr")>─┤  │"
  , "\(colored "zz")>─┤  │ \(colored "zg")<─┤  │ \(colored "mf")>─┤  │ \(colored "fq")<─┤  │"
  , "\(colored "pf")>─┤  │ \(colored "bb")>─┤  │ \(colored "sx")>─┤  │ \(colored "qn")>─┤  │"
  , "\(colored "xk")>─┤  │ \(colored "hg")>─┤  │ \(colored "rj")>─┤  │ \(colored "bh")>─┤  │"
  , "\(colored "sf")>─┘  │ \(colored "dl")>─┘  │ \(colored "xt")>─┘  │ \(colored "vq")>─┘  │"
  , "       v        v        v        v"
  , "       \(colored "qk")       \(colored "kr")       \(colored "zs")       \(colored "kf")"
  , "       │        │        │        │"
  , "       └────────┴────────┴────────┴───\(colored "gf")─>rx"
  ]
  |> Str.joinWith "\n"
