interface Day19.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      , part1TestAns, part2TestAns
      , part1Ans, part2Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test : Str
      , parser.Core.{ Parser, map, keep, skip, oneOf, sepBy, const, many }
      , parser.String.{ string, parseStr, digits }
      ]

part1TestAns = 19114
part1Ans = 472630
part2TestAns = 167409079868000
part2Ans = 116738260946855

Name : Str
Part :
  { x: U64
  , m: U64
  , a: U64
  , s: U64
  }
Action :
  [ SendTo Name
  , Reject
  , Accept
  ]
Rule :
  { component : Component
  , condition : Condition
  , number : U64
  , action : Action
  }
Component : [X, M, A, S]
Condition : [LT, GT]
Workflows : Dict Name (List Rule, Action)
PartRange :
  { x: (U64, U64)
  , m: (U64, U64)
  , a: (U64, U64)
  , s: (U64, U64)
  }

alpha : Parser (List U8) Str
alpha =
  "abcdefghijklmnopqrstuvwxyz"
  |> Str.graphemes
  |> List.map string
  |> oneOf

alphas : Parser (List U8) Str
alphas =
  many alpha
  |> map \chars -> chars |> Str.joinWith ""

parse : Str -> (Workflows, List Part)
parse = \str ->
  action : Parser (List U8) Action
  action =
    oneOf
    [ string "A" |> map \_ -> Accept
    , string "R" |> map \_ -> Reject
    , alphas |> map SendTo
    ]

  condition : Parser (List U8) Condition
  condition =
    oneOf
      [ string "<" |> map \_ -> LT
      , string ">" |> map \_ -> GT
      ]

  component : Parser (List U8) Component
  component =
    oneOf
      [ string "x" |> map \_ -> X
      , string "m" |> map \_ -> M
      , string "a" |> map \_ -> A
      , string "s" |> map \_ -> S
      ]

  digitsU64 : Parser (List U8) U64
  digitsU64 =
    digits |> map Num.toU64

  rule : Parser (List U8) Rule
  rule =
    const \comp -> \cond -> \num -> \a ->
      { condition: cond
      , component: comp
      , action: a
      , number: num
      }
    |> keep component
    |> keep condition
    |> keep digitsU64
    |> skip (string ":")
    |> keep action

  rules : Parser (List U8) (List Rule)
  rules =
    rule |> sepBy (string ",")

  workflow : Parser (List U8) (Name, (List Rule, Action))
  workflow =
    const \name -> \rs -> \a ->
      (name, (rs, a))
    |> keep alphas
    |> skip (string "{")
    |> keep rules
    |> skip (string ",")
    |> keep action
    |> skip (string "}")

  workflows : Parser (List U8) Workflows
  workflows =
    workflow
    |> sepBy (string "\n")
    |> map Dict.fromList

  part : Parser (List U8) Part
  part =
    const \x -> \m -> \a -> \s ->
      { x, m, a, s }
    |> skip (string "{x=")
    |> keep digitsU64
    |> skip (string ",m=")
    |> keep digitsU64
    |> skip (string ",a=")
    |> keep digitsU64
    |> skip (string ",s=")
    |> keep digitsU64
    |> skip (string "}")

  parts : Parser (List U8) (List Part)
  parts =
    part |> sepBy (string "\n")

  all : Parser (List U8) (Workflows, List Part)
  all =
    const \ws -> \ps ->
      (ws, ps)
    |> keep workflows
    |> skip (string "\n\n")
    |> keep parts
    |> skip (string "\n")

  when parseStr all str is
    Ok i -> i
    Err e ->
      dbg e
      crash "invalid input"

addPart : Part -> U64
addPart = \{ x, m, a, s } -> x + m + a + s

fulfils : Part, Rule -> Bool
fulfils = \part, rule ->
  getter =
    when rule.component is
      X -> .x
      M -> .m
      A -> .a
      S -> .s

  when rule.condition is
    LT -> getter part < rule.number
    GT -> getter part > rule.number

part1 : (Workflows, List Part) -> U64
part1 = \(workflows, parts) ->
  isAccepted : Part, Name -> Bool
  isAccepted = \part, workflow ->
    (rules, finalAction) =
      when workflows |> Dict.get workflow is
        Ok x -> x
        Err _ -> crash "invalid workflow name \(workflow)"

    action =
      rules
      |> List.walkUntil (Err NoConditionsFound) \a, rule ->
          if part |> fulfils rule then
            Break (Ok rule.action)
          else
            Continue a
      |> Result.withDefault finalAction

    when action is
      Accept -> Bool.true
      Reject -> Bool.false
      SendTo workspace -> part |> isAccepted workspace

  parts
  |> List.keepIf \part -> part |> isAccepted "in"
  |> List.map addPart
  |> List.sum

splitRange : PartRange, Rule ->
  { rangeToActUpon : [Lower, Upper]
  , action : Action
  , lowerRange : PartRange
  , upperRange : PartRange
  }
splitRange = \partRange, rule ->
  set : PartRange, [Lower, Upper], U64 -> PartRange
  set = \pr, range, to ->
    when (rule.component, range) is
      (X, Upper) -> { pr & x: (to, pr.x.1) }
      (X, Lower) -> { pr & x: (pr.x.0, to) }
      (M, Upper) -> { pr & m: (to, pr.m.1) }
      (M, Lower) -> { pr & m: (pr.m.0, to) }
      (A, Upper) -> { pr & a: (to, pr.a.1) }
      (A, Lower) -> { pr & a: (pr.a.0, to) }
      (S, Upper) -> { pr & s: (to, pr.s.1) }
      (S, Lower) -> { pr & s: (pr.s.0, to) }

  when rule.condition is
    LT ->
      lowerRange = partRange |> set Lower (rule.number - 1)
      upperRange = partRange |> set Upper rule.number
      { lowerRange, upperRange, action: rule.action, rangeToActUpon : Lower }

    GT ->
      lowerRange = partRange |> set Lower rule.number
      upperRange = partRange |> set Upper (rule.number + 1)
      { lowerRange, upperRange, action: rule.action, rangeToActUpon : Upper }

part2 : (Workflows, List Part) -> U64
part2 = \(workflows, _) ->
  countAccepted : PartRange, Name -> U64
  countAccepted = \partRange, workflow ->
    noParts : Bool
    noParts =
      [.x, .m, .a, .s]
      |> List.all \getter -> getter partRange == (0, 0)

    if noParts then
      0
    else
      (rules, finalAction) =
        when workflows |> Dict.get workflow is
          Ok x -> x
          Err _ -> crash "invalid workflow name \(workflow)"

      (rangesAndActions, leftoverRange) =
        rules
        |> List.walk ([], partRange) \(rsAndAs, loRange), rule ->
            { lowerRange, upperRange, action, rangeToActUpon } =
              splitRange loRange rule

            when rangeToActUpon is
              Upper ->
                ( rsAndAs |> List.append (upperRange, action)
                , lowerRange
                )
              Lower ->
                ( rsAndAs |> List.append (lowerRange, action)
                , upperRange
                )

      countFromLeftOver =
        when finalAction is
          Accept -> possibilities leftoverRange
          Reject -> 0
          SendTo name -> countAccepted leftoverRange name

      rangesAndActions
      |> List.walk countFromLeftOver \count, (otherPartRange, action) ->
          accepted =
            when action is
              Accept -> possibilities otherPartRange
              Reject -> 0
              SendTo name -> countAccepted otherPartRange name
          accepted + count

  range = (1, 4000)
  countAccepted { x: range, m: range, a: range, s: range } "in"

possibilities : PartRange -> U64
possibilities = \range ->
  xPos = (1 + range.x.1 - range.x.0)
  mPos = (1 + range.m.1 - range.m.0)
  aPos = (1 + range.a.1 - range.a.0)
  sPos = (1 + range.s.1 - range.s.0)
  xPos * mPos * aPos * sPos
