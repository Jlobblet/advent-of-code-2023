t =: fread getenv 'INPUT'

matches =: {{ {{ +/ ({. e.&:> {:) <;._1 }. y}}"1 (_) ". ,;._2 y }}
m =: matches t

NB. part one - keep nonzero scores, double with power, sum
points =: {{ +/ 2 ^ <: (#~*) y }}
echo points m

NB. part two
initial =: 0 ; (1 $~ # m) ; m
step =: {{
    'count cards scores' =. y
    _2 Z: 0 = # cards
    count =. count + {. cards
    cards =. (+ ({. cards) * ({. scores) > i.@#) }. cards
    scores =. }. scores
    count ; cards ; scores
}}

echo 0&{:: F. step initial

exit''
