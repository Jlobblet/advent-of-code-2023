parse =: {{
    NB. split lines and remove game number
    y =. <@(#~ [: +/\ _2 |.!.0 ': ' E. ]);._2 y
    NB. split on semicolons
    y =. '; '&splitstring L: 0 y
    NB. split on commas
    y =. ', '&splitstring L: 0 y
    NB. split on spaces and parse numbers
    y =. ((([: ". 0 {:: ]) ; ([: {. 1 {:: ])) ' ' splitstring ]) L:0 y
    NB. unbox and transpose
    y =. |:@:; L: 2 > L: 2 y
    y
}}

p =: {{
    'scores colours' =. y
    ('rgb' i. ~. colours) {^:_1 colours >.//. scores
}}

p1g =: [: *./ 12 13 14 >: p
p1 =: {{ +/ (# >:@i.@#) ; p1g L: 1 y }}

p2g =: [: */ p
p2 =: {{ +/ ; p2g L: 1 y }}

d =: parse fread getenv 'INPUT'
echo p1 d
echo p2 d
exit ''
