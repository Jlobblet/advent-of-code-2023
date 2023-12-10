parse =: [: x: _ ". >;._2

extrapolate =: {{ +/, (*"1 [: (>: ^&:x: i.) 1 { $) (%."1 2 [: (^/~ >:) [: x: [: i. 1 { $) y }}

parsed =: parse fread getenv 'INPUT'
echo extrapolate parsed
echo extrapolate |."1 parsed
exit ''
