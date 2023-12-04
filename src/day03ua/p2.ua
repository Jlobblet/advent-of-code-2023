# uiua version 0.5.1

# Open input file and read to string
&fras &var "INPUT"
# Parse
⊜∘↧⊙(≠@\n.)+1\+=@\n.
Lines ←
×↯:+1⇡/×.△. ↧⊃(≥@0)(≤@9) Lines
# Unique IDs for each number
⍥(↥×⊙(>0.)⬚0↻0_1.)⊡1△.
Numbers ←
Gears ← ×=@*Lines ⍜♭(+1⇡⧻) Lines

# Find the unique numbers adjacent to each gear
⍜¤(⊠⬚0↻/⊂⊠⊂.1_0_¯1) Gears
▽:Numbers⧻.
.⊕(□▽≠0.⊝)∩♭:
# Filter to where the are precisely two
▽=2∵⧻
# Find the gear ratio
∵(/×⊜parse∩♭:Lines∊Numbers⊔)
# Sum
/+
&p
