# uiua version 0.5.1

# Open input file and read to string
&fras &var "INPUT"
# Extract non-digit non-dot non-newline symbols
⊏⍏.⊝▽¬∊,"0123456789.\n".
Symbols ←
# Parse
⊜∘↧⊙(≠@\n.)+1\+=@\n.
# Unique IDs for each digit
×↯:+1⇡/×.△. ↧⊃(≥@0)(≤@9).
# Unique IDs for each number
⍥(↥×⊙(>0.)⬚0↻0_1.)⊡1△.
# Find symbols and adjacencies
⊙(⍜¤(/↥⊠↻/⊂⊠⊂.1_0_¯1) ∵(∊:Symbols).)
# find IDs of numbers to keep
⊙(⊝▽≠0.▽∩♭:).
# Extract numbers from input string and sum
/+⊜parse∩♭×⊙∊.
&p
