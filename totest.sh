#!/bin/bash

read -d '' awkScript <<'EOF'
($5 == "Effect") {
  print  \
    $2 ", " \
    ($6 == "ID" ? "Scfg.effect_ID" : ("Scfg.E \\"" $6 "\\"")) ", " \
    $4 ", " \
    "Complexity." \
    ($8 == "N" ? "Linear \\"N\\"" : ($8 == "∞" ? "Unbounded" : "Const " $8)) " ;"
}
EOF

grep -- '->' | \
  gsed 's!(\|)\|:! !g' | \
  awk "$awkScript"
