#!/bin/bash

./setup-inputs.bash
mkdir -p logs

# Clear out Scieneer FASLs
rm -rf /home/jm/Lib/Lisp/fasl/misc/scl-*/home/jm/Lib/Lisp/nst/

echo ============================================================
echo Testing the trunk \(with everything recompiled\) on Scieneer
echo > logs/scieneer-fresh.log
/opt/scl/bin/scl -lkeys ~/Lib/Lisp/scieneer/lkey.text \
  < inputs/scieneer/nst-tests.lisp \
  2>> logs/scieneer-fresh.log >> logs/scieneer-fresh.log
grep TOTAL logs/scieneer-fresh.log | tail -1

echo ------------------------------------------------------------
echo Testing the trunk \(without recompiling\) on Scieneer
echo > logs/scieneer-reload.log
/opt/scl/bin/scl -lkeys ~/Lib/Lisp/scieneer/lkey.text \
  < inputs/scieneer/nst-tests.lisp \
  2>> logs/scieneer-reload.log >> logs/scieneer-reload.log
grep TOTAL logs/scieneer-reload.log | tail -1

echo ============================================================
