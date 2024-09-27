rem lf95 compile
lf95 -chk -trace -trap diou -c -dbl aercoare.f
lf95 -chk -trace -trap diou -c -dbl timesubs.f
lf95 aercoare.obj timesubs.obj -exe aercoare.exe
