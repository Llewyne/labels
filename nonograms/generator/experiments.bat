rem =============================START PARAMETERS=====================================================
rem @echo off
setlocal enabledelayedexpansion
rem set the number of experiments to run per setting
set /a numexps=10
rem path to inputs
set inpath="\"C:\Users\3822443\Documents\CNIns\EXINPUT\prepro\\""
rem path to put outputs
set outpath="\"C:\Users\3822443\Documents\CNouts\EXOUTS\Preprocessed\phase2\\""
rem score weights (6 doubles), for vertex, angle, size, shape, length, and obfuscation penalties respectively
set wgt="0.75,1,0.05,0,75,0,1"
rem thresholds for determining g1-continuity and splitting picture curves.
set splitstring="10,20,20,10"
rem seed (int) for randomization. 0 = no seed
set sd=0
rem ratio (double) of curves to connect during initialization. 
set cra=0
rem set the connection stategy in case of a nonzero cratio. r=random, g=greedy s=semi-greedy
set cst=s
rem set the starting temperature (double) for SA
set T=153
rem set the cooling rate (double between 0 and 1) for SA
set crate=0.99
rem set the number of iterations (int) to run pairwise optimization
set pwi=0
rem set the number of attempts to randomize a curve (int, 0=no limit) before restarting the program
set resi=50
rem set the number of attempts (int, 0= no limit) to randomize a curve before lowering the temperature and picking a different curve to optimize
set randa=10
rem set the number of attempts (int, 0= no limit) to randomize a curve before making an exception saying polybezier-self intersections are okay. 
set psa=50 
rem set the number of iterations between snapshots
set shots=100
rem set flags to include here. Available flags are -me (Make new ECEs) -dre (Dont resolve ECEs) -npsi (No Polybezier self-intersections allowed) -roc (Restart program on crash) -rc (Allow reconnetions during optimizations) -fc (Continue SA until full convergence) -sic (Split input curves if an ECE can be made out of it) -sharp (Sharpen ECEs to increase the angle) -col (Use color information and automatically test for solvability)
set "flags= -rc -fc -sharp -sic -col"


rem =============================END PARAMETERS, START INPUT ==========================================

cd "C:\Users\3822443\OneDrive - Universiteit Utrecht\Curved Nonograms\Repo\CSCode\CurvedNonogramsGenerator\CurvedNonogramsGenerator\bin\Release\"

set File[0]="elephant"
set File[1]="bait"
set File[2]="beaverf"
set File[3]="butterfly"
set File[4]="knight"
set File[5]="swallow"
set File[6]="fish"
set File[7]="airplane"
set File[8]="batf"
set File[9]="coffee_cupfix"
set File[10]="fruitf"
set File[11]="binoculars"
set File[12]="kiwif"
set File[13]="swanf"
set File[14]="yoga"


set Split[0]="12,20,20,12"
set Split[1]="10,20,20,8"
set Split[2]="10,20,20,8"
set Split[3]="10,20,20,10"
set Split[4]="14,20,20,8"
set Split[5]="12,20,20,12"
set Split[6]="10,20,20,7"
set Split[7]="12,20,20,12"
set Split[8]="12,20,20,12"
set Split[9]="12,20,20,12"
set Split[10]="12,20,20,12"
set Split[11]="12,20,20,12"
set Split[12]="12,20,20,12"
set Split[13]="12,20,20,12"
set Split[14]="12,20,18,16.5"

rem =================================END INPUT ==========================================================
for /l %%y in (0,1,5) do (
for /l %%x in (1,1,%numexps%) do (
CurvedNonogramsGenerator.exe indir=%inpath%" file=prepro!File[%%y]!arr1.xml -am outdir=%outpath%" weights=%wgt% splstr=!Split[%%y]! outfile=!File[%%y]!out%%x.svg seed=%sd% cratio=%cra% cstrat=%cst% temp=%T% coolrate=%crate% ras=%resi% savefile=!File[%%y]!arr%%x.xml rattempts=%randa% sirattempts=%psa% snap=%shots% pairits=%pwi%%flags%
)
)