1. Install the required tools. You will need:
   - stack (https://haskellstack.org)
   - ipe (http://ipe.otfried.org)
   - git (probably included in your OS)
   
2. Download packages from github. You will need:
   - impop (https://github.com/mloffler/impop)
   - hgeometry (https://github.com/mloffler/hgeometry)
   These should be placed next to each other in a common directory.
   (Ideally, you should not need to download the source of hgeometry and
   just be able to include it like any other package; however, the impop
   package depends on some features in my copy of hgeometry which are at
   this time not yet merged into the main package.)
   
3. Compile the packages. Go to the "impop" folder and run the command

    > stack build
    
   This should automatically build the hgeometry package as well.
   
4. Try if everything went well by running the executable.

    > stack run
    
   If all is well, this should prompt for a file name. Type "eye" for
   example. You should get a windows showing a curve arrangement; when
   you move your mouse over it some of its features should light up.
   Close the window to exit the application.
   
5. Try accessing the code in interactive mode by running:

    > stack ghci src/Test.hs
    
   If all is well, you should enter GHCI in the Test module. Try typing:
   
    *Test> testcurve1
    
   You should get some output showing the coordinates of the control
   points of some Bezier curve.
   
6. Try running different test files by editing "impop/app/Main.hs", for
   instance, change the line to
   
    main = TestLabelPipeline.main
    
   Now, running the application should not open a graphics window, but
   rather produce a bunch of files.
   
7. Try opening different files. The folder "impop/ipe" contains several
   ipe files which you can open instead of "eye". You can also create
   your own curve arrangements and save them here using ipe. Warning:
   the application does not yet support all types of curves you can
   draw with ipe; if you want to draw your own arrangement make sure to
   use only straight line segments and cubic Bezier pieces (that is, 
   splines with exactly four control points), otherwise you may get
   unexpected behaviour.
