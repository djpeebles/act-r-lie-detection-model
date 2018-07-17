# act-r-lie-detection-model
An ACT-R model of experimental data presented by Street, Bischof, Vadillo and Kingstone (2016)

Instructions for use.
1. Extract ACT-R zip files containing ACT-R code.
2. In the resulting folder, create a folder called 'models'.
3. In the 'models' folder place the three lisp files: lie-detection.lisp, lhststs.lisp, and package.lisp
4. In your lisp, load 'load-act-r.lisp'
5. Load 'lie-detection.lisp'
6. Run the model using the 'runsim' funtion.  For example (runsim 100 “easy”) runs 100 simulated participants on the 'easy' condition.
