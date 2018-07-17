# act-r-lie-detection-model
An ACT-R model of experimental data presented by Street, Bischof, Vadillo and Kingstone (2016)

Instructions for use.
1. Extract the actr7.zip file containing the ACT-R code.
2. In the resulting actr7 folder, create a folder called 'models'.
3. In the 'models' folder place the three lisp files: lie-detection.lisp, lhststs.lisp, and package.lisp.
4. In your lisp, load the 'load-act-r.lisp' file.
5. Load the 'lie-detection.lisp' file.
6. Run the model using the 'runsim' funtion.  For example (runsim 100 “easy”) runs 100 simulated participants on the 'easy' condition.
For each simulated participant the model outputs the following data comparing the human data for each diagnosticity condition with the response proportions from the model after the training phase (Model-trn) and the testing phase (Model-tst).

S: 99 (easy) ((silence 30) (voice 40) (self 50) (face 80))  
13 13 22 31  
  8  12  16  20  24  28  32  
0.2 0.3 0.4 0.5 0.6 0.7 0.8  
Human-esy,0.219,0.341,0.524,0.670,0.634,0.858,0.754  
Model-trn,0.062,0.148,0.277,0.533,0.734,0.850,0.935  
Model-tst,0.254,0.367,0.492,0.645,0.787,0.870,0.912  
CORRELATION:  0.954  
MEAN DEVIATION:  0.086

The model outputs the final proportions to file (data.csv) which can then be plotted using the R script data-plots.R to produce the graph seen in the data.pdf file.
