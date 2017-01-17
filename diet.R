
##################################################
# Create Likert Variables for Diet


Fish <- rbinom(N,6,0.6)+1;
Carrots <- rbinom(N,6,0.5)+1;
Peas <- rbinom(N,6,0.6)+1;
Turnip <- rbinom(N,6,0.2)+1;
Lamb <- rbinom(N,6,0.6)+1;
Liver <- rbinom(N,6,0.6)+1;
Yoghurt <- rbinom(N,6,0.6)+1;
Grapes <- rbinom(N,6,0.6)+1;

#################################################

# Seperate our population into several variables types.
# This is a "Secret" Variable, to create plausible data.
