# multi class neural net sample(custom) for Digit Trainer example
input Picture [28, 28];
hidden H [200] from Picture all; 
hidden H2 [200] from H all;
output Result [10] sigmoid from H2 all;
