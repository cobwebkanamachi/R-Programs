#def in net sharp runnable in Azure Machine Learning Studio.
# http://blogs.technet.com/b/machinelearning/archive/2015/03/10/convolutional-neural-nets-in-net.aspx
const { T = true; F = false; }
input Picture [28, 28];
hidden C1 [5, 12, 12]
  from Picture convolve {
    InputShape  = [28, 28]; // required
    KernelShape = [ 5,  5]; // required
    Stride      = [ 2,  2]; // optional, default is 1 in all dimensions
    MapCount    = 5;        // optional, default is 1
  };
  
hidden C2 [50, 4, 4]
  from C1 convolve {
    InputShape  = [ 5, 12, 12];
    KernelShape = [ 1,  5,  5];
    Stride      = [ 1,  2,  2];
    Sharing     = [ F,  T,  T]; // optional, default is true in all dimensions
    MapCount    = 10;
  }

output Result [10] sigmoid from C2 all;
