library(ggplot2)

max_iter=25
cl=colours()
step=seq(-2,0.8,by=0.005)
points=array(0,dim=c(length(step)^2,3))
t=0

for(a in step)
{
  for(b in step+0.6)
  {
    x=0;y=0;n=0;dist=0
    while(n<max_iter & dist<4)
    {
      n=n+1
      newx=a+x^2-y^2
      newy=b+2*x*y
      dist=newx^2+newy^2
      x=newx;y=newy
    }

    if(dist<4)
    { 
      color=24 # black
    }
    else
    {
      color=n*floor(length(cl)/max_iter)
    }

    t=t+1
    points[t,]=c(a,b,color)
  }
}

df=as.data.frame(points)	

# Can change the colors by fiddling with the following.
# last_plot() + scale_colour_manual(values=sort(c("#00000000", rainbow(23)), decreasing=FALSE))
#http://stackoverflow.com/questions/23865703/how-to-update-this-outdated-example-so-that-ggplot2-does-not-give-error-use-th
base_size = 12
base_family = ""
ggplot(data=df, aes(V1, V2, color=cl[V3]))+ 
geom_point() + 
theme(panel.background=element_rect(fill = "white", colour = NA), 
      panel.grid.major=element_line(colour = "grey90", size = 0.2), 
      panel.grid.minor=element_line(colour = "grey98", size = 0.5), 
      axis.ticks=element_line(colour = "black"), 
      axis.text.x=element_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1), 
      axis.text.y=element_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1), 
      axis.title.x=element_text(size = base_size, vjust = 0.5), 
      axis.title.y=element_text(size = base_size, angle = 90, vjust = 0.5))  

ggsave('mandelbrot_ggplot2.png')

print('Image Saved.')
dev.off()

#please see bellow if you run azure machine learning on this(this is sample configuration)
#http://www.princeton.edu/~whao/theme_complete_bw.R
