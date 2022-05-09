smartgrid = read.csv('C:/Users/lhk65/OneDrive/Desktop/tdata/smart_grid_stability_augmented.csv')

smart_T = smartgrid[smartgrid$stabf == 'stable',]
smart_F = smartgrid[smartgrid$stabf == 'unstable',]

sample_tr =  sample(1:nrow(smart_T),2000)
sample_t =  sample(1:nrow(smart_T),1000)
sample_f =  sample(1:nrow(smart_F),1000)

smart_tr = smart_T[sample_tr,c(1:13)]
smart_t = smart_T[sample_f,c(1:13)]
smart_f = smart_F[sample_t,c(1:13)]

tsquare(smart_tr,smart_t,smart_f,100)
