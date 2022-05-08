Occupancy = read.csv('C:/Users/lhk65/OneDrive/Desktop/tdata/Occupancy.csv')

Occupancy_T = Occupancy[Occupancy$Occupancy == '1',]
Occupancy_F = Occupancy[Occupancy$Occupancy == '0',]

sample_tr =  sample(1:nrow(Occupancy_T),2000)
sample_t =  sample(1:nrow(Occupancy_T),1000)
sample_f =  sample(1:nrow(Occupancy_F),1000)

occupancy_tr = Occupancy_T[sample_tr,c(2:6)]
occupancy_t = Occupancy_T[sample_t,c(2:6)]
occupancy_f = Occupancy_F[sample_f,c(2:6)]

Ttest(occupancy_tr,occupancy_f)

tsquare(occupancy_tr,occupancy_t,occupancy_f,100)
