data1 = read.csv("horse_info.csv",header=T)
data2 = read.csv("results.csv",header=T)
fulldata = merge(data2, data1, by="horseno")