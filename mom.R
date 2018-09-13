x = read.table("DataEx3G8.txt")
acf = acf(x)
acvf = acf(x,type="covariance")
phi_1 <- (acvf[1][[1]]*acvf[2][[1]]-acvf[0][[1]]*acvf[3][[1]])/(acvf[1][[1]]*(acvf[1][[1]]-acvf[2][[1]]*(acvf[0][[1]]/acvf[1][[1]])))
phi_2 <- (acvf[3][[1]] - phi_1*acvf[2][[1]])/acvf[1][[1]]

arima=arima(x,order=c(2,0,1))
