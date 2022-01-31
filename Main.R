# This file contains four functions : Convex_Estimation_Inference, Convex_Estimation_Inference_nosplit,Convex_Estimation_Inference_nosplit_znostop,Convex_Estimation_Inference_znostop


source('./function_blocks.R')

Convex_Estimation_Inference<-function(data,sigma,alpha=0.05){	
	splitted_data<-splitting(data,sigma)
	loc_stop<-locating_stopping(splitted_data[[1]],splitted_data[[2]],splitted_data[[5]])
	print('z_hat')
	print((loc_stop[[5]]))
	z_hat<-z_est(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]])
	print('z_ci')
	z_CI<-z_ci(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],splitted_data[[6]],alpha)
	print('m_hat')
	m_hat<-m_est(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],splitted_data[[6]],loc_stop[[4]])
	print('m_ci')
	m_CI<-m_ci(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],splitted_data[[6]],alpha,loc_stop[[5]],sigma)
	print('z_hat,z_CI,m_hat,m_CI')
	print(c(z_hat,z_CI,m_hat,m_CI))
	return(c(z_hat,z_CI,m_hat,m_CI))
}

Convex_Estimation_Inference_nosplit<-function(data,sigma,alpha=0.05){	
	splitted_data<-splitting_no(data,sigma)
	loc_stop<-locating_stopping(splitted_data[[1]],splitted_data[[2]],splitted_data[[5]])
	print('z_hat')
	print((loc_stop[[5]]))
	z_hat<-z_est(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]])
	print('z_ci')
	z_CI<-z_ci(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],splitted_data[[6]],alpha)
	print('m_hat')
	m_hat<-m_est(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],splitted_data[[6]],loc_stop[[4]])
	print('m_ci')
	m_CI<-m_ci(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],splitted_data[[6]],alpha,loc_stop[[5]],sigma)
	print('z_hat,z_CI,m_hat,m_CI')
	print(c(z_hat,z_CI,m_hat,m_CI))
	return(c(z_hat,z_CI,m_hat,m_CI))
}

Convex_Estimation_Inference_nosplit_znostop<-function(data,sigma,alpha=0.05){	
	splitted_data<-splitting_no(data,sigma)
	loc_stop<-locating_stopping_norealstop(splitted_data[[1]],splitted_data[[2]],splitted_data[[5]])
	print('z_hat')
	print((loc_stop[[5]]))
	z_hat<-z_est_nostop(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],loc_stop[[5]])
	print('z_ci')
	z_CI<-z_ci(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],splitted_data[[6]],alpha)
	print('m_hat')
	m_hat<-m_est(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],splitted_data[[6]],loc_stop[[4]])
	print('m_ci')
	m_CI<-m_ci(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],splitted_data[[6]],alpha,loc_stop[[5]],sigma)
	print('z_hat,z_CI,m_hat,m_CI')
	print(c(z_hat,z_CI,m_hat,m_CI))
	return(c(z_hat,z_CI,m_hat,m_CI))
}


Convex_Estimation_Inference_znostop<-function(data,sigma,alpha=0.05){
    splitted_data<-splitting(data,sigma)
    loc_stop<-locating_stopping_norealstop(splitted_data[[1]],splitted_data[[2]],splitted_data[[5]])
    print('z_hat')
    print((loc_stop[[5]]))
    z_hat<-z_est_nostop(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],loc_stop[[5]])
    print('z_ci')
    z_CI<-z_ci(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],splitted_data[[6]],alpha)
    print('m_hat')
    m_hat<-m_est(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],loc_stop[[3]],splitted_data[[6]],loc_stop[[4]])
    print('m_ci')
    m_CI<-m_ci(splitted_data[[3]],loc_stop[[1]],loc_stop[[2]],splitted_data[[6]],alpha,loc_stop[[5]],sigma)
    print('z_hat,z_CI,m_hat,m_CI')
    print(c(z_hat,z_CI,m_hat,m_CI))
    return(c(z_hat,z_CI,m_hat,m_CI))
}


