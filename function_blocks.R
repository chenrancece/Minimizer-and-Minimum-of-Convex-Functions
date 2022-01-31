data_gen<-function(n,sigma,f){
	#x<-seq(0,1,1/n)
#	x<-0:n
#	x<-x/n
	x<-seq(0,1,length.out=n+1)
	y_1<-f(x)
	noise<-rnorm(n+1,0,sigma)
	y_o<-y_1+noise
}


splitting<-function(data_o,sigma){
	n=length(data_o)-1
	noise1<-rnorm((2*(n+1)),0,sigma)
	data_l<-data_o + (1/sqrt(2))*noise1[1:(n+1)] +  sqrt(3/2)*noise1[(n+2):(2*n+2)]
	data_s<-data_o + (1/sqrt(2))*noise1[1:(n+1)] -  sqrt(3/2)*noise1[(n+2):(2*n+2)]
	data_e<-data_o - sqrt(2)*noise1[1:(n+1)]
	return(list(data_l,data_s,data_e,sqrt(3)*sigma,sqrt(3)*sigma,sqrt(3)*sigma))
}

splitting_no<-function(data_o,sigma){
	n=length(data_o)-1
	return(list(data_o,data_o,data_o,sigma,sigma,sigma))
}


locating_stopping<-function(data_l,data_s,sd_s){
	n<-length(data_l)-1
	print(n)
	J<-floor(log2(n+1))
	loc<-rep(-1,J+1)
	loc[1]<-1 # i for j=0 corresponding to loc[1]
	j<-1
	direction<-0
	not_stopped<-TRUE
	stop_thresh<-2
	print('j,J,not_stopped,starting')
	print(c(j,J,not_stopped))
	while((j<J+1)&not_stopped)
	{	print('In while')
		candidate_i_low<-max(loc[j]*2-2,1)
		candidate_i_high<-min(loc[j]*2+1,floor((n+1)*2^(j-J)))
		i_j<-candidate_i_low
		sum_min<-sum(data_l[(1+(i_j-1)*2^(J-j) ):(i_j*2^(J-j)) ])
		for(i in (candidate_i_low+1):candidate_i_high){
			sum_alt<-sum(data_l[(1+(i-1)*2^(J-j) ):(i*2^(J-j)) ])
			if(sum_alt<sum_min){ 
				i_j<-i
				sum_min<-sum_alt
				 }
		}
		loc[j+1]<-i_j
		T_r=Inf # will be normalized
		T_l=Inf #will be normalized
		if(i_j+5 < floor((n+1)*2^(j-J)) ){
			print('i_j+5 side,sd_s')
			print(sd_s)
			T_r<- (sum(data_s[(1+ (i_j+5)*2^(J-j)) : ((i_j+6)*2^(J-j))])-sum(data_s[(1+ (i_j+4)*2^(J-j) ):( (i_j+5)*2^(J-j))]))/(sd_s*sqrt(2^(J-j)))
			print(T_r)
		}
		if(i_j>6 ){
			print('i_j>6,sd_s')
			print(sd_s)
			T_l<- (sum(data_s[(1+ (i_j-7)*2^(J-j)) :( (i_j-6)*2^(J-j))])-sum(data_s[(1+ (i_j-6)*2^(J-j) ):( (i_j-5)*2^(J-j))])  )/(sd_s*sqrt(2^(J-j)))
			print(T_l)
		}	
		print('T_r,T_l,i_j,j')
		print(c(T_r,T_l,i_j,j))
		print(class(min(T_r,T_l)<stop_thresh))
		if((min(T_r,T_l)<stop_thresh))
			{
				not_stopped<-FALSE
				direction<-(T_r<stop_thresh)-(T_l<stop_thresh)
			}
			j<-j+1
	}	
	j<-j-1
	return(list(loc[j+1],j,not_stopped,direction,loc)) # j here is hat_j, loc[j+1] is i_{hat j}
}


locating_stopping_norealstop<-function(data_l,data_s,sd_s){
	n<-length(data_l)-1
	print(n)
	J<-floor(log2(n+1))
	loc<-rep(-1,J+1)
	loc[1]<-1 # i for j=0 corresponding to loc[1]
	j<-1
	direction<-0
	not_stopped<-TRUE
	stop_thresh<-2
	print('j,J,not_stopped,starting')
	print(c(j,J,not_stopped))
	while((j<J+1)&not_stopped)
	{	print('In while')
		candidate_i_low<-max(loc[j]*2-2,1)
		candidate_i_high<-min(loc[j]*2+1,floor((n+1)*2^(j-J)))
		i_j<-candidate_i_low
		sum_min<-sum(data_l[(1+(i_j-1)*2^(J-j) ):(i_j*2^(J-j)) ])
		for(i in (candidate_i_low+1):candidate_i_high){
			sum_alt<-sum(data_l[(1+(i-1)*2^(J-j) ):(i*2^(J-j)) ])
			if(sum_alt<sum_min){ 
				i_j<-i
				sum_min<-sum_alt
				 }
		}	
		loc[j+1]<-i_j
		T_r=Inf # will be normalized
		T_l=Inf #will be normalized
		if(i_j+5 < floor((n+1)*2^(j-J)) ){
			print('i_j+5 side,sd_s')
			print(sd_s)
			T_r<- (sum(data_s[(1+ (i_j+5)*2^(J-j)) : ((i_j+6)*2^(J-j))])-sum(data_s[(1+ (i_j+4)*2^(J-j) ):( (i_j+5)*2^(J-j))]))/(sd_s*sqrt(2^(J-j)))
			print(T_r)
		}
		if(i_j>6 ){
			print('i_j>6,sd_s')
			print(sd_s)
			T_l<- (sum(data_s[(1+ (i_j-7)*2^(J-j)) :( (i_j-6)*2^(J-j))])-sum(data_s[(1+ (i_j-6)*2^(J-j) ):( (i_j-5)*2^(J-j))])  )/(sd_s*sqrt(2^(J-j)))
			print(T_l)
		}	
		print('T_r,T_l,i_j,j')
		print(c(T_r,T_l,i_j,j))
		print(class(min(T_r,T_l)<stop_thresh))
		if((min(T_r,T_l)<stop_thresh))
			{
				not_stopped<-FALSE
				direction<-(T_r<stop_thresh)-(T_l<stop_thresh)
			}
			j<-j+1
	}	
	j<-j-1
	if(j<J)
	{	j1<-j+1
		while(j1<J+1)
		{
		candidate_i_low<-max(loc[j1]*2-2,1)
		candidate_i_high<-min(loc[j1]*2+1,floor((n+1)*2^(j1-J)))
		i_j<-candidate_i_low
		sum_min<-sum(data_l[(1+(i_j-1)*2^(J-j1) ):(i_j*2^(J-j1)) ])
		for(i in (candidate_i_low+1):candidate_i_high){
			sum_alt<-sum(data_l[(1+(i-1)*2^(J-j1) ):(i*2^(J-j1)) ])
			if(sum_alt<sum_min){ 
				i_j<-i
				sum_min<-sum_alt
				 }
		}	
		loc[j1+1]<-i_j
		T_r=Inf # will be normalized
		T_l=Inf #will be normalized
		if(i_j+5 < floor((n+1)*2^(j1-J)) ){
			print('i_j+5 side,sd_s')
			print(sd_s)
			T_r<- (sum(data_s[(1+ (i_j+5)*2^(J-j1)) : ((i_j+6)*2^(J-j1))])-sum(data_s[(1+ (i_j+4)*2^(J-j1) ):( (i_j+5)*2^(J-j1))]))/(sd_s*sqrt(2^(J-j1)))
			print(T_r)
		}
		if(i_j>6 ){
			print('i_j>6,sd_s')
			print(sd_s)
			T_l<- (sum(data_s[(1+ (i_j-7)*2^(J-j1)) :( (i_j-6)*2^(J-j1))])-sum(data_s[(1+ (i_j-6)*2^(J-j1) ):( (i_j-5)*2^(J-j1))])  )/(sd_s*sqrt(2^(J-j1)))
			print(T_l)
		}	
		print('T_r,T_l,i_j,j1')
		print(c(T_r,T_l,i_j,j1))
		print(class(min(T_r,T_l)<stop_thresh))
			j1<-j1+1	
		}
	}
	return(list(loc[j+1],j,not_stopped,direction,loc)) # j here is hat_j, loc[j+1] is i_{hat j}
}


z_est_nostop<-function(data_e,i_j,j,not_stopped,loc){
    n=length(data_e)-1
    J=floor(log2(n+1))
        if(not_stopped)
        {
            if(j!=floor(log2(n+1))){return('forbidden_input_z_est')}
            candidate<-(max(i_j-2,1)):(min(i_j+2,n+1))
            candidate_f<-data_e[candidate]
            inx<-which(candidate_f==min(candidate_f))
            inx_final<-inx+max(i_j-2,1)-1
            return((inx_final-1)/n)
        }   else {
#            return((i_j-1/2)*2^(J-j)/n -1/(2*n))
				return( (loc[J+1]-1)/n)
        }
}


z_est<-function(data_e,i_j,j,not_stopped){
	n=length(data_e)-1
	J=floor(log2(n+1))
		if(not_stopped)
		{ 
			if(j!=floor(log2(n+1))){return('forbidden_input_z_est')}
			candidate<-(max(i_j-2,1)):(min(i_j+2,n+1))
			candidate_f<-data_e[candidate]
			inx<-which(candidate_f==min(candidate_f))
			inx_final<-inx+max(i_j-2,1)-1
			return((inx_final-1)/n)
		}	else {
			return((i_j-1/2)*2^(J-j)/n -1/(2*n))
		}
}


z_ci<-function(data_e,i_j,j,not_stopped,sd_e,alpha,stop_thresh=2){
	K_halfalpha=ceiling(log(alpha/2)/log(pnorm(-stop_thresh)))
	n=length(data_e)-1
	J<-floor(log2(n+1))
	alpha1<-alpha/8
	alpha2<-alpha/24
	if(not_stopped){
		if(j!=floor(log2(n+1))){return('forbidden_input_z_ci')}
		L<-max(1,i_j-12*2^K_halfalpha)
		U<-min(n+1,i_j+12*2^K_halfalpha)
		noise<-rnorm(n+1,0,sd_e)
		data_s1<-data_e+noise
			data_s1_l<-data_s1[1:n]-data_s1[2:(n+1)]
			data_s1_r<-data_s1[2:(n+1)]-data_s1[1:n]
		data_s2<-data_e-noise
		Ind_l<-rep(0,n+1)
		Ind_r<-rep(0,n+1)
		Ind_l[1:n]<-(data_s1_l<2*sd_e*qnorm(1-alpha1))
		Ind_l[U]<-1
		i_l<-min(which(Ind_l[L:U]==1))+L-1
		Ind_r[1:n]<-(data_s1_r<2*sd_e*qnorm(1-alpha1))
		i_r<-max( which(Ind_l[L:U]==1) , 0) +L-1
			if(i_l==U){
					slope<-data_s2[n-1]-data_s2[n]+2*sqrt(2)*sd_e*qnorm(1-alpha2)
				if((i_l==n+1)&slope>0)
					{
						height<-data_s2[n+1]-data_s2[n] + 2*sqrt(2)*sd_e*qnorm(1-alpha2)
						t_lo<- -height/(n*slope)+ (n-1)/n
						t_lo<-min(max(t_lo,(n-1)/n),1)
						t_hi<-1
						return(c(t_lo,t_hi))
					} else{
					return(U-1/n,U-1/n)
					}
			} 
			if(i_r==L-1){
					slope <- data_s2[3]-data_s2[2]+ 2*sqrt(2)*sd_e
				if((i_l==0)&slope>0){
						height<-data_s2[1]-data_s2[2]+2*sqrt(2)*sd_e
						t_hi<-height/(n*slope)+1/n
						t_hi<-min(max(0,t_hi),1/n)
						t_lo<-0
						return(c(t_lo,t_hi))
				} else{	
					return(c(0,0))
				}
			}
			if((i_l-U)*(i_r-L+1)!=0){
					i_lo<-max(i_l-1,L)
					i_hi<-min(i_r+2,U)
					if((i_hi-i_lo> 2)|((i_hi-n-1)*(i_lo-1)==0))
						{ return(c((i_lo-1)/n,(i_hi-1)/n))
						} 
					slope_r<-data_s2[i_hi+1]-data_s2[i_hi]+2*sqrt(2)*sd_e*qnorm(1-alpha2)
					slope_l<-data_s2[i_lo-1]-data_s2[i_lo]+2*sqrt(2)*sd_e*qnorm(1-alpha2)
					if(slope_l<0|slope_r<0)
					{
						return( c((i_hi+i_lo-2)/(2*n),(i_hi+i_lo-2)/(2*n)))
					 }else{
						height_r<-data_s2[i_hi-1]-data_s2[i_hi]+2*sqrt(2)*sd_e*qnorm(1-alpha2)
						height_l<-data_s2[i_lo+1]-data_s2[i_lo]+2*sqrt(2)*sd_e*qnorm(1-alpha2)
						t_hi<-min(max((height_r/(n*slope_r)+i_hi/n),(i_hi-1)/n),i_hi/n) 
						t_lo<-min(max( -height_l/(n*slope)  + i_lo/n ,i_lo/n),(i_lo+1)/n)
						return(c(t_lo,t_hi))
					}
			} else {return('bug in program')}	
	} else {
		t_lo<-max(0,i_j-12*2^K_halfalpha+1)*2^(J-j)/n-1/(2*n)
		t_hi<- min(floor((n+1)*2^(j-J)),i_j+12*2^K_halfalpha-2)*2^(J-j)/n-1/(2*n)
	}
	return(c(t_lo,t_hi))
}

m_est<-function(data_e,i_j,j,not_stopped,sd_e,direction){
	n<-length(data_e)-1
	J=floor(log2(n+1))
	if(not_stopped){
		candidate_low<-max(i_j-2,1)
		candidate_high<-min(i_j+2,n+1)
		print('candidate_low,candidate_high,i_j')
		print(c(candidate_low,candidate_high,i_j))
		Mhat<-min(data_e[candidate_low:candidate_high])
	}	else {
		ti_j<-i_j + 2*direction
		Mhat<-2^(j-J)*sum(data_e[((i_j-1)*2^(J-j)+1):(i_j*2^(J-j))])
		print('direction,Mhat')
		print(c(direction,Mhat))
	}
	return(Mhat)
}

S_n_beta<-function(n,beta){
		normalvalue<-(1-beta)^(1/n)
		return(qnorm(normalvalue))
	}

m_ci<-function(data_e,i_j,j,sd_e,alpha,loc,sigma,stop_thresh=2){
	tK_alpha= max(4, 2+ ceiling(log2(2+qnorm(1-alpha/3)))) #need to be changed for different thresh
	tK_quatalpha<- max(4, 2+ ceiling(log2(2+qnorm(1-alpha/12))))
	n<-length(data_e)-1
	J=floor(log2(n+1))
	K_quatalpha<-ceiling(log(alpha/4)/log(pnorm(-stop_thresh)))
	js<-max(0,j-K_quatalpha)
	jl<-min(J,j+tK_quatalpha)
	i_js<-loc[js+1]
	I_lo<-max(1,2^(jl-js)*(i_js-5))
	I_hi<-min(2^(jl-js)*(i_js+4)+1,floor((n+1)/2^(J-jl)))
	print('js,jl,i_js,I_lo,I_hi,J,j,tK_quatalpha,K_quatalpha')
	print(c(js,jl,i_js,I_lo,I_hi,J,j,tK_quatalpha,K_quatalpha))
	candidate_raw<-data_e[((I_lo-1)*2^(J-jl)+1):(I_hi*2^(J-jl))]
	candidate_mat<-matrix(candidate_raw,ncol=I_hi-I_lo+1)
	hatf1<-min(apply(candidate_mat,2,mean))
	print(hatf1)
	Slong<-S_n_beta(I_hi-I_lo+1,alpha/4)
	f_hi<-hatf1+Slong*sd_e/sqrt(2^(J-jl))
	if(j+tK_quatalpha<J+1){
		f_lo<-hatf1 - qnorm(1-alpha/4)*sd_e/sqrt(2^(J-jl)) - sqrt(3)*sigma/sqrt(2^(J-jl))
		return(c(f_lo,f_hi))
		} else {
		H<-sd_e*S_n_beta(I_hi-I_lo+3,alpha/8)	
		h<-rep(Inf,n)
		k_l<-I_lo  #the min in first interval is indexed by h(0) in paper, but h[1] here
		k_r<-I_hi-1
		if(I_lo==1){
					h[1]<-min(data_e[2]-H,data_e[2]*2-data_e[3]-3*H)
					k_l<-2
					}
		if(I_hi==n+1){
					h[n]<-min( data_e[n]-H , 2*data_e[n]-data_e[n-1]-3*H )
					k_r<-n-1
					}
		for(i in k_l:k_r){
					l_l<-data_e[i]-H #left line, left point
					l_r<-data_e[i]*2-data_e[i-1]-3*H #left line, right point
					r_l<-data_e[i+1]*2-data_e[i+2]-3*H
					r_r<-data_e[i+1]-H
					if((l_l-r_l)*(l_r-r_r)<0){
						h[i]<- l_l + (l_r-l_l)*(r_l-l_l)/(r_l-l_l-r_r+l_r)
					} else {
						h[i]<-min(max(l_l,l_r),max(r_l,r_r))
					}
					}
		f_lo<-min(h)
		return(c(f_lo,f_hi))
		}
}


