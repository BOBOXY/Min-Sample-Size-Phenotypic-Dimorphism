#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rcpp_distC2a(NumericMatrix m1,int n,int e11,int e12){
    NumericMatrix m2(n,n);
	std::fill(m2.begin(),m2.end(),NumericVector::get_na());
	
	for(int i=0;i<n;i++){
		for(int j=e12-1;j<n;j++)
		{			
			if(i+j+1<=e11)
				m2(i,j)=m1(i,j);
			else 
				break;
		}	
	}
	
	return m2;	
}

// [[Rcpp::export]]
NumericMatrix rcpp_distC2b(NumericMatrix m1,NumericMatrix m2,int n,int e11,int e12){   
	NumericMatrix m3=clone(m2);
	
	for(int i=0;i<n;i++){
		for(int j=0;j<n;j++)
		{	
			if((i+j+1>=e12)&&j<e11)m3(i,j)=NA_REAL;
		}	
	}
	
	
	return m3;	
}

// [[Rcpp::export]]
NumericMatrix rcpp_distC2b_N(NumericMatrix m1,int n,NumericVector elv,int mod){   
	NumericMatrix m2=clone(m1);
	
	for(int lm=0;lm<mod*2;lm+=2){
		int e11=elv[lm], e12=elv[lm+1];
		for(int i=0;i<n;i++){
			for(int j=0;j<n;j++)
			{	
				if((i+j+1>=e12)&&j<e11)m2(i,j)=NA_REAL;
			}	
		}
	}
	
	return m2;	
}


// [[Rcpp::export]]
void rcpp_Cord(NumericMatrix& m_cord,const NumericMatrix& m_aord,int n,int iniint12_1,int finint12_1){
	
	//Cord[col(Aord)>=iniint[[l2]][l]&col(Aord)<finint[[l2]][l]&(row(Aord)+col(Aord))<=(finint[[l2]][l])]=
    //            Aord[col(Aord)>=iniint[[l2]][l]&col(Aord)<finint[[l2]][l]&(row(Aord)+col(Aord))<=(finint[[l2]][l])]}
		
	for(int i=0;i<n;i++){
		for(int j=0;j<n;j++){
			if((j+1>=iniint12_1) && (j+1<finint12_1) && (i+j+2<=finint12_1))
				m_cord(i,j)=m_aord(i,j);
		}
	}
	
	return;	
}



// [[Rcpp::export]]
NumericMatrix rcpp_matrix_delrow(NumericMatrix m, int del_row){		
    int nrow=m.nrow(),ncol=m.ncol();
	NumericVector v=as<NumericVector>(transpose(m));
	v.erase((del_row-1)*ncol,del_row*ncol);
	v.attr("dim")=Dimension(nrow,ncol-1);
	
	return transpose(as<NumericMatrix>(v));
}

// [[Rcpp::export]]
NumericMatrix rcpp_matrix_plus(NumericMatrix m1,NumericMatrix m2){
      NumericMatrix m3=clone(m1);	
	  for(NumericVector::iterator i = m3.begin(),j=m2.begin(); i != m3.end()&&j != m2.end(); ++i,++j) {
         *i+=*j;
	}
     return m3;
}

// [[Rcpp::export]]
double rcpp_diffem(NumericMatrix PC,NumericMatrix distC,int ndata,double lambda,int mod0){
    
	//PC, distC, ndata, 
	//List elememl1;	
	
	double diffem=0;
	 
    //Excess mass for one mode

    //NumericMatrix eml1=PC-(distC*lambda);	
	NumericMatrix eml1=rcpp_matrix_plus(PC,-1*lambda*distC);
	
	
    //eml1max=max(eml1,na.rm=T);
    //elememl1[[1]]=which(eml1==eml1max,arr.ind=T)[1,];
	Function r_max("max");  
    Function r_which("which");   	
	
	RObject o_eml1max= r_max(eml1,_["na.rm"]=true);
	double eml1max=as<double>(o_eml1max);
	List ls=r_which(eml1==eml1max,_["arr.ind"]=true);
	int max_idx=as<int>(ls[0]);
	int t_row=(max_idx-1)%ndata+1;
	int t_col=(max_idx-1)/ndata+1;
	
    //elememl1[[1]]=c(sum(elememl1[[1]])-1,elememl1[[1]][2]);
	IntegerVector elememl1((mod0+1)*2);
	elememl1[0]=t_row+t_col-1;
	elememl1[1]=t_col;
	
	
    ////eml1maxtemp=eml1max
    //elememl1temp=elememl1;
    IntegerVector elememl1temp=elememl1;

    //for(modtemp in 1:mod0){
	  for(int modtemp=1;modtemp<=mod0;modtemp++){

      //eml1maxtemp=0;
	  double eml1maxtemp=0;

      //In one interval, remove one subinterval    
  
      //for(lm in 1:modtemp){
		for(int lm=1;lm<=modtemp;lm++){

        //if(diff(elememl1[[lm]])<0){
		int el1=elememl1[(lm-1)*2],el2=elememl1[(lm-1)*2+1];
		if(el2<el1){

          //distC2a=matrix(NA,nrow=ndata,ncol=ndata);
          //distC2a[((row(distC)+col(distC)-1)<=elememl1[[lm]][1])&(col(distC)>=elememl1[[lm]][2])]=distC[((row(distC)+col(distC)-1)<=elememl1[[lm]][1])&(col(distC)>=elememl1[[lm]][2])];
			NumericMatrix distC2a(ndata,ndata);
			std::fill(distC2a.begin(),distC2a.end(),NumericVector::get_na());
	
			for(int i=0;i<ndata;i++){
				for(int j=el2-1;j<ndata;j++)
				{			
					if(i+j+1<=el1)
						distC2a(i,j)=distC(i,j);
					else 
						break;
				}	
			}
		  
		  //eml2a=-PC[-ndata,]+lambda*distC2a[-1,];		 
		  //NumericMatrix eml2a=-1*rcpp_matrix_delrow(PC,ndata)+lambda*rcpp_matrix_delrow(distC2a,1);
		  NumericMatrix eml2a=rcpp_matrix_plus(-1*rcpp_matrix_delrow(PC,ndata),lambda*rcpp_matrix_delrow(distC2a,1));
		  
          //eml2amax=max(eml2a,na.rm=T);
          //elememl2a=which(eml2a==eml2amax,arr.ind=T)[1,];
          //elememl2a=c(sum(elememl2a)-1,elememl2a[2]);
		  
		  RObject o_eml2amax= r_max(eml2a,_["na.rm"]=true);
		  double eml2amax=as<double>(o_eml2amax);
		  List ls2a=r_which(eml2a==eml2amax,_["arr.ind"]=true);
		  int max_idx2a=as<int>(ls2a[0]);
		 
		  int t_row2a=(max_idx2a-1)%(ndata-1)+1;//
		  int t_col2a=(max_idx2a-1)/(ndata-1)+1;//		 

          double eml1maxtempa=-eml2amax+eml1max;

          if(eml1maxtempa>eml1maxtemp){
            eml1maxtemp=eml1maxtempa;
			
            //elememl1temp[[lm]]=c(elememl2a[2],elememl1[[lm]][2]);
			elememl1temp[(lm-1)*2]=t_col2a;
			elememl1temp[(lm-1)*2+1]=elememl1[(lm-1)*2+1];
			
            //elememl1temp[[modtemp+1]]=c(elememl1[[lm]][1],elememl2a[1]);
			elememl1temp[modtemp*2]=elememl1[(lm-1)*2];
			elememl1temp[modtemp*2+1]=t_row2a+t_col2a-1;
			
          }

        }
		}

     
       
      //add one new interval

      //distC2b=distC;
		
      //for(lm in 1:modtemp){
      //  distC2b[((row(distC)+col(distC)-1)>=elememl1[[lm]][2])&(col(distC)<=elememl1[[lm]][1])]=NA;
      //}
		NumericMatrix distC2b=clone(distC);	
		
		for(int lm=1;lm<=modtemp;lm++){
			int el1=elememl1[(lm-1)*2],el2=elememl1[(lm-1)*2+1];
			
			for(int i=0;i<ndata;i++){
				for(int j=0;j<ndata;j++)
				{	
					if((i+j+1>=el2)&&j<el1)distC2b(i,j)=NA_REAL;
				}	
			}
		}	


      //if(sum(!is.na(distC2b))>0){
		if(na_omit(distC2b).length()>0){

        //eml2b=PC-lambda*distC2b;
		//NumericMatrix eml2b=PC-lambda*distC2b;
		NumericMatrix eml2b=rcpp_matrix_plus(PC,-1*lambda*distC2b);
		
        //eml2bmax=max(eml2b,na.rm=T);
        //elememl2b=which(eml2b==eml2bmax,arr.ind=T)[1,];
        //elememl2b=c(sum(elememl2b)-1,elememl2b[2]);
		
		RObject o_eml2bmax= r_max(eml2b,_["na.rm"]=true);
		double eml2bmax=as<double>(o_eml2bmax);
		List ls2b=r_which(eml2b==eml2bmax,_["arr.ind"]=true);
		int max_idx2b=as<int>(ls2b[0]);
		 
		int t_row2b=(max_idx2b-1)%ndata+1;//
		int t_col2b=(max_idx2b-1)/ndata+1;//	

        //eml1maxtempb=eml2bmax+eml1max;
		double eml1maxtempb=eml2bmax+eml1max;

        // if(eml1maxtempb>eml1maxtemp){
          // eml1maxtemp=eml1maxtempb;
          // elememl1temp=elememl1;
          // elememl1temp[[modtemp+1]]=elememl2b;
        // }
		
		if(eml1maxtempb>eml1maxtemp){
            eml1maxtemp=eml1maxtempb;
			elememl1temp=elememl1;           
			
            //elememl1temp[[modtemp+1]]=elememl2b;
			elememl1temp[modtemp*2]=t_row2b+t_col2b-1;
			elememl1temp[modtemp*2+1]=t_col2b;
			
          }

      }

      elememl1=elememl1temp;
      diffem=eml1maxtemp-eml1max;
      eml1max=eml1maxtemp;

    }	

    return diffem;


  }
  
  
  // [[Rcpp::export]]
NumericVector rcpp_diffem_N(NumericMatrix PC,NumericMatrix distC,int ndata,NumericVector lambdas,int mod0){
    
	//PC, distC, ndata, 
	//List elememl1;

    NumericVector 	diffem_v(lambdas.length());
	
	for(int lit=0;lit<lambdas.length();lit++){
	
	double lambda=lambdas[lit];
	double diffem=0;
	 
    //Excess mass for one mode

    //NumericMatrix eml1=PC-(distC*lambda);	
	NumericMatrix eml1=rcpp_matrix_plus(PC,-1*lambda*distC);
	
	
    //eml1max=max(eml1,na.rm=T);
    //elememl1[[1]]=which(eml1==eml1max,arr.ind=T)[1,];
	Function r_max("max");  
    Function r_which("which");   	
	
	RObject o_eml1max= r_max(eml1,_["na.rm"]=true);
	double eml1max=as<double>(o_eml1max);
	List ls=r_which(eml1==eml1max,_["arr.ind"]=true);
	int max_idx=as<int>(ls[0]);
	int t_row=(max_idx-1)%ndata+1;
	int t_col=(max_idx-1)/ndata+1;
	
    //elememl1[[1]]=c(sum(elememl1[[1]])-1,elememl1[[1]][2]);
	IntegerVector elememl1((mod0+1)*2);
	elememl1[0]=t_row+t_col-1;
	elememl1[1]=t_col;
	
	
    ////eml1maxtemp=eml1max
    //elememl1temp=elememl1;
    IntegerVector elememl1temp=elememl1;

    //for(modtemp in 1:mod0){
	  for(int modtemp=1;modtemp<=mod0;modtemp++){

      //eml1maxtemp=0;
	  double eml1maxtemp=0;

      //In one interval, remove one subinterval    
  
      //for(lm in 1:modtemp){
		for(int lm=1;lm<=modtemp;lm++){

        //if(diff(elememl1[[lm]])<0){
		int el1=elememl1[(lm-1)*2],el2=elememl1[(lm-1)*2+1];
		if(el2<el1){

          //distC2a=matrix(NA,nrow=ndata,ncol=ndata);
          //distC2a[((row(distC)+col(distC)-1)<=elememl1[[lm]][1])&(col(distC)>=elememl1[[lm]][2])]=distC[((row(distC)+col(distC)-1)<=elememl1[[lm]][1])&(col(distC)>=elememl1[[lm]][2])];
			NumericMatrix distC2a(ndata,ndata);
			std::fill(distC2a.begin(),distC2a.end(),NumericVector::get_na());
	
			for(int i=0;i<ndata;i++){
				for(int j=el2-1;j<ndata;j++)
				{			
					if(i+j+1<=el1)
						distC2a(i,j)=distC(i,j);
					else 
						break;
				}	
			}
		  
		  //eml2a=-PC[-ndata,]+lambda*distC2a[-1,];		 
		  //NumericMatrix eml2a=-1*rcpp_matrix_delrow(PC,ndata)+lambda*rcpp_matrix_delrow(distC2a,1);
		  NumericMatrix eml2a=rcpp_matrix_plus(-1*rcpp_matrix_delrow(PC,ndata),lambda*rcpp_matrix_delrow(distC2a,1));
		  
          //eml2amax=max(eml2a,na.rm=T);
          //elememl2a=which(eml2a==eml2amax,arr.ind=T)[1,];
          //elememl2a=c(sum(elememl2a)-1,elememl2a[2]);
		  
		  RObject o_eml2amax= r_max(eml2a,_["na.rm"]=true);
		  double eml2amax=as<double>(o_eml2amax);
		  List ls2a=r_which(eml2a==eml2amax,_["arr.ind"]=true);
		  int max_idx2a=as<int>(ls2a[0]);
		 
		  int t_row2a=(max_idx2a-1)%(ndata-1)+1;//
		  int t_col2a=(max_idx2a-1)/(ndata-1)+1;//		 

          double eml1maxtempa=-eml2amax+eml1max;

          if(eml1maxtempa>eml1maxtemp){
            eml1maxtemp=eml1maxtempa;
			
            //elememl1temp[[lm]]=c(elememl2a[2],elememl1[[lm]][2]);
			elememl1temp[(lm-1)*2]=t_col2a;
			elememl1temp[(lm-1)*2+1]=elememl1[(lm-1)*2+1];
			
            //elememl1temp[[modtemp+1]]=c(elememl1[[lm]][1],elememl2a[1]);
			elememl1temp[modtemp*2]=elememl1[(lm-1)*2];
			elememl1temp[modtemp*2+1]=t_row2a+t_col2a-1;
			
          }

        }
		}

     
       
      //add one new interval

      //distC2b=distC;
		
      //for(lm in 1:modtemp){
      //  distC2b[((row(distC)+col(distC)-1)>=elememl1[[lm]][2])&(col(distC)<=elememl1[[lm]][1])]=NA;
      //}
		NumericMatrix distC2b=clone(distC);	
		
		for(int lm=1;lm<=modtemp;lm++){
			int el1=elememl1[(lm-1)*2],el2=elememl1[(lm-1)*2+1];
			
			for(int i=0;i<ndata;i++){
				for(int j=0;j<ndata;j++)
				{	
					if((i+j+1>=el2)&&j<el1)distC2b(i,j)=NA_REAL;
				}	
			}
		}	


      //if(sum(!is.na(distC2b))>0){
		if(na_omit(distC2b).length()>0){

        //eml2b=PC-lambda*distC2b;
		//NumericMatrix eml2b=PC-lambda*distC2b;
		NumericMatrix eml2b=rcpp_matrix_plus(PC,-1*lambda*distC2b);
		
        //eml2bmax=max(eml2b,na.rm=T);
        //elememl2b=which(eml2b==eml2bmax,arr.ind=T)[1,];
        //elememl2b=c(sum(elememl2b)-1,elememl2b[2]);
		
		RObject o_eml2bmax= r_max(eml2b,_["na.rm"]=true);
		double eml2bmax=as<double>(o_eml2bmax);
		List ls2b=r_which(eml2b==eml2bmax,_["arr.ind"]=true);
		int max_idx2b=as<int>(ls2b[0]);
		 
		int t_row2b=(max_idx2b-1)%ndata+1;//
		int t_col2b=(max_idx2b-1)/ndata+1;//	

        //eml1maxtempb=eml2bmax+eml1max;
		double eml1maxtempb=eml2bmax+eml1max;

        // if(eml1maxtempb>eml1maxtemp){
          // eml1maxtemp=eml1maxtempb;
          // elememl1temp=elememl1;
          // elememl1temp[[modtemp+1]]=elememl2b;
        // }
		
		if(eml1maxtempb>eml1maxtemp){
            eml1maxtemp=eml1maxtempb;
			elememl1temp=elememl1;           
			
            //elememl1temp[[modtemp+1]]=elememl2b;
			elememl1temp[modtemp*2]=t_row2b+t_col2b-1;
			elememl1temp[modtemp*2+1]=t_col2b;
			
          }

      }

      elememl1=elememl1temp;
      //diffem=eml1maxtemp-eml1max;
	  diffem_v[lit]=eml1maxtemp-eml1max;
      eml1max=eml1maxtemp;

    }	
		}

    return diffem_v;


  }

