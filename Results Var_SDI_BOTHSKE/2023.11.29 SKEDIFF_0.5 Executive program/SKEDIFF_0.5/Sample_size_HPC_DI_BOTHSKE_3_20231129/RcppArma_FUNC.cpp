#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::Mat<double> rcpp_arma_add(arma::Mat<double> m1, arma::Mat<double> m2,double lambda){		
   
	
	return m2.head_rows(m2.n_rows-1)*lambda-m1.tail_rows(m1.n_rows-1);
}

//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double rcpp_arma_fill(List L){		
    NumericVector v=L[0];
	
	return v[0];
}


  
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::Mat<double> arma_diffem_N(const arma::Mat<double>& PC,const arma::Mat<double>& distC,int ndata,arma::vec lambdas,int mod0){
    
	//PC, distC, ndata, 
	//List elememl1;

    arma::vec diffem_v(lambdas.n_elem);
	
	for(int lit=0;lit<lambdas.n_elem;lit++){
	
	double lambda=lambdas[lit];
	//double diffem=0;
	 
    //Excess mass for one mode

    //NumericMatrix eml1=PC-(distC*lambda);	
	arma::Mat<double> eml1=PC-distC*lambda;
	
	
	
    //eml1max=max(eml1,na.rm=T);
    //elememl1[[1]]=which(eml1==eml1max,arr.ind=T)[1,];
	//Function r_max("max");  
    //Function r_which("which");   	
	
	//RObject o_eml1max= r_max(eml1,_["na.rm"]=true);
	//double eml1max=as<double>(o_eml1max);
	//List ls=r_which(eml1==eml1max,_["arr.ind"]=true);
	double eml1max=eml1.max();
	int max_idx=eml1.index_max();
	int t_row=(max_idx-1)%ndata+1;
	int t_col=(max_idx-1)/ndata+1;
	
    //elememl1[[1]]=c(sum(elememl1[[1]])-1,elememl1[[1]][2]);
	arma::vec elememl1((mod0+1)*2);
	elememl1[0]=t_row+t_col-1;
	elememl1[1]=t_col;
	
	
    ////eml1maxtemp=eml1max
    //elememl1temp=elememl1;
    arma::vec elememl1temp=elememl1;

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
			arma::Mat<double> distC2a(ndata,ndata);
			distC2a.fill(NumericVector::get_na());
	
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
		  arma::Mat<double>  eml2a=distC2a.tail_rows(distC2a.n_rows-1)*lambda-PC.head_rows(PC.n_rows-1);
		  
          //eml2amax=max(eml2a,na.rm=T);
          //elememl2a=which(eml2a==eml2amax,arr.ind=T)[1,];
          //elememl2a=c(sum(elememl2a)-1,elememl2a[2]);
		  
		  //RObject o_eml2amax= r_max(eml2a,_["na.rm"]=true);
		  //double eml2amax=as<double>(o_eml2amax);
		  //List ls2a=r_which(eml2a==eml2amax,_["arr.ind"]=true);
		  double eml2amax=eml2a.max();
		  int max_idx2a=eml2a.index_max();
		 
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
	  
		arma::Mat<double> distC2b=distC;	
		
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
		arma::vec v_finite=distC2b.elem(find_finite(distC2b));
		if(v_finite.n_elem>0){

        //eml2b=PC-lambda*distC2b;
		//NumericMatrix eml2b=PC-lambda*distC2b;
		arma::Mat<double> eml2b=PC-lambda*distC2b;
		
        //eml2bmax=max(eml2b,na.rm=T);
        //elememl2b=which(eml2b==eml2bmax,arr.ind=T)[1,];
        //elememl2b=c(sum(elememl2b)-1,elememl2b[2]);
		
		//RObject o_eml2bmax= r_max(eml2b,_["na.rm"]=true);
		//double eml2bmax=as<double>(o_eml2bmax);
		//List ls2b=r_which(eml2b==eml2bmax,_["arr.ind"]=true);
		double eml2bmax=eml2b.max();
		int max_idx2b=eml2b.index_max();
		 
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


//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
List rcpp_arma_minimos(List L_minimos,const arma::Mat<double>& m_aord, int mod0, int ndata, List& iniint,  List& finint, double large){
			
	List minimos=clone(L_minimos);
  
    List iniint2;
    List finint2;
  
	  for(int k=1;k<=mod0;k++){

      //minimos[[k+1]]=minimos[[k]][(1:(length(minimos[[k]])-1))]
	   NumericVector minimos_k_0=minimos[k-1];
	   int vlen=minimos_k_0.length()-1;
	   NumericVector vec_k(vlen);
	   for(int j=0;j<vlen;j++)
	   {
		   vec_k[j]=minimos_k_0[j];
	   }
	   minimos.push_back(vec_k);
	   NumericVector minimos_k=minimos[k];
	  
	   iniint2=List::create();
       finint2=List::create();
	
      //for(l3 in 1:k){
	  for(int l3=1;l3<=k;l3++){
        //iniint2[[l3]]=iniint[[l3]][(1:(length(minimos[[k]])-1))]
		//finint2[[l3]]=finint[[l3]][(1:(length(minimos[[k]])-1))]
		NumericVector vec_i=iniint[l3-1];
		NumericVector vec_f=finint[l3-1];
		NumericVector vec_i2(vlen);
		NumericVector vec_f2(vlen);
		for(int j=0;j<vlen;j++)
	    {
		   vec_i2[j]=vec_i[j];
		   vec_f2[j]=vec_f[j];
	    }
		
		iniint2.push_back(vec_i2);
		finint2.push_back(vec_f2);
		        
      }
	  
	  
      //iniint2[[k+1]]=rep(-1,(length(minimos[[k]])-1))
      //finint2[[k+1]]=rep(ndata+1,(length(minimos[[k]])-1))
	  NumericVector vec_i2_k1(vlen);
      NumericVector vec_f2_k1(vlen);
	  vec_i2_k1.fill(-1);
	  vec_f2_k1.fill(ndata+1);  
	  iniint2.push_back(vec_i2_k1);
	  finint2.push_back(vec_f2_k1);



      //for(l in 1:(length(minimos[[k]]))){
		for(int l=1; l<= minimos_k_0.length(); l++){

        //#when we add one interval (outside) ->Bord
        //Bord=matrix(large,nrow=(ndata-1),ncol=(ndata-1))
		NumericMatrix Bord(ndata-1,ndata-1);
		Bord.fill(large);
		
        LogicalMatrix elemB(ndata-1,ndata-1);
		elemB.fill(1);

        //#when we quit one interval (inside) ->Cord
        //Cord=matrix(-1,nrow=(ndata-1),ncol=(ndata-1))
		NumericMatrix Cord(ndata-1,ndata-1);
		Cord.fill(-1);
     
	    //NumericVector minimos_k_0=minimos[k-1];//here
		
		//if(l<=(length(minimos[[k]])-2)){
		bool islLE= (l<=(minimos_k_0.length()-2));//here
		
		//Cord=arma_Cord(as<arma::Mat<double>>(wrap(Cord)),m_aord,elemB, iniint, finint, ndata-1 ,k,l, islLE);
		int ndata_1=ndata-1;
		
		LogicalMatrix elemBtemp(ndata_1,ndata_1);
		//NumericMatrix r_m_cord=as<NumericMatrix>(wrap(m_cord));
		
		for(int l2=0;l2<k; l2++){			
			NumericVector vec_iniint=iniint[l2];
			NumericVector vec_finint=finint[l2];
		
			  if(islLE==true){
				if(vec_iniint[l-1]>1){				 
					  for(int i=0;i<ndata_1;i++){
						  for(int j=0;j<ndata_1;j++){
							  elemBtemp(i,j)= ((i+j+2)<vec_iniint[l-1]) & (j+1<vec_iniint[l-1]);
						  }
					  }
				}else{				 
				  elemBtemp.fill(0);
				}
				
				if(vec_finint[l-1]<(ndata_1)){//here				
				  for(int i=0;i<ndata_1;i++){
						  for(int j=0;j<ndata_1;j++){
							  elemBtemp(i,j)= elemBtemp(i,j) | ((i+j+2)<=ndata_1+1) & (j+1>vec_finint[l-1]);
						  }
					  }
				}
				
				if(vec_iniint[l-1]>0){					 
					  for(int i=0;i<ndata_1;i++){
						  for(int j=0;j<ndata_1;j++){
							  elemB(i,j)=elemB(i,j)& elemBtemp(i,j);
						  }
					  }
				  }
			  }
			  
			  if(l>1){
				if(vec_iniint[l-1]>0){//here				   
					for(int i=0;i<ndata_1;i++){
						for(int j=0;j<ndata_1;j++){
							if((j+1>=vec_iniint[l-1]) & (j+1<vec_finint[l-1]) & (i+j+2<=vec_finint[l-1]))
								Cord(i,j)=m_aord(i,j);
						}
					}
				  
				  }
				  
			  }			  
			
			  
		}	

		
        //if(l<=(length(minimos[[k]])-2)){
		if(islLE==true){
			
          //Bord[elemB]=Aord[elemB]
		  for(int j=0;j<ndata-1;j++)
		  {
			  for(int j2=0;j2<ndata-1;j2++)
			  {
				  if(elemB(j,j2)==true)Bord(j,j2)=m_aord(j,j2);
			  }
		  }	
		  
          //elemmin=apply(Bord,MARGIN=1,which.min)
		  //posmin=apply(Bord,MARGIN=1,min)+minimos[[k]][l]	
		  
		  NumericVector elemmin(0);
		  NumericVector posmin(0);
		  for(int j=0;j<Bord.nrow();j++){
			  int minpos=which_min(Bord.row(j));
			  elemmin.push_back(minpos+1);//add 1
			  
			  double minval=min(Bord.row(j))+minimos_k_0[l-1];//here
			  posmin.push_back(minval);
			  
		  }
		  
		  
		  //valmenor=posmin[1:(ndata-l-1-k)]<minimos[[k+1]][(l+1):length(minimos[[k+1]])]
		  //valmenor=(1:(ndata-l-1-k))[valmenor]		
		  
		  LogicalVector valmenor(0);
		  //for(int j=0;j<minimos_k.length()-l;j++)
		  for(int j=0;j<=ndata-l-2-k;j++)
		  {
			  bool bVal=posmin[j]<minimos_k[l+j];
			  if(bVal==true){
				  valmenor.push_back(j+1);//add 1
			  }
		  }	
          
         
          //if(length(valmenor)>0){
			if(valmenor.length()>0){			 		   
			  
            //minimos[[k+1]][(valmenor+l)]=posmin[valmenor]
			for(int j=0;j<valmenor.length();j++)
			{
				minimos_k[valmenor[j]+l-1]=posmin[valmenor[j]-1];//here
			}
			
			
            //for(l3 in 1:k){
			for(int l3=1;l3<=k;l3++){
			   NumericVector vec_iniint=iniint[l3-1];
			   NumericVector vec_finint=finint[l3-1];
			   NumericVector vec_iniint2=iniint2[l3-1];
			   NumericVector vec_finint2=finint2[l3-1];			   
			   
              //iniint2[[l3]][(valmenor+l)]=iniint[[l3]][(l)]
              //finint2[[l3]][(valmenor+l)]=finint[[l3]][(l)]
			  for(int j=0;j<valmenor.length();j++){
				  vec_iniint2[valmenor[j]+l-1]=vec_iniint[l-1];//here
				  vec_finint2[valmenor[j]+l-1]=vec_finint[l-1];//here				  
			  }
			  
            }
			
            //iniint2[[k+1]][(valmenor+l)]=elemmin[valmenor]
            //finint2[[k+1]][(valmenor+l)]=elemmin[valmenor]+valmenor
			 NumericVector vec_iniint2_k=iniint2[k];
			 NumericVector vec_finint2_k=finint2[k];
			  for(int j=0;j<valmenor.length();j++){
				  vec_iniint2_k[valmenor[j]+l-1]=elemmin[valmenor[j]-1];//here
				  vec_finint2_k[valmenor[j]+l-1]=elemmin[valmenor[j]-1]+valmenor[j];//here
				  
			  }
			
          }

		  
        }		
	

        //if(l>1){
		  if(l>1){
			  
          //elemmax=apply(Cord,MARGIN=1,which.max)
          //posmax=-apply(Cord,MARGIN=1,max)+minimos[[k]][l]		
		  
		  NumericVector elemmax(0);
		  NumericVector posmax(0);
		  for(int j=0;j<Cord.nrow();j++){
			  int maxpos=which_max(Cord.row(j));
			  elemmax.push_back(maxpos+1);//add 1
			  
			  double maxval=minimos_k_0[l-1]-max(Cord.row(j));//here
			  posmax.push_back(maxval);
			  
		  }   
	 

          //valmayor=posmax[(l-1):1]<minimos[[k+1]][1:(l-1)]
		  //valmayor=(1:(l-1))[valmayor]
		  //poselemmax=elemmax[(l-1):1]
		
		  LogicalVector valmayor(0);
		  
		  NumericVector poselemmax(0);		
		  
		  for(int j=0;j<l-1;j++)
		  {			
			  
			  bool bVal=posmax[l-1-j-1]<minimos_k[j];//here
			  if(bVal==true){
				  valmayor.push_back(j+1);//add 1				 
			  }
			  
			  poselemmax.push_back(elemmax[l-1-j-1]);//here
		  }				
	    
		  
          //if(length(valmayor)>0){
			if(valmayor.length()>0){
				
		     NumericVector vec_iniint2_k=iniint2[k];//k+1
			 NumericVector vec_finint2_k=finint2[k];
			  
            //minimos[[k+1]][(valmayor)]=posmax[l-valmayor]
			for(int j=0;j<valmayor.length();j++)
			{
				
				minimos_k[valmayor[j]-1]=posmax[l-valmayor[j]-1];//
			}		
			
            //for(l4 in 1:length(valmayor)){
			for(int l4=1;l4<=valmayor.length();l4++){
				
              //for(l3 in 1:k){
			  for(int l3=1;l3<=k;l3++){
				NumericVector vec_iniint=iniint[l3-1];
			    NumericVector vec_finint=finint[l3-1];
				NumericVector vec_iniint2=iniint2[l3-1];
				NumericVector vec_finint2=finint2[l3-1];	
				  
                //if((iniint[[l3]][l]>0)&(poselemmax[valmayor[l4]]>=iniint[[l3]][l])&((poselemmax[valmayor[l4]]+l-valmayor[l4])<=finint[[l3]][l])){
                if(vec_iniint[l-1]>0 && (poselemmax[valmayor[l4-1]-1]>=vec_iniint[l-1]) && ((poselemmax[valmayor[l4-1]-1]+l-valmayor[l4-1])<=vec_finint[l-1])){  
				  
				  //iniint2[[l3]][valmayor[l4]]=iniint[[l3]][l]
                  //finint2[[l3]][valmayor[l4]]=poselemmax[valmayor[l4]]
                  //iniint2[[k+1]][valmayor[l4]]=(poselemmax[valmayor[l4]]+l-valmayor[l4])
                  //finint2[[k+1]][valmayor[l4]]=finint[[l3]][l]				  
				
				  vec_iniint2[valmayor[l4-1]-1]=vec_iniint[l-1];
				  vec_finint2[valmayor[l4-1]-1]=poselemmax[valmayor[l4-1]-1];
				  vec_iniint2_k[valmayor[l4-1]-1]=poselemmax[valmayor[l4-1]-1]+l-valmayor[l4-1];//here
				  vec_finint2_k[valmayor[l4-1]-1]=vec_finint[l-1];				
				  

                  //if(iniint2[[l3]][valmayor[l4]]==finint2[[l3]][valmayor[l4]]){
				  if(vec_iniint2[valmayor[l4-1]-1]==vec_finint2[valmayor[l4-1]-1]){					  
                    //iniint2[[l3]][valmayor[l4]]=-1
                    //finint2[[l3]][valmayor[l4]]=ndata+1
					vec_iniint2[valmayor[l4-1]-1]=-1;
					vec_finint2[valmayor[l4-1]-1]=ndata+1;
                  }

                  //if(iniint2[[k+1]][valmayor[l4]]==finint2[[k+1]][valmayor[l4]]){
				  if(vec_iniint2_k[valmayor[l4-1]-1]==vec_finint2_k[valmayor[l4-1]-1]){
                    //iniint2[[k+1]][valmayor[l4]]=-1
                    //finint2[[k+1]][valmayor[l4]]=ndata+1
					vec_iniint2_k[valmayor[l4-1]-1]=-1;
					vec_finint2_k[valmayor[l4-1]-1]=ndata+1;
                  }

                }else{
					
                  //iniint2[[l3]][valmayor[l4]]=iniint[[l3]][(l)]
                  //finint2[[l3]][valmayor[l4]]=finint[[l3]][(l)]
				  vec_iniint2[valmayor[l4-1]-1]=vec_iniint[l-1];
				  vec_finint2[valmayor[l4-1]-1]=vec_finint[l-1];
                }
              }
            }
          }

        }		

      }
  
	  iniint=iniint2;
      finint=finint2;

    }//#the end of for(k in 1:mod0)		

		
	return minimos;

}

