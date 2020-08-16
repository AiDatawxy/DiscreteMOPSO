
### target functions ###
target_function<-function(x,type){
    # minimize loss
    result_1=0
    # minimize cost  
    result=list(result_1,result_2)
    return(result)
}


### whether the particle satisfy restrictions ###
constraint_function<-function(x,type){
    flag=TRUE
    result=target_function(x,type)
    if(result[[1]]>f1_max){
        flag=FALSE
    }
    else if(result[[2]]>f2_max){
        flag=FALSE
    }
    return(flag)
}


### whether particla A is 'worse' than particle B ###
A_worse_than_B_function<-function(A,B,type){
    result_A<-target_function(A,type)
    result_B<-target_function(B,type)
    # assume A is not worse than B
    result=0
    if((result_A[[1]]>=result_B[[1]]) && (result_A[[2]]>=result_B[[2]])){
        # A is worse than B
        result=1
    }
    return(result)
}

### best location for single particle ###
particle_best_function<-function(particle_best,location,type){
    for(i in 1:length(location)){
        if(A_worse_than_B_function(location[[i]],particle_best[[i]],type)==0){
            if(sample(2,1)==2){
                particle_best[[i]]<-location[[i]]
            }
        }
    }
    return(particle_best)
}


### select target particles from particle set ###
not_worse_function<-function(particle_set,type){
    not_worse<-list()
    index=0
    for(particle_A in particle_set){
        A_score=0
        for(particle_B in particle_set){
            if(sum(particle_A==particle_B)<dimension){
                score=A_worse_than_B_function(particle_A,particle_B,type)
                A_score=A_score+score
                if(score==1){
                    break
                }
            }
        }
        if(A_score==0){
            index=index+1
            not_worse[[index]]=particle_A
        }
    }
    return(not_worse)
}


### merge two target particle set to one ###
not_worse_from_two_function<-function(set_A,set_B,type){
    not_worse<-list()
    index=0
    # compare particle in set A with set B
    # 先拿A中的粒子同B比较
    for(particle_A in set_A){
        A_score=0
        for(particle_B in set_B){
            score=A_worse_than_B_function(particle_A,particle_B,type)
            A_score=A_score+score
            if(score==1){
                break
            }
        }
        if(A_score==0){
            index=index+1
            not_worse[[index]]=particle_A
        }
    }
    # compare particle in set B with set 'not_worse'
    for(particle_B in set_B){
        B_score=0
        for(particle in not_worse){
            score=A_worse_than_B_function(particle_B,particle,type)
            B_score=B_score+score
            if(score==1){
                break
            }
        }
        if(B_score==0){
            index=index+1
            not_worse[[index]]=particle_B
        }
    }
    
    return(not_worse)
}


### calculate probabliity according to density ###
probability_function<-function(not_worse){
    n=length(not_worse)
    probability_information<-vector()
    coordinate_number<-list()
    index=1
    coordinate_number_key_value=list(not_worse[[1]],0)
    coordinate_number[[index]]=coordinate_number_key_value
    for(i in 1:n){
        point=not_worse[[i]]
        add=TRUE
        for(j in 1:length(coordinate_number)){
            if(sum(point!=coordinate_number[[j]][[1]])==0){
                coordinate_number[[j]][[2]]=coordinate_number[[j]][[2]]+1
                add=FALSE
                break
            }
        }
        if(add){
            index=index+1
            coordinate_number_key_value=list(point,1)
            coordinate_number[[index]]=coordinate_number_key_value
        }
    }
    coordinate_nubmer_probability<-vector()
    for(i in 1:length(coordinate_number)){
        coordinate_nubmer_probability[i]=
            1/(length(coordinate_number)*coordinate_number[[i]][[2]])
    }
    for(i in 1:n){
        point=not_worse[[i]]
        for(j in 1:length(coordinate_number)){
            if(sum(point!=coordinate_number[[j]][[1]])==0){
                probability_information[i]=coordinate_nubmer_probability[j]
                break
            }
        }
    }
    return(probability_information)
}


###  compress set 'not_worse' if its size is over the threshold ###
compress_not_worse_function<-function(not_worse){
    index=sample(length(not_worse),max_number,replace=F)
    not_worse=not_worse[index]
    return(not_worse)
}


### best global location ###
global_best_function<-function(not_worse,probability_information){
    index=sample(length(not_worse),1,prob=probability_information)
    return(not_worse[[index]])
}


### update particles' velocity ###
velocity_update_function<-function(velocity,location,
                                   particle_best,global_best,w){
    for(i in 1:length(velocity)){
        velocity_point=velocity[[i]]
        location_point=location[[i]]
        particle_best_point=particle_best[[i]]
        for(j in 1:dimension){
            velocity_point[j]=w*velocity_point[j]+
                phi1*runif(1)*(particle_best_point[j]-location_point[j])+
                phi2*runif(1)*(global_best[j]-location_point[j])
        }
        velocity[[i]]=velocity_point
    }
    
    return(velocity)    
}


### update particles' location ###
location_update_function<-function(velocity,location,type){
    for(i in 1:length(location)){
        point<-vector()
        for(j in 1:dimension){
            v=velocity[[i]][j]
            logit=1/(1+exp(-v))
            point[j]=ifelse(runif(1)<logit,1,0)
        }
        if(constraint_function(point,type)){
            location[[i]]=point
        }
    }
    return(location)
}
