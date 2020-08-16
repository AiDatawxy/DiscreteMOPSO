rm(list=ls())
source("DMPSO_Model_function.R")


# parameters
data<-read.csv("DMPSO_Data.csv",header=T)
data
### dimension ###
dimension<-23
### particle numbers ###
particle_number<-100
### iteration numbers ###
iretation<-5000
### 'not_worse' size threshold ###
max_number=200

### params in core equation
w_min<-0.4
w_max<-0.9
phi1<-1.49618
phi2<-1.49618

### static params in model ###
model_w<-data$model_w
model_p<-data$model_p
model_l<-data$model_l
model_c<-data$model_c


# function-1 value threshold
f1_max=100

plot_data<-data.frame(number=rep(0,max_number))
col_names<-vector()
col_names[1]<-"number"

for(type in c("independent","dependent")){
    for(f2_max in c(26, 30, 34, 38)){
        ### initialize partidcles location ###
        location<-list()
        current_particle_number=0
        while(current_particle_number<particle_number){
            point<-vector()
            for(j in 1:dimension){
                point[j]<-sample(2,1)-1
            }
            if(constraint_function(point,type)){
                current_particle_number<-current_particle_number+1
                location[[current_particle_number]]<-point
            }
        }
        
        ### initialize particles' velocity ###
        velocity<-list()
        for(i in 1:particle_number){
            point<-vector()
            for(j in 1:dimension){
                point[[j]]<-runif(1,-1,1)
            }
            velocity[[i]]<-point
        }
        
        ### initialize particles' best location ###
        particle_best<-location
        
        ### initialize 'not_worse' set ###
        not_worse<-not_worse_function(particle_best,type)
        
        ### iterating processs ###
        for(round in 1:iretation+1){
            # update particles' best location
            particle_best<-particle_best_function(particle_best,location,type)
            # update 'not_worse' set
            not_worse_new<-not_worse_function(particle_best,type)
            not_worse<-not_worse_from_two_function(not_worse,not_worse_new,type)
            # calculate probability of 'not_worse'
            probability_information<-probability_function(not_worse)
            # if size of 'not_worse' set is larger than threshold, compress it
            if(length(not_worse)>max_number){
                not_worse<-compress_not_worse_function(not_worse)
                probability_information<-probability_function(not_worse)
            }
            # update best global location
            global_best<-global_best_function(not_worse,probability_information)
            # calculate weight parameter
            w=w_max-(w_max-w_min)*round/iretation
            # update particles' velocity
            velocity<-velocity_update_function(velocity,location,
                                               particle_best,global_best,w)
            # update particles' location
            location<-location_update_function(velocity,location,type)
            
            print(round)
        }
        
        ### handle result ###
        # select best global value and scheme
        final_global_best_index=1
        final_global_best_value=target_function(not_worse[[1]],type)
        final_global_best_sum=
            final_global_best_value[[1]]+final_global_best_value[[2]]
        for(i in 1:length(not_worse)){
            current_value=target_function(not_worse[[i]],type)
            current_sum=current_value[[1]]+current_value[[2]]
            if(current_sum<final_global_best_sum){
                final_global_best_sum=current_sum
                final_global_best_index=i
            }
        }
        final_global_best<-not_worse[[final_global_best_index]]
        final_global_best_value<-target_function(final_global_best,type)
        # save best gloable scheme
        xxx<-c(final_global_best,rep(0,max_number-length(final_global_best)))
        col_name<-paste(type,f2_max,"action",sep="_")
        col_names<-c(col_names,col_name)
        plot_data$new_col=xxx
        names(plot_data)<-col_names
        # save best global function 1 value
        xxx<-c(final_global_best_value[[1]],
               rep(0,max_number-length(final_global_best_value[[1]])))
        col_name<-paste(type,f2_max,"result1",sep="_")
        col_names<-c(col_names,col_name)
        plot_data$new_col=xxx
        names(plot_data)<-col_names
        # save best global function 2 value
        xxx<-c(final_global_best_value[[2]],
               rep(0,max_number-length(final_global_best_value[[2]])))
        col_name<-paste(type,f2_max,"result2",sep="_")
        col_names<-c(col_names,col_name)
        plot_data$new_col=xxx
        names(plot_data)<-col_names
        # calculate total loss and total cost
        not_worse_lose<-vector()
        not_worse_cost<-vector()
        for(i in 1:length(not_worse)){
            point_value=target_function(not_worse[[i]],type)
            not_worse_lose[i]=point_value[[1]]
            not_worse_cost[i]=point_value[[2]]
        }
        # save loss
        xxx<-c(not_worse_lose,
               rep(0,max_number-length(not_worse_lose)))
        col_name<-paste(type,f2_max,"lose",sep="_")
        col_names<-c(col_names,col_name)
        plot_data$new_col=xxx
        names(plot_data)<-col_names
        # save cost
        xxx<-c(not_worse_cost,
               rep(0,max_number-length(not_worse_cost)))
        col_name<-paste(type,f2_max,"cost",sep="_")
        col_names<-c(col_names,col_name)
        plot_data$new_col=xxx
        names(plot_data)<-col_names
    }
}

write.csv(plot_data,"plot_data_1.csv",row.names=F)


