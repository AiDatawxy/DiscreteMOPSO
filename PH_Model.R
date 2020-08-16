source("PH_Model_function.R")

f <- function(rIds, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax, 
    particle_number, iretation, max_number, w_min, w_max, phi1, phi2) {

    plot_data <- data.frame(number = rep(0, max_number))
    col_names <- vector()
    col_names[1] <- "number"
 
    ### 粒子位置初始化 ###
    location <- list()
    current_particle_number = 0
    while(current_particle_number < particle_number) {
        point <- vector()
        for(j in 1:rCnt) {
            point[j] <- sample(2, 1) - 1
        }
        if(constraint_function(rIds, point, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)) {
            current_particle_number <- current_particle_number + 1
            location[[current_particle_number]] <- point
        }
    }

    ### 粒子速度初始化 ###
    velocity <- list()
    for(i in 1:particle_number) {
        point <- vector()
        for(j in 1:rCnt) {
            point[[j]] <- runif(1, -1, 1)
        }
        velocity[[i]] <- point
    }
    
    ### 初始粒子最优位置 ###
    particle_best <- location
    
    ### 初始非劣集 ###
    not_worse <- not_worse_function(particle_best, rIds, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)
    
    ### 迭代过程 ###
    for(round in 1:iretation + 1) {
        # 更新粒子最优位置
        particle_best <- particle_best_function(particle_best, location, rIds, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)
        # 更新非劣集
        not_worse_new <- not_worse_function(particle_best, rIds, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)
        not_worse <- not_worse_from_two_function(not_worse, not_worse_new, rIds, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)
        # 计算非劣点的概率信息
        probability_information <- probability_function(not_worse)
        # 如果非劣集超额，则将其压缩，并获取压缩后的概率信息
        if(length(not_worse) > max_number) {
            not_worse <- compress_not_worse_function(not_worse, max_number)
            probability_information <- probability_function(not_worse)
        }
        # 更新全局最优位置
        global_best <- global_best_function(not_worse, probability_information)
        # 计算本论的w
        w = w_max - (w_max - w_min) * round / iretation
        # 更新粒子群速度
        velocity <- velocity_update_function(velocity, location,
                                           particle_best, global_best, w, rCnt, phi1, phi2)
        # 更新粒子群位置
        location <- location_update_function(velocity, location, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)
    }
    

    ### 结果处理 ###
    # 从最终非劣集中获取全局最优策略、值
    final_global_best_index = 1
    final_global_best_value = target_function(rIds, not_worse[[1]], rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs)
    final_global_best_sum =
        final_global_best_value[[1]] + final_global_best_value[[2]]
    for(i in 1:length(not_worse)) {
        current_value = target_function(rIds, not_worse[[i]], rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs)
        current_sum = current_value[[1]] + current_value[[2]]
        if(current_sum < final_global_best_sum) {
            final_global_best_sum = current_sum
            final_global_best_index = i
        }
    }
    final_global_best <- not_worse[[final_global_best_index]]
    final_global_best_value <- target_function(rIds, final_global_best, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs)
    # 保存全局最优策略
    xxx <- c(final_global_best, rep(0, max_number - length(final_global_best)))
    col_name <- "action"
    col_names <- c(col_names, col_name)
    plot_data$new_col = xxx
    names(plot_data) <- col_names
    # 保存全局最优函数值1 
    xxx <- c(final_global_best_value[[1]],
           rep(0, max_number - length(final_global_best_value[[1]])))
    col_name <- "result_v"
    col_names <- c(col_names, col_name)
    plot_data$new_col = xxx
    names(plot_data) <- col_names
    # 保存全局最优函数值2 
    xxx <- c(final_global_best_value[[2]],
           rep(0, max_number - length(final_global_best_value[[2]])))
    col_name <- "result_c"
    col_names <- c(col_names, col_name)
    plot_data$new_col = xxx
    names(plot_data) <- col_names
    # 获取最终非劣集的损失和成本
    not_worse_lose <- vector()
    not_worse_cost <- vector()
    for(i in 1:length(not_worse)) {
        point_value = target_function(rIds, not_worse[[i]], rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs)
        not_worse_lose[i] = point_value[[1]]
        not_worse_cost[i] = point_value[[2]]
    }
    # 保存非劣集损失
    xxx <- c(not_worse_lose,
           rep(0, max_number - length(not_worse_lose)))
    col_name <-  "lose"
    col_names <- c(col_names, col_name)
    plot_data$new_col = xxx
    names(plot_data) <- col_names
    # 保存非劣集成本
    xxx <- c(not_worse_cost,
           rep(0, max_number - length(not_worse_cost)))
    col_name <- "cost"
    col_names <- c(col_names, col_name)
    plot_data$new_col = xxx
    names(plot_data) <- col_names


    return(plot_data)
}


