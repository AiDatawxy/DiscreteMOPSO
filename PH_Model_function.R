###################### 函数 ##################################################
# rIds:             风险源编号向量
# x:                决策向量
# rCnt:             风险源个数
# rVentures:        风险损失
# rCosts:           控制成本
# rProbabilities:   发生概率
# ex:               是否考虑权重放大
# rWeightis:        权重
# re:               是否考虑相关性
# RrPairCnt         相关风险对对数
# RrIs:             风险对的风险相关性
# RrIdSmalls:       风险对中小风险编号
# RrIdBigs:         风险对中大风险编号


### 目标向量值函数 ###
target_function <- function(rIds, x, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs) {
    ########## 基础参数 ##########
    # 损失想要最小
    result_v = 0
    # 成本想要最小
    result_c = 0

    ########## 权重 ##########
    w <- vector()
    for(i in 1 : rCnt) {
        if(ex){
            w[i] <- rWeights[i]
        }
        else {
            w[i] <- 0
        }
    }
    
    ########## 损失与成本 ##########
    for(i in 1 : rCnt) {
        result_v = result_v +
            exp(w[i]) * (1 - x[i]) * rProbabilities[i] * rVentures[i]
        result_c = result_c + x[i] * rCosts[i]
    }

    ########## 相关性 ##########
    if(re) {
        for(i in 1 : RrPairCnt) {
            Lij <- rVentures[RrIdSmalls[i]] + rVentures[RrIdBigs[i]] + rCnt * 0.5 * RrIs[i] * (
                -abs(rVentures[RrIdSmalls[i]] - rVentures[RrIdBigs[i]]) )


            if((x[RrIdSmalls[i]] == 0) && (x[RrIdBigs[i]] == 0)) {
                result_v = result_v + rProbabilities[RrIdSmalls[i]] * rProbabilities[RrIdBigs[i]] * (
                    (exp(w[RrIdSmalls[i]]) + exp(w[RrIdBigs[i]])) * 0.5 * Lij - rVentures[RrIdSmalls[i]] * exp(w[RrIdSmalls[i]]) - rVentures[RrIdBigs[i]] * exp(w[RrIdBigs[i]]) )

            }
        }

    }

    result = list(result_v, result_c)
    return (result)

}

### 判断某粒子是否满足约束条件的函数 ###
constraint_function <- function(rIds, x, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax) {
    flag = TRUE
    result = target_function(rIds, x, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs)
    if(result[[1]] > ventureMax) {
        flag = FALSE
    }
    else if(result[[2]] > costMax) {
        flag = FALSE
    }
    return(flag)
}

### 判断A粒子是否比B粒子劣的函数 ###
A_worse_than_B_function <- function(rIds, A, B, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax) {
    result_A <- target_function(rIds, A, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs)
    result_B <- target_function(rIds, B, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs)
    # 假设A不比B劣
    result = 0
    if( ((result_A[[1]] > result_B[[1]]) && (result_A[[2]] >= result_B[[2]])) ||
        ((result_A[[1]] >= result_B[[1]]) && (result_A[[2]] > result_B[[2]])) ) {
        # A确实比B劣
        result = 1
    }
    return(result)
}

### 单个粒子最优位置的函数 ###
particle_best_function <- function(particle_best, location, rIds, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax) {
    for(i in 1:length(location)) {
        if(A_worse_than_B_function(rIds, location[[i]], particle_best[[i]], rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax) == 0) {
            if(sample(2,1) == 2) {
                particle_best[[i]] <- location[[i]]
            }
        }
    }
    return(particle_best)
}

### 从一个粒子集中抽出非劣集 ###
not_worse_function <- function(particle_set, rIds, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax) {
    not_worse <- list()
    index = 0
    for(particle_A in particle_set) {
        A_score = 0
        for(particle_B in particle_set) {
            if(sum(particle_A == particle_B) < rCnt) {
                score = A_worse_than_B_function(rIds, particle_A, particle_B, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)
                A_score = A_score + score
                if(score == 1) {
                    break
                }
            }
        }
        if(A_score == 0) {
            index = index + 1
            not_worse[[index]] = particle_A
        }
    }
    return(not_worse)
}

### 从两个非劣集中形成一个非劣集 ###
not_worse_from_two_function <- function(set_A, set_B, rIds, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax) {
    not_worse <- list()
    index = 0
    # 先拿A中的粒子同B比较
    for(particle_A in set_A) {
        A_score = 0
        for(particle_B in set_B) {
            score = A_worse_than_B_function(rIds, particle_A, particle_B, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)
            A_score = A_score + score
            if(score == 1) {
                break
            }
        }
        if(A_score == 0) {
            index = index + 1
            not_worse[[index]] = particle_A
        }
    }
    # 再拿B中的粒子同当前not_worse比较
    for(particle_B in set_B){
        B_score = 0
        for(particle in not_worse){
            score = A_worse_than_B_function(rIds, particle_B, particle, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)
            B_score = B_score + score
            if(score == 1) {
                break
            }
        }
        if(B_score == 0) {
            index = index + 1
            not_worse[[index]] = particle_B
        }
    }

    return(not_worse)
}

### 根据密度计算各点概率 ###
probability_function <- function(not_worse) {
    # 非劣点数目
    n = length(not_worse)
    # 非劣点的概率
    probability_information <- vector()
    # 存储{坐标-数量}键值对的列表
    coordinate_number <- list()
    index = 1
    coordinate_number_key_value = list(not_worse[[1]], 0)
    coordinate_number[[index]] = coordinate_number_key_value
    for(i in 1:n) {
        point = not_worse[[i]]
        add = TRUE
        for(j in 1:length(coordinate_number)) {
            if(sum(point != coordinate_number[[j]][[1]]) == 0) {
                coordinate_number[[j]][[2]] = coordinate_number[[j]][[2]] + 1
                add = FALSE
                break
            }
        }
        if(add) {
            index = index + 1
            coordinate_number_key_value = list(point, 1)
            coordinate_number[[index]] = coordinate_number_key_value
        }
    }
    # 设取到每个有点坐标是等概率的，有点坐标中每个点等概率
    coordinate_nubmer_probability <- vector()
    for(i in 1:length(coordinate_number)) {
        coordinate_nubmer_probability[i] =
            1 / (length(coordinate_number) * coordinate_number[[i]][[2]])
    }
    # 计算每个非劣点的概率
    for(i in 1:n) {
        point = not_worse[[i]]
        for(j in 1:length(coordinate_number)) {
            if(sum(point != coordinate_number[[j]][[1]]) == 0) {
                probability_information[i] = coordinate_nubmer_probability[j]
                break
            }
        }
    }
    return(probability_information)
}

### 将超额非劣集压缩 ###
compress_not_worse_function<-function(not_worse, max_number) {
    index = sample(length(not_worse), max_number, replace = F)
    not_worse = not_worse[index]
    return(not_worse)
}

### 全局最优位置的函数 ###
# 从非劣点集中不等概率抽一个作为全局最优
global_best_function <- function(not_worse, probability_information) {
    index = sample(length(not_worse), 1, prob = probability_information)    
    return(not_worse[[index]])
}

### 更新粒子群速度的函数 ###
velocity_update_function <- function(velocity, location,
                                   particle_best, global_best, w, rCnt, phi1, phi2) {
    for(i in 1:length(velocity)) {
        velocity_point = velocity[[i]]
        location_point = location[[i]]
        particle_best_point = particle_best[[i]]
        for(j in 1:rCnt) {
            velocity_point[j] = w * velocity_point[j] +
                phi1 * runif(1) * (particle_best_point[j] - location_point[j]) +
                phi2 * runif(1) * (global_best[j] - location_point[j])
        }
        velocity[[i]] = velocity_point
    }

    return(velocity)
}

### 更新粒子群位置的函数 ###
location_update_function <- function(velocity, location, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax) {
    for(i in 1:length(location)) {
        point <- vector()
        for(j in 1:rCnt){
            v = velocity[[i]][j]
            logit = 1 / (1 + exp(-v))
            point[j] = ifelse(runif(1) < logit, 1, 0)
        }
        if(constraint_function(rIds, point, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax)) {
            location[[i]] = point
        }
    }
    return(location)
}
