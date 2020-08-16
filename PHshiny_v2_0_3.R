library("shiny")

source("PH_Model.R")
source("PH_Model_function.R")

ui <- fluidPage(
    titlePanel(h1("装配式建筑风险优化控???")),
    
    sidebarLayout(
        sidebarPanel(

            ########## 风险源输??? ##########
            br(),
            h2("风险源输入部份"),
            # 关于小bug的说???
            helpText("首先点击“插入”、“更新”和“删除”按钮各一???"),
            
            # 增加风险???
            h4("增加风险???"),
            numericInput(inputId = "insertRid",
                         label = "风险源编???",
                         min = 1, max = 100, value = 1, step = 1),
            numericInput(inputId = "insertCost",
                         label = "成本???0 - 100 万元",
                         min = 0, max = 100, value = 10, step = 0.01),            
            numericInput(inputId = "insertVenture",
                         label = "损失???0 - 100 万元",
                         min = 0, max = 100, value = 10, step = 0.01),
            numericInput(inputId = "insertProbability",
                         label = "概率???0 - 1",
                         min = 0, max = 1, value = 0, step = 0.001),
            numericInput(inputId = "insertWeight",
                         label = "权重???0 - 1",
                         min = 0, max = 1, value = 0, step = 0.001),
            actionButton("buttonInsertCV", "插入"),           
            br(),
            
            # 更改风险???
            h4("更改风险???"),
            numericInput(inputId = "updateRid",
                         label = "风险源编???",
                         min = 1, max = 100, value = 1, step = 1),
            numericInput(inputId = "updateCost",
                         label = "成本??? 0 - 100 万元",
                         min = 0, max = 100, value = 10, step = 0.01),
            numericInput(inputId = "updateVenture",
                         label = "损失??? 0 - 100 万元",
                         min = 0, max = 100, value = 10, step = 0.01),
            numericInput(inputId = "updateProbability",
                         label = "概率???0 - 1",
                         min = 0, max = 1, value = 0, step = 0.001),
            numericInput(inputId = "updateWeight",
                         label = "权重???0 - 1",
                         min = 0, max = 1, value = 0, step = 0.001),
            actionButton("buttonUpdateCV", "更新"),
            br(),
            
            # 删除风险???            
            h4("删除风险???"),
            numericInput(inputId = "deleteRid",
                         label = "风险源编???",
                         min = 1, max = 100, value = 1, step = 1),
            actionButton("buttonDeleteCV", "删除"),
            br(),

            ########## 相关风险对输??? ##########
            hr(color = "#775543"),
            h2("相关性风险对输入部分"),
            # 关于小bug的说???
            helpText("首先点击“插入风险对”和“删除风险对”按钮各一???"),

            # 增加相关风险???
            h4("添加相关性风险对"),
            numericInput(inputId = "insertRRid1",
                         label = "风险源序???1", min = 0, max = 100, value = 1, step = 1),
            numericInput(inputId = "insertRRid2",
                         label = "风险源序???2", min = 0, max = 100, value = 1, step = 1),
            numericInput(inputId = "insertR", 
                         label = "交互系数", min = -1, max = 1, value = 0, step = 0.01),
            actionButton("buttonInsertR", "插入风险???"),
            br(),

            # 删除相关风险???
            h4("删除相关性风险对"),
            numericInput(inputId = "deleteRRid",
                         label = "风险源序???1/2", min = 0, max = 100, value = 1, step = 1),
            actionButton("buttonDeleteR", "删除风险???")
                        
        ),
        
        mainPanel(
            ########## 选项部分 ##########
            hr(),
            fluidRow(
                column(3, checkboxInput("checkEx", "权重放大效应", FALSE)),
                column(3, checkboxInput("checkRe", "风险相关???", FALSE)),
                column(3, actionButton("buttonRun", "开???")),
                column(3, textOutput("state"))
            ),
            
            ########## 参数部分 ##########
            hr(),
            h2("参数选择部分"),
            # 参数1
            fluidRow(
                column(3, sliderInput("particleCnt", "粒子数目 m", 
                    min = 1, max = 500, step = 1, value = 10)),
                column(3, sliderInput("iretationTimes", "迭代次数 T",
                    min = 0, max = 10000, step = 1, value = 1000)),
                column(3, sliderInput("beCeiling", "非劣解集上限 Ceil",
                    min = 10, max = 1000, step = 1, value = 200))
            ),
    
            # 参数2
            fluidRow(
                column(3, sliderInput("w_min", "最小惯性权??? w_min",
                                       min = 0, max = 1, step = 0.01, value = 0.4)),
                column(3, sliderInput("w_max", "最大惯性权??? w_max",
                                       min = 0, max = 1, step = 0.01, value = 0.9)),
                column(3, sliderInput("phi1", "加速因???1 phi1",
                                       min = 0, max = 10, step = 0.001, value = 1.49618)),
                column(3, sliderInput("phi2", "加速因???2 phi2",
                                       min = 0, max = 10, step = 0.001, value = 1.49618))
            ),  
            
            # 参数3 
            fluidRow(
                column(3, sliderInput("ventureMax", "损失上限: 万元 venture max", 
                                      min = 0, max = 1000, step = 1, value = 100)),
                column(3, sliderInput("costMax", "成本上限: 万元 cost max", 
                                      min = 0, max = 1000, step = 1, value = 100))
            ),
            
            actionButton("buttonParameter", "输入参数"),
            
            # 显示参数
            br(),
            hr(),
            h2("参数???"),
            tableOutput("tableParameter"),
            
            ########## 风险源表部分 #########
            br(),
            hr(),
            h2("风险源表"),
            # 风险源数???
            h5(textOutput(outputId = "count")),
            # 权重之和
            h5(textOutput(outputId = "weightsSum")),
            # 风险源表
            tableOutput(outputId = "CVtable"),

            ########## 相关风险对部??? ##########
            br(),
            hr(),
            h2("相关性风险对???"),
            # 相关风险对表
            tableOutput(outputId = "Rtable"),      


            ########## 结果展示部分 ##########
            br(),
            hr(),
            h2("结果"),
            fluidRow(
                column(3, h3("策略"), tableOutput(outputId = "tableAction")),
                column(3, h3("期望损失与控制成???"), tableOutput(outputId = "tableResult")),
                column(6, h3("非劣解图"), plotOutput(outputId = "plotResult"))
            )

        )
    )
)

server <- function(input, output) {

    #################### 数据部分 ####################
    ##### 风险源的数据 #####
    rCnt = 0
    rWeightsSum = 0
    rIds <- vector()
    rCosts <- vector()
    rVentures <- vector()
    rProbabilities <- vector()
    rWeights <- vector()

    ##### 参数数据 #####
    particle_number = 100
    iretation = 50
    max_number = 200
    w_min = 0.4
    w_max = 0.9
    phi1 = 1.49618
    phi2 = 1.49618
    ventureMax = 100
    costMax = 100
    ex = FALSE
    re = FALSE

    ##### 相关风险对数??? #####
    # 风险集对
    RrPairCnt = 0
    RrIdSmalls <- vector()
    RrIdBigs <- vector()
    RrIs <- vector()
    
    ##### 运行控制部分 #####
    firstTime = TRUE 
    allowCV = FALSE 
    allowR = FALSE
    allowParameter = FALSE 

    ###################  参数表输入与展示 ####################
    eventParameter <- eventReactive(input$buttonParameter, {
        if(is.integer(input$particleCnt) && input$particleCnt >= 1 && input$particleCnt <= 500 && 
            is.integer(input$iretationTimes) && input$iretationTimes >= 10 && input$iretationTimes <= 10000 &&
            is.integer(input$beCeiling) && input$beCeiling >= 10 && input$beCeiling <= 1000 &&
            is.numeric(input$w_min) && input$w_min >= 0 && input$w_min <= 1 && 
            is.numeric(input$w_max) && input$w_max >= 0 && input$w_max <= 1 &&
            is.numeric(input$phi1) && input$phi1 >= 0 && input$phi1 <= 10 && 
            is.numeric(input$phi2) && input$phi2 >= 0 && input$phi2 <= 10 &&
            is.numeric(input$costMax) && input$costMax >= 0 && 
            is.numeric(input$ventureMax) && input$ventureMax >= 0 &&
            allowParameter) {

            particle_number <<- input$particleCnt
            iretation <<- input$iretationTimes
            max_number <<- input$beCeiling
            w_min <<- input$w_min
            w_max <<- input$w_max
            phi1 <<- round(input$phi1, 3)
            phi2 <<- round(input$phi2, 3)
            ventureMax <<- input$ventureMax
            costMax <<- input$costMax
            ex <<- input$checkEx
            re <<- input$checkRe
        }
    })
    
    output$tableParameter <- renderTable(digits = 3, {
        temp <- vector()
        dataset <- data.frame(temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp)
        eventParameter()
        if (allowParameter) {
            eventParameter()
            if (ex) {
                exHere = "???"
            }
            else {
                exHere = "???"
            }
            if(re) {
                reHere = "???"
            }
            else {
                reHere = "???"
            }
            dataset <- data.frame(particle_number, iretation, max_number, w_min, w_max, phi1, phi2, ventureMax, costMax, exHere, reHere)            
        }
        names(dataset) <- c("粒子???", "迭代次数", "非劣集上???", "w_min", "w_max", "phi1", "phi2", "损失上限", "成本上限", "权重放大", "风险相关???")
        return(dataset)
    })


    #################### 风险源表输入与展??? ####################
    # 插入风险???
    eventInsert <- eventReactive(input$buttonInsertCV, {
        if(is.integer(input$insertRid) && input$insertRid >= 1 && input$insertRid <= 100  && sum(input$insertRid == rIds) == 0 && 
            is.numeric(input$insertCost) && input$insertCost >= 0 && input$insertCost <= 100 &&
            is.numeric(input$insertVenture) && input$insertVenture >= 0 && input$insertVenture <= 100 &&
            is.numeric(input$insertProbability) && input$insertProbability >= 0 && input$insertProbability <= 1 &&
            is.numeric(input$insertWeight) && input$insertWeight >= 0 && input$insertWeight <= 1 &&
            allowCV) {
            rCnt <<- rCnt + 1
            rWeightsSum <<- rWeightsSum + input$insertWeight
            rIds[rCnt] <<- as.integer(input$insertRid)
            rCosts[rCnt] <<- as.numeric(input$insertCost)
            rVentures[rCnt] <<- as.numeric(input$insertVenture)
            rProbabilities[rCnt] <<- input$insertProbability
            rWeights[rCnt] <<- input$insertWeight
        }
    })
    
    # 更新风险???
    eventUpdate <- eventReactive(input$buttonUpdateCV, {
        if(is.integer(input$updateRid) && input$updateRid >= 1 && input$updateRid <= 100 && sum(input$updateRid == rIds) > 0 && 
            is.numeric(input$updateCost) && input$updateCost >= 0 && input$updateCost <= 100 && 
            is.numeric(input$updateVenture) && input$updateVenture >= 0 && input$updateVenture <= 100 && 
            is.numeric(input$updateProbability) && input$updateProbability >= 0 && input$updateProbability <= 1 && 
            is.numeric(input$updateWeight) && input$updateWeight >= 0 && input$updateWeight <= 1 &&
            allowCV) {
            rCnt <<- rCnt	
            rWeightsSum <<- rWeightsSum - rWeights[which(rIds == input$updateRid)]
            rWeightsSum <<- rWeightsSum + input$updateWeight
            rCosts[which(rIds == input$updateRid)] <<- input$updateCost
            rVentures[which(rIds == input$updateRid)] <<- input$updateVenture
            rProbabilities[which(rIds == input$updateRid)] <<- round(input$updateProbability, 3)
            rWeights[which(rIds == input$updateRid)] <<- round(input$updateWeight, 3)
        }
    })
    
    # 删除风险???
    eventDelete <- eventReactive(input$buttonDeleteCV, {
        if(is.integer(input$deleteRid) && sum(input$deleteRid == rIds) > 0 &&
            allowCV) {

            # 相关风险对也须调???
            location = max(which(RrIdSmalls == input$deleteRid), which(RrIdBigs == input$deleteRid), -1)
            if (location > 0) {
                RrIdSmalls[location] <<- NA
                RrIdBigs[location] <<- NA 
                RrPairCnt <<- RrPairCnt - 1
                RrIs <<- RrIs[which(! is.na(RrIdSmalls))]
                RrIdSmalls <<- RrIdSmalls[which(! is.na(RrIdSmalls))]
                RrIdBigs <<- RrIdBigs[which(! is.na(RrIdBigs))]
            }

            # 删除本风险源
            rWeightsSum <<- rWeightsSum - rWeights[which(rIds == input$deleteRid)]
            rIds[which(rIds == input$deleteRid)] <<- NA
            rCnt <<- rCnt - 1
            rCosts <<- rCosts[which(! is.na(rIds))]
            rVentures <<- rVentures[which(! is.na(rIds))]
            rProbabilities <<- rProbabilities[which(! is.na(rIds))]
            rWeights <<- rWeights[which(! is.na(rIds))]
            rIds <<- rIds[which(! is.na(rIds))]

        }
    })
    
    output$count <- renderText({
        eventInsert()
        eventUpdate()
        eventDelete()
        paste("风险源数???: ", rCnt, sep = "")
    })
    
    output$weightsSum <- renderText({
        eventInsert()
        eventUpdate()
        eventDelete()
        paste("权重之和: ", as.character(rWeightsSum), sep = "")
    })
    
    output$CVtable <- renderTable(digits = 3, {
        eventInsert()
        eventUpdate()
        eventDelete()
        dataset <- data.frame(rIds, rCosts, rVentures, rProbabilities, rWeights)
        names(dataset) <- c("风险???", "成本", "损失", "概率", "权重")
        return(dataset)
    })
    
    #################### 相关风对表输入与展示 ####################
    # 插入风险???
    eventRInsert <- eventReactive(input$buttonInsertR, {
        if(is.integer(input$insertRRid1) && input$insertRRid1 >= 1 && input$insertRRid1 <= 100 && sum(input$insertRRid1 == rIds) > 0 && sum(input$insertRRid1 == RrIdSmalls) == 0 && sum(input$insertRRid1 == RrIdBigs) == 0 &&
            is.integer(input$insertRRid2) && input$insertRRid2 >= 1 && input$insertRRid2 <= 100 && sum(input$insertRRid2 == rIds) > 0 && sum(input$insertRRid2 == RrIdSmalls) == 0 && sum(input$insertRRid2 == RrIdBigs) == 0 && 
            input$insertRRid1 != input$insertRRid2 &&
            is.numeric(input$insertR) && input$insertR >= -1 && input$insertR <= 1 &&
            allowR) {

            if(input$insertRRid1 < input$insertRRid2) {
                RrIdSmall = input$insertRRid1
                RrIdBig = input$insertRRid2
            }
            else {
                RrIdSmall = input$insertRRid2
                RrIdBig = input$insertRRid1
            }

            RrPairCnt <<- RrPairCnt + 1
            RrIdSmalls[RrPairCnt] <<- RrIdSmall
            RrIdBigs[RrPairCnt] <<- RrIdBig
            RrIs[RrPairCnt] <<- input$insertR
       }
    })
    
    # 删除风险???
    eventRDelete <- eventReactive(input$buttonDeleteR, {
        if(is.integer(input$deleteRRid) && ((sum(input$deleteRRid == RrIdSmalls) > 0) || (sum(input$deleteRRid == RrIdBigs) > 0)) &&
            allowR) {
            location = max(which(RrIdSmalls == input$deleteRRid), which(RrIdBigs == input$deleteRRid), -1)
            RrIdSmalls[location] <<- NA
            RrIdBigs[location] <<- NA 
            RrPairCnt <<- RrPairCnt - 1
            RrIs <<- RrIs[which(! is.na(RrIdSmalls))]
            RrIdSmalls <<- RrIdSmalls[which(! is.na(RrIdSmalls))]
            RrIdBigs <<- RrIdBigs[which(! is.na(RrIdBigs))]
         }

    })
        
    output$Rtable <- renderTable(digits = 3, {
        eventRInsert()
        eventRDelete()
        eventDelete()
        dataset <- data.frame(RrIdSmalls, RrIdBigs, RrIs)
        names(dataset) <- c("风险???1", "风险???2", "交互系数")
        return(dataset)
    })


    #################### 运行及结果部??? ####################
    output$state <- renderText({"状态： ???"})
    eventRun <- eventReactive(input$buttonRun, {
        if(firstTime) {
            allowCV <<- TRUE 
            allowR <<- TRUE 
            allowParameter <<- TRUE 
            output$state <- renderText({"状态： 输入数据"})
            firstTime <<- FALSE
            table = NULL
        }
        else {
            allowCV <<- FALSE 
            allowR <<- FALSE 
            allowParameter <<- FALSE 

            output$state <- renderText({"状态： 运行???"})
            ########## 算法 ##########
            table <- f(rIds, rCnt, rVentures, rCosts, rProbabilities, ex, rWeights, re, 
                RrPairCnt, RrIs, RrIdSmalls, RrIdBigs, ventureMax, costMax, 
                particle_number, iretation, max_number, w_min, w_max, phi1, phi2) 
            output$state <- renderText({"状态：运行结束"})

            allowCV <<- TRUE 
            allowR <<- TRUE 
            allowParameter <<- TRUE 
        }

        return(table)
    })


    output$tableAction <- renderTable({
        dataset <- eventRun()
        if(! is.null(dataset)) {
            action <- dataset$action[seq(1, rCnt)]
            action[which(action == 1)] = "控制"
            action[which(action == 0)] = "不控???"
            dataset <- data.frame(rIds, action)
            names(dataset) <- c("风险???", "策略")            
        }
        return(dataset)
    })

    output$tableResult <- renderTable({
        dataset <- eventRun()
        if(! is.null(dataset)) {
            rowNames <- c("期望损失", "控制成本")
            rowValues <- c(dataset$result_v[1], dataset$result_c[1])
            dataset <- data.frame(rowNames, rowValues)
            names(dataset) <- c("名称", "???(万元)")
        }
        return (dataset)
    })

    output$plotResult <- renderPlot({
        dataset <- eventRun()
        if(! is.null(dataset)) {
            lose <- dataset$lose
            cost <- dataset$cost
            result_v = dataset$result_v[1]
            result_c = dataset$result_c[1]
            plot(x = cost, y = lose, pch = 16, main = "Venture--Cost",
                 xlab = "Cost", ylab = "Venture")
            points(x = result_c, y = result_v, pch = 16, col = "red")
        }
        else {
            return(NULL)
        }

    })

}

shinyApp(ui = ui, server = server)














