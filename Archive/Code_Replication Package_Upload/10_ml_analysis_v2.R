# ML analysis
rm(list=ls(all=TRUE))

vec.pac= c("haven", "foreign", "quantreg", "gbm", "glmnet",
           "MASS", "rpart", "doParallel", "sandwich", "randomForest",
           "nnet", "matrixStats", "xtable", "readstata13", "lfe", "doParallel", "car",
           "caret", "foreach", "multcomp", "cowplot", "data.table", "dplyr", "ggplot2")

for (i in vec.pac){
  if(! i %in% rownames(installed.packages())){
    install.packages(i, dependencies = TRUE)
  }
}

lapply(vec.pac, require, character.only = TRUE)

# Fix function-name conflicts.
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
arrange <- dplyr::arrange
rename <- dplyr::rename
transmute <- dplyr::transmute
summarise <- dplyr::summarise
count <- dplyr::count
distinct <- dplyr::distinct
left_join <- dplyr::left_join
inner_join <- dplyr::inner_join
full_join <- dplyr::full_join
bind_rows <- dplyr::bind_rows
case_when <- dplyr::case_when
desc <- dplyr::desc
all_of <- tidyselect::all_of


dir.create("ML", showWarnings = FALSE, recursive = TRUE)
dir.create("ML/Tables", showWarnings = FALSE, recursive = TRUE)
dir.create("ML/Figures", showWarnings = FALSE, recursive = TRUE)
dir.create("ML/RData", showWarnings = FALSE, recursive = TRUE)


ptm <- proc.time()

set.seed(1211);

cl   <- makeCluster(24, outfile="")
registerDoParallel(cl)

# Load data
data <- haven::read_dta("data/panel_individual.dta")
data <- as.data.frame(data)

data$row_id <- seq_len(nrow(data))
data_original <- data

# Outcome and treatment
y_var <- "I_ig"
d_var <- "HighCCEI"

# Fixed effect and cluster.
cluster_var      <- "class"
fixed_effect_var <- "class"

check_required_vars <- function(df, vars, label = "required variables"){
  missing_vars <- setdiff(vars, colnames(df))
  if(length(missing_vars) > 0){
    stop(
      paste0(
        "Missing ", label, ": ",
        paste(missing_vars, collapse = ", ")
      )
    )
  }
}

# Basic variables.
data$class <- as.factor(data$class)
data[[d_var]] <- as.numeric(data[[d_var]])
data[[y_var]] <- as.numeric(data[[y_var]])

# Male-pair dummies; malepair_00 is the omitted base.
data$male_i <- as.numeric(data$male_i)
data$male_j <- as.numeric(data$male_j)

data$malepair_00 <- as.numeric(data$male_i == 0 & data$male_j == 0)
data$malepair_01 <- as.numeric(data$male_i == 0 & data$male_j == 1)
data$malepair_10 <- as.numeric(data$male_i == 1 & data$male_j == 0)
data$malepair_11 <- as.numeric(data$male_i == 1 & data$male_j == 1)

# Friendship dummies; friendship_none is the omitted base.
data$friendship <- as.numeric(data$friendship)

data$friendship_none   <- as.numeric(data$friendship == 0)
data$friendship_oneway <- as.numeric(data$friendship == 1)
data$friendship_mutual <- as.numeric(data$friendship == 2)


# Inputs
sim     <- 250       # change to 250 later
K       <- 2       # number of folds
p       <- 5       # number of groups
thres   <- 0.2     # quantile for most/least clan_vars group
beta    <- 0.5
alpha   <- 0.05
p_score <- 1       # known propensity score

names   <- c("I_ig")
Y       <- c(y_var)
D       <- rep(d_var, length(Y))

cluster      <- cluster_var
fixed_effect <- fixed_effect_var
partition    <- "0"


# Controls
group1_controls <- c(
  "mathscore_i",
  "mathscore_diff",
  "outgoing_i",
  "outgoing_diff",
  "opened_i",
  "opened_diff",
  "agreeable_i",
  "agreeable_diff",
  "conscientious_i",
  "conscientious_diff",
  "stable_i",
  "stable_diff"
)

# Group 2.
group2_numeric_controls <- c(
  "height_i",
  "height_diff",
  "inclass_n_friends_i",
  "inclass_n_diff",
  "inclass_popularity_i",
  "inclass_popularity_diff"
)

# Non-base dummy categories.
malepair_controls <- c(
  "malepair_01",
  "malepair_10",
  "malepair_11"
)

friendship_controls <- c(
  "friendship_oneway",
  "friendship_mutual"
)

# Group 3.
group3_stems <- c(
  "pblclass_horizontal",
  "teacher_induce",
  "peer_sociable",
  "peer_fair",
  "peer_helpful",
  "peer_selfish",
  "peer_reciprocal",
  "class_sociable",
  "class_belonged",
  "class_outcast",
  "class_harass"
)

group3_controls <- as.vector(
  rbind(
    paste0(group3_stems, "_i"),
    paste0(group3_stems, "_diff")
  )
)

# Add post.
group3_controls <- c("post", group3_controls)

# Group 4 controls only.
group4_controls <- c(
  "ccei_i",
  "ccei_diff",
  "RA_i",
  "RA_diff"
)

# Missing dummies are already created in panel_individual.
missing_dummy_sources <- character(0)

# Check selected variables.
check_required_vars(
  data,
  unique(c(group1_controls, group2_numeric_controls, group3_controls, group4_controls)),
  "selected numeric controls"
)

check_required_vars(
  data,
  unique(c(malepair_controls, friendship_controls)),
  "selected dummy controls"
)

# Numeric controls.
controls_numeric_raw <- unique(c(
  group1_controls,
  group2_numeric_controls,
  group3_controls,
  group4_controls
))

# Dummy controls.
controls_as_is_raw <- unique(c(
  malepair_controls,
  friendship_controls
))

# Convert numeric controls to numeric.
for(v in controls_numeric_raw){
  data[[v]] <- suppressWarnings(as.numeric(data[[v]]))
}

# Convert dummy controls to numeric.
for(v in controls_as_is_raw){
  data[[v]] <- suppressWarnings(as.numeric(data[[v]]))
}

cat("\nSelected Group 1 controls:\n")
print(group1_controls)

cat("\nSelected Group 2 numeric controls:\n")
print(group2_numeric_controls)

cat("\nSelected malepair dummy controls, base group omitted = malepair_00:\n")
print(malepair_controls)

cat("\nSelected friendship dummy controls, base group omitted = friendship_none:\n")
print(friendship_controls)

cat("\nSelected Group 3 controls:\n")
print(group3_controls)

cat("\nSelected Group 4 controls, controls only, not main interest:\n")
print(group4_controls)

cat("\nExisting missing dummy variables will be loaded after controls are finalized.\n")

cat("\nAll selected controls are present in data.\n")

# Missing dummies
missing_dummy_sources <- setdiff(controls_numeric_raw, "post")
missing_vars <- paste0(missing_dummy_sources, "_missing")
missing_vars <- missing_vars[missing_vars %in% colnames(data)]

for(v in missing_vars){
  data[[v]] <- as.numeric(data[[v]])
}

controls <- c(
  controls_numeric_raw,
  controls_as_is_raw,
  missing_vars
)

controls <- unique(controls)

cat("\nExisting missing dummy controls used in ML:\n")
print(missing_vars)


# CLAN variables
clan_vars <- c(
  group1_controls,
  group2_numeric_controls,
  malepair_controls,
  friendship_controls,
  group3_controls
)

clan_vars <- unique(clan_vars)
clan_vars <- clan_vars[clan_vars %in% colnames(data)]

names_clan_vars <- clan_vars


# Weights
external_weights <- "0"


# Formulas
X <- ""

for(i in 1:length(controls)){
  X <- paste(X, controls[i], "+", sep = "")
}

X  <- substr(X, 1, nchar(X)-1)
XL <- paste("(", X , ")^2", sep="")


# Keep analysis variables
key_variables <- c(Y, D, controls)

if (fixed_effect != "0") {
  key_variables <- c(key_variables, fixed_effect)
}

if (cluster != "0") {
  key_variables <- c(key_variables, cluster)
}

if (external_weights != "0") {
  key_variables <- c(key_variables, external_weights)
}

key_variables <- unique(c(key_variables, "row_id"))

data <- data %>% dplyr::select(all_of(key_variables))


# Collinearity diagnostics
cat("\n--------------------------------------------\n")
cat("Collinearity diagnostics before ML estimation\n")
cat("--------------------------------------------\n")

# 1) Check zero-variance variables among numeric controls
numeric_controls_for_check <- controls[sapply(data[, controls, drop = FALSE], is.numeric)]

zero_var_controls <- numeric_controls_for_check[
  sapply(data[, numeric_controls_for_check, drop = FALSE], function(x){
    length(unique(na.omit(x))) <= 1
  })
]

cat("\nZero-variance numeric controls:\n")
print(zero_var_controls)

# 2) Check perfectly correlated numeric controls
if(length(numeric_controls_for_check) >= 2){
  
  cor_mat <- suppressWarnings(
    cor(data[, numeric_controls_for_check, drop = FALSE], use = "pairwise.complete.obs")
  )
  
  perfect_cor_pairs <- which(
    abs(cor_mat) == 1 & upper.tri(cor_mat),
    arr.ind = TRUE
  )
  
  cat("\nPerfectly correlated numeric control pairs:\n")
  
  if(nrow(perfect_cor_pairs) > 0){
    perfect_cor_table <- data.frame(
      var1 = rownames(cor_mat)[perfect_cor_pairs[,1]],
      var2 = colnames(cor_mat)[perfect_cor_pairs[,2]],
      corr = cor_mat[perfect_cor_pairs]
    )
    print(perfect_cor_table)
  } else {
    print("None")
  }
}

# 3) Check exact linear combinations in the actual model matrix
form_check <- as.formula(paste("~", X))
mm_check <- model.matrix(form_check, data = data)

if(ncol(mm_check) > 1){
  mm_check_no_intercept <- mm_check[, colnames(mm_check) != "(Intercept)", drop = FALSE]
  
  linear_combo <- caret::findLinearCombos(mm_check_no_intercept)
  
  cat("\nLinear-combination columns in model.matrix(~ X):\n")
  
  if(!is.null(linear_combo$linearCombos)){
    
    for(j in 1:length(linear_combo$linearCombos)){
      cat("\nLinear combo", j, ":\n")
      print(colnames(mm_check_no_intercept)[linear_combo$linearCombos[[j]]])
    }
    
    cat("\nSuggested columns to remove by caret::findLinearCombos:\n")
    print(colnames(mm_check_no_intercept)[linear_combo$remove])
    
  } else {
    print("None")
  }
}

cat("\n--------------------------------------------\n")
cat("End collinearity diagnostics\n")
cat("--------------------------------------------\n")

# Drop collinear controls
numeric_controls_for_check <- controls[sapply(data[, controls, drop = FALSE], is.numeric)]

zero_var_controls <- numeric_controls_for_check[
  sapply(data[, numeric_controls_for_check, drop = FALSE], function(x){
    length(unique(na.omit(x))) <= 1
  })
]

form_check <- as.formula(paste("~", X))
mm_check <- model.matrix(form_check, data = data)

linear_combo_remove <- c()

if(ncol(mm_check) > 1){
  
  mm_check_no_intercept <- mm_check[, colnames(mm_check) != "(Intercept)", drop = FALSE]
  
  linear_combo <- caret::findLinearCombos(mm_check_no_intercept)
  
  if(!is.null(linear_combo$remove)){
    linear_combo_remove <- colnames(mm_check_no_intercept)[linear_combo$remove]
  }
}

# Only remove variables that are actual controls.
remove_controls <- unique(c(
  zero_var_controls,
  intersect(linear_combo_remove, controls)
))

cat("\n--------------------------------------------\n")
cat("Controls removed because of zero variance or perfect collinearity\n")
cat("--------------------------------------------\n")
print(remove_controls)

controls <- setdiff(controls, remove_controls)
clan_vars <- setdiff(clan_vars, remove_controls)
names_clan_vars <- clan_vars
removed_collinear_controls <- remove_controls

# Rebuild formula after removing collinear controls
X <- ""

for(i in 1:length(controls)){
  X <- paste(X, controls[i], "+", sep = "")
}

X  <- substr(X, 1, nchar(X)-1)
XL <- paste("(", X , ")^2", sep="")

# Rebuild key_variables after removing collinear controls
key_variables <- c(Y, D, controls)

if (fixed_effect != "0") {
  key_variables <- c(key_variables, fixed_effect)
}

if (cluster != "0") {
  key_variables <- c(key_variables, cluster)
}

if (external_weights != "0") {
  key_variables <- c(key_variables, external_weights)
}

key_variables <- unique(c(key_variables, "row_id"))

data <- data %>% dplyr::select(all_of(key_variables))
data <- as.data.frame(data)

cat("\nRemaining number of controls:\n")
print(length(controls))

cat("\nRemaining controls:\n")
print(controls)








# ML inputs
methods  <- c("glmnet", "gbm", "pcaNNet", "rf")
method_names <- c("Elastic Net", "Boosting", "Nnet", "Random Forest")

args         <- list(svmLinear2=list(type='eps-regression'),
                     svmLinear=list(type='nu-svr'),
                     svmPoly=list(type='nu-svr'),
                     gbm=list(verbose=FALSE),
                     rf=list(ntree=1000),
                     gamboost=list(baselearner='btree'),
                     avNNet=list(verbose = 0, linout = TRUE, trace = FALSE),
                     pcaNNet=list(linout = TRUE, trace = FALSE, MaxNWts=100000, maxit=10000),
                     nnet=list(linout = TRUE, trace = FALSE, MaxNWts=100000, maxit=10000))

output_name <- paste("range", "-", "best", "-", 2, "-", 2, "-", sim, sep = "")
name        <- "EL1"

methodML   <- c("repeatedcv", "repeatedcv", "repeatedcv", "none")
tune       <- c(500, 100, 100, NA)
proces     <- c("range", "range", "range", "range")
select     <- c("best", "best", "best", NA)
cv         <- c(2, 2, 2, 2)
rep        <- c(2, 2, 2, NA)

tune_param <- list(0)
tune_param[[1]] <- NULL
tune_param[[2]] <- NULL
tune_param[[3]] <- NULL
tune_param[[4]] <- data.frame(mtry = 5)

# Estimation
r <- foreach(t = 1:sim, .combine='cbind', .inorder=FALSE, .packages=vec.pac) %dopar% {
  
  set.seed(t);
  
  results       <- matrix(NA,5*length(Y), length(methods))
  results_het   <- matrix(NA,5*length(Y), length(methods))
  results_test  <- matrix(NA,15*length(Y), length(methods))
  results_group <- matrix(NA,15*length(Y), length(methods))
  table.who     <- matrix(NA, (length(clan_vars)*15)*length(Y), length(methods))
  bestML        <- matrix(NA, 2*length(Y), length(methods))
  
  if(partition!="0"){
    ind <- createDataPartition(data[,partition], p = .5, list = FALSE)
    
    datause_raw <- as.data.frame(data[ ind,])
    dataout_raw <- as.data.frame(data[-ind,])
  }
  
  if(partition=="0"){
    split             <- runif(nrow(data))
    cvgroup           <- as.numeric(cut(split,quantile(split,probs = c(0, 2/3,1)),include.lowest = TRUE))
    
    datause_raw       <- as.data.frame(data[cvgroup == 1,])
    dataout_raw       <- as.data.frame(data[cvgroup != 1,])
  }
  
  for(i in 1:length(Y)){
    
    y      <- Y[i]
    d      <- D[i]
    
    cc_vars <- c(controls, y, d, clan_vars)
    if(fixed_effect != "0") { cc_vars <- c(cc_vars, fixed_effect) }
    if(cluster != "0") { cc_vars <- c(cc_vars, cluster) }
    if(external_weights != "0") { cc_vars <- c(cc_vars, external_weights) }
    cc_vars <- unique(cc_vars)
    
    datause   <- data.frame(datause_raw[complete.cases(datause_raw[, cc_vars]),])
    dataout   <- data.frame(dataout_raw[complete.cases(dataout_raw[, cc_vars]),])
    
    ind_u <- which(datause[,d]==1)         # treatment indicator
    
    for(l in 1:length(methods)){
      
      if(methods[l] == "glmnet"){ x <- XL }
      if(methods[l] != "glmnet"){ x <- X }
      
      form           <- as.formula(paste(y,"~",x,sep=""));
      
      # ML scores
      if(p_score==1){  md_x        <- rep((nrow(datause[datause[,d]==1,]) + nrow(dataout[dataout[,d]==1,]))/(nrow(datause) + nrow(dataout)), nrow(dataout))   }
      if(p_score==0){  md_x        <- predict(model_prop, newdata=dataout, type="raw")  }
      
      fitControl   <- trainControl(method = methodML[l], number = cv[l], repeats = rep[l], allowParallel = FALSE, verboseIter=FALSE, search="random", selectionFunction=select[l])
      arg          <- c(list(form=form, data = datause[ind_u,],  method = methods[l],  tuneGrid = tune_param[[l]], trControl = fitControl, preProcess=proces[l], tuneLength=tune[l]), args[[methods[l]]])
      if(is.na(arg$tuneLength)){ arg$tuneLength <- NULL }
      fit.yz1      <- suppressWarnings(do.call(caret::train, arg))
      
      fitControl   <- trainControl(method = methodML[l], number = cv[l], repeats = rep[l], allowParallel = FALSE, verboseIter=FALSE, search="random", selectionFunction=select[l])
      arg          <- c(list(form=form, data = datause[-ind_u,],  method = methods[l], tuneGrid = tune_param[[l]], trControl = fitControl, preProcess=proces[l], tuneLength=tune[l]), args[[methods[l]]])
      if(is.na(arg$tuneLength)){ arg$tuneLength <- NULL }
      fit.yz0      <- suppressWarnings(do.call(caret::train, arg))
      
      B_out        <- predict(fit.yz0, newdata=dataout, type="raw")
      S_out        <- predict(fit.yz1, newdata=dataout, type="raw") -B_out
      B_use        <- predict(fit.yz0, newdata=datause, type="raw")
      S_use        <- predict(fit.yz1, newdata=datause, type="raw") -B_use
      
      ind           <- (md_x>0.01 & md_x<0.99)
      dataout       <- dataout[ind, ]
      B_out         <- B_out[ind]
      S_out         <- S_out[ind]
      md_x          <- md_x[ind]
      
      # GATES
      dataout$S.f   <- S_out
      dataout$G.f   <- B_out+md_x[1]*dataout$S.f;
      dataout$B.f   <- B_out
      dataout$d.til <- dataout[,d] - md_x[1] - mean(dataout[,d] - md_x[1])
      
      for (ii in 1:3){
        
        datause$S.f  <- S_use
        datause$G.f  <- B_use+md_x[1]*datause$S.f;
        datause$B.f  <- B_use
        datause$d.til <- datause[,d] - md_x[1] - mean(datause[,d] - md_x[1])
        
        dataout$label.S <- (dataout[,y]-dataout$G.f)/dataout$d.til- S_out;
        datause$label.S <- (datause[,y]-datause$G.f)/datause$d.til-S_use;
        
        form       <- as.formula(paste("label.S","~",x,sep=""));
        tuneGrid   <- expand.grid(size = c(1,2,3, 4, 5), decay = c(.1))
        fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 2, allowParallel = FALSE, verboseIter=FALSE, search="random", selectionFunction="best")
        arg        <- c(list(form=form, data = datause, method = "nnet", tuneGrid = tuneGrid , trControl = fitControl, linout=TRUE, preProcess="range", tunelength=100, maxit=100, trace=FALSE, MaxNWts=100000))
        fit.yz2    <- suppressWarnings(do.call(caret::train, arg))
        dataout$S.fA <- S_out+predict(fit.yz2, newdata=dataout, type="raw")
      }
      
      B   <- B_out
      S   <- dataout$S.fA
      
      S2        <- S+runif(length(S), 0, 0.00001)
      
      if(external_weights != "0"){
        S_rep <- rep(S2, round(dataout[,external_weights]))
      } else {
        S_rep <- S2
      }
      
      breaks    <- quantile(S_rep, seq(0,1, 0.2),  include.lowest =T)
      
      breaks[1] <- breaks[1] - 0.001
      breaks[6] <- breaks[6] + 0.001
      SG        <- cut(S2, breaks = breaks)
      
      SGX       <- model.matrix(~-1+SG)
      DSG       <- data.frame(as.numeric(I(as.numeric(dataout[,d])-md_x))*SGX)
      
      colnames(DSG) <- c("G1", "G2", "G3", "G4", "G5")
      dataout[,c("B", "S", "G1", "G2", "G3", "G4", "G5", "weight")] <- cbind(B, S, DSG$G1, DSG$G2, DSG$G3, DSG$G4, DSG$G5, as.numeric((1/(md_x*(1-md_x)))))
      
      if(var(dataout$B)==0) {dataout$B <- dataout$B + rnorm(length(dataout$B),  mean=0, sd=0.1) }
      if(var(dataout$S)==0) {dataout$S <- dataout$S + rnorm(length(dataout$S),  mean=0, sd=0.1) }
      
      if(fixed_effect != "0"){
        form1 <- as.formula(paste(y, "~", "B+S+G1+G2+G3+G4+G5 | ", fixed_effect, "| 0 |", cluster, sep=""))
      } else {
        form1 <- as.formula(paste(y, "~", "B+S+G1+G2+G3+G4+G5 | 0 | 0 |", cluster, sep=""))
      }
      
      weight_reg <- dataout$weight
      if(external_weights != "0"){
        weight_reg <- dataout$weight*dataout[,external_weights]
      }
      
      a <- tryCatch({
        a <- felm(form1, data=dataout, weights=weight_reg)
      },error=function(e){
        cat("ERROR :",methods[l], t, i, "\n")
        if(fixed_effect != "0"){
          form1  <- as.formula(paste(y, "~", "G1+G2+G3+G4+G5 | ", fixed_effect, "| 0 |", cluster, sep=""))
        } else {
          form1  <- as.formula(paste(y, "~", "G1+G2+G3+G4+G5 | 0 | 0 |", cluster, sep=""))
        }
        reg    <- felm(form1, data=dataout, weights=weight_reg)
        return(reg)
      }, warning = function(war) {
        cat("WARNING :",methods[l], t, i, "\n")
        if(fixed_effect != "0"){
          form1  <- as.formula(paste(y, "~", "G1+G2+G3+G4+G5 | ", fixed_effect, "| 0 |", cluster, sep=""))
        } else {
          form1  <- as.formula(paste(y, "~", "G1+G2+G3+G4+G5 | 0 | 0 |", cluster, sep=""))
        }
        reg    <- felm(form1, data=dataout, weights=weight_reg)
        return(reg)
      })
      reg   <- a
      
      coef <- (summary(reg)$coefficients['G5',1])
      pval <- (summary(reg)$coefficients['G5',4])
      results_test[(1+(i-1)*15):(5+((i-1)*15)),l]  <- c(coef, confint(reg, 'G5', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)) )
      
      coef <- (summary(reg)$coefficients['G1',1])
      pval <- (summary(reg)$coefficients['G1',4])
      results_test[(6+(i-1)*15):(10+((i-1)*15)),l]  <- c(coef, confint(reg, 'G1', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)) )
      
      test <- glht(reg, linfct = c("G5-G1==0"))
      coef <- (summary(reg)$coefficients['G5',1]) - (summary(reg)$coefficients['G1',1])
      pval <- summary(test)$test$pvalues[1]
      results_test[(11+(i-1)*15):(15+((i-1)*15)),l] <- c((confint(test,level = 1-alpha))$confint[1:3],(as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
      
      mean <- summary(reg)$coef[c('G1','G2','G3','G4','G5'),1]
      sd   <- summary(reg)$coef[c('G1','G2','G3','G4','G5'),2]
      
      crit <- qnorm(1-alpha/(p))
      
      results_group[((i-1)*15+1):((i-1)*15+5),l]   <- sort(mean)
      results_group[((i-1)*15+6):((i-1)*15+10),l]  <- sort(mean +crit*sd)
      results_group[((i-1)*15+11):((i-1)*15+15),l] <- sort(mean -crit*sd)
      
      bestML[(1+(i-1)*2),l]  <- (sum(mean^2)/5)
      
      # BLP
      Sd            <- dataout$S- mean(dataout$S)
      dataout$S_ort <- I((as.numeric(dataout[,d])-md_x)*Sd)
      dataout$d_ort <- I((as.numeric(dataout[,d])-md_x))
      
      if(fixed_effect != "0"){
        form1 <- as.formula(paste(y, "~", "B+S+d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
      } else {
        form1 <- as.formula(paste(y, "~", "B+S+d_ort+S_ort| 0 | 0 |", cluster, sep=""))
      }
      
      a  <- tryCatch({
        a  <- felm(form1, data=dataout, weights=weight_reg)
      },error=function(e){
        cat("ERROR2 :",methods[l], t, i, "\n")
        if(fixed_effect != "0"){
          form1 <- as.formula(paste(y, "~", "d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
        } else {
          form1 <- as.formula(paste(y, "~", "d_ort+S_ort| 0 | 0 |", cluster, sep=""))
        }
        reg   <- felm(form1, data=dataout, weights=weight_reg)
        return(reg)
      }, warning = function(war) {
        cat("WARNING2 :",methods[l], t, i, "\n")
        if(fixed_effect != "0"){
          form1 <- as.formula(paste(y, "~", "d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
        } else {
          form1 <- as.formula(paste(y, "~", "d_ort+S_ort| 0 | 0 |", cluster, sep=""))
        }
        reg   <- felm(form1, data=dataout, weights=weight_reg)
        return(reg)
      })
      reg <- a
      
      coef <- (summary(reg)$coefficients['d_ort',1])
      pval <- (summary(reg)$coefficients['d_ort',4])
      results[(1+(i-1)*5):(i*5),l]      <-c(coef, confint(reg, 'd_ort', level = 1-alpha)[1:2],  (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
      
      coef <- (summary(reg)$coefficients['S_ort',1])
      pval <- (summary(reg)$coefficients['S_ort',4])
      results_het[(1+(i-1)*5):(i*5),l] <- c(coef, confint(reg, 'S_ort', level = 1-alpha)[1:2],  (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
      bestML[(2+(i-1)*2),l]      <- abs(summary(reg)$coefficients['S_ort',1])*sqrt(var(dataout$S))
      
      # CLAN
      high.effect     <- quantile(S_rep, 1-thres);
      low.effect      <- quantile(S_rep, thres);
      
      dataout$h       <- as.numeric(dataout$S>high.effect)
      dataout$l       <- as.numeric(dataout$S<low.effect)
      
      if(var(dataout$h)==0){dataout$h <- as.numeric(runif(length(dataout$h))<0.1)}
      if(var(dataout$l)==0){dataout$l <- as.numeric(runif(length(dataout$l))<0.1)}
      
      for(m in 1:length(clan_vars)){
        a  <- tryCatch({
          form  <- as.formula(paste(clan_vars[m],"~h+l-1", sep=""))
          
          weight_clan <- NULL
          if(external_weights != "0"){
            weight_clan <- dataout[(dataout$h==1)| (dataout$l==1),external_weights]
          }
          if(external_weights != "0"){
            reg   <- lm(form, data=dataout[(dataout$h==1)| (dataout$l==1),] , weights = weight_clan)
          } else {
            reg   <- lm(form, data=dataout[(dataout$h==1)| (dataout$l==1),])
          }
          
          coef  <- reg$coefficients['h'] - reg$coefficients['l']
          test  <- glht(reg, linfct = c("h-l==0"))
          
          coef  <- (summary(reg)$coefficients['h',1])
          pval  <- (summary(reg)$coefficients['h',4])
          res1  <- c(coef, confint(reg, 'h', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
          
          coef  <- (summary(reg)$coefficients['l',1])
          pval  <- (summary(reg)$coefficients['l',4])
          res2  <- c(coef, confint(reg, 'l', level = 1-alpha)[1:2], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
          
          coef  <- (summary(reg)$coefficients['h',1]) - (summary(reg)$coefficients['l',1])
          pval  <- summary(test)$test$pvalues[1]
          res3  <- c((confint(test,level = 1-alpha))$confint[1:3], (as.numeric(coef<0)*(pval/2) + as.numeric(coef>0)*(1-pval/2)),(as.numeric(coef<0)*(1-pval/2) + as.numeric(coef>0)*(pval/2)))
          a     <- c(res1, res2, res3)
          
        },error=function(e){
          cat("ERROR3 :",methods[l], t, i, clan_vars[m], "\n")
          
          if(external_weights != "0"){
            res1  <- c(weighted.mean(dataout[(dataout$h==1),clan_vars[m]],dataout[(dataout$h==1),external_weights]), weighted.mean(dataout[(dataout$h==1),clan_vars[m]],dataout[(dataout$h==1),external_weights]), weighted.mean(dataout[(dataout$h==1),clan_vars[m]], dataout[(dataout$h==1),external_weights]), 0.5, 0.5)
            res2  <- c(weighted.mean(dataout[(dataout$l==1),clan_vars[m]],dataout[(dataout$l==1),external_weights]), weighted.mean(dataout[(dataout$l==1),clan_vars[m]],dataout[(dataout$l==1),external_weights]), weighted.mean(dataout[(dataout$l==1),clan_vars[m]], dataout[(dataout$l==1),external_weights]), 0.5, 0.5)
          } else {
            res1  <- c(mean(dataout[(dataout$h==1),clan_vars[m]], na.rm=TRUE), mean(dataout[(dataout$h==1),clan_vars[m]], na.rm=TRUE), mean(dataout[(dataout$h==1),clan_vars[m]], na.rm=TRUE), 0.5, 0.5)
            res2  <- c(mean(dataout[(dataout$l==1),clan_vars[m]], na.rm=TRUE), mean(dataout[(dataout$l==1),clan_vars[m]], na.rm=TRUE), mean(dataout[(dataout$l==1),clan_vars[m]], na.rm=TRUE), 0.5, 0.5)
          }
          
          res3  <- c((res1[1] - res2[1]), (res1[1] - res2[1]), (res1[1] - res2[1]), 0.5 , 0.5)
          a     <- c(res1, res2, res3)
          return(a)
        })
        table.who[((i-1)*length(clan_vars)*15+(m-1)*15+1):((i-1)*length(clan_vars)*15+(m)*15),l]   <- a
      }
    }
  }
  res <- c(as.vector(results_test), as.vector(results), as.vector(results_het), as.vector(results_group), as.vector(bestML), as.vector(table.who))
  print(t)
  r <- data.frame(res)
}

# Tables and figures
results_test  <- array(c(as.matrix(r[1:(15*length(Y)*length(methods)),])), c(15*length(Y),length(methods), sim))
results       <- array(c(as.matrix(r[((15*length(Y)*length(methods))+1):((15+5)*length(Y)*length(methods)),])), c(5*length(Y),length(methods), sim))
results_het   <- array(c(as.matrix(r[(((20)*length(Y)*length(methods))+1):((20+5)*length(Y)*length(methods)),])), c(5*length(Y),length(methods), sim))
results_group <- array(c(as.matrix(r[(((25)*length(Y)*length(methods))+1):((25+15)*length(Y)*length(methods)),])), c(15*length(Y),length(methods), sim))
bestML        <- array(c(as.matrix(r[(((40)*length(Y)*length(methods))+1):((40+2)*length(Y)*length(methods)),])), c(2*length(Y),length(methods), sim))
table.who     <- array(c(as.matrix(r[(((42)*length(Y)*length(methods))+1):((42+length(clan_vars)*15)*length(Y)*length(methods)),])), c(length(clan_vars)*15*length(Y),length(methods), sim))

median_over_sim <- function(arr) {
  out <- apply(arr, c(1, 2), stats::median, na.rm = TRUE)
  if (is.null(dim(out))) {
    out <- matrix(out, nrow = dim(arr)[1], ncol = dim(arr)[2])
  }
  return(out)
}

quantile_over_sim <- function(arr, probs = c(0.25, 0.75)) {
  n_row <- dim(arr)[1]
  n_method <- dim(arr)[2]
  out <- matrix(NA_real_, nrow = n_row, ncol = n_method * length(probs))
  for (j in seq_len(n_method)) {
    for (i in seq_len(n_row)) {
      q <- stats::quantile(arr[i, j, ], probs = probs, na.rm = TRUE, names = FALSE)
      out[i, ((j - 1) * length(probs) + 1):(j * length(probs))] <- as.numeric(q)
    }
  }
  return(out)
}

results_all       <- median_over_sim(results)
results_het_all   <- median_over_sim(results_het)
test_all          <- median_over_sim(results_test)
table.who_all     <- median_over_sim(table.who)
group_all         <- median_over_sim(results_group)
bestML_all        <- median_over_sim(bestML)
bestML_all_inter  <- quantile_over_sim(bestML)

best1 <- order(-bestML_all[1,])[1:2]
best2 <- order(-bestML_all[2,])[1:2]

if(best1[1] != best2[1]){ best <- c(best1[1], best2[1]) }
if(best1[1] == best2[1]){ best <- c(best2[1], best2[2]) }


# Save heterogeneity score
best_method <- methods[best[1]]
l_best      <- best[1]

if(best_method == "glmnet"){
  x_score <- XL
}
if(best_method != "glmnet"){
  x_score <- X
}

y_score <- Y[1]
d_score <- D[1]

score_vars <- unique(c("row_id", y_score, d_score, controls, clan_vars))
score_df   <- as.data.frame(data[complete.cases(data[, score_vars]), ])

ind_u <- which(score_df[, d_score] == 1)

form_score <- as.formula(paste(y_score, "~", x_score, sep=""))

md_score <- rep(mean(score_df[, d_score] == 1), nrow(score_df))

fitControl <- trainControl(method = methodML[l_best], number = cv[l_best], repeats = rep[l_best],
                           allowParallel = FALSE, verboseIter=FALSE, search="random",
                           selectionFunction=select[l_best])

arg <- c(list(form=form_score, data = score_df[ind_u,], method = best_method,
              tuneGrid = tune_param[[l_best]], trControl = fitControl,
              preProcess=proces[l_best], tuneLength=tune[l_best]), args[[best_method]])
if(is.na(arg$tuneLength)){ arg$tuneLength <- NULL }
fit_score1 <- suppressWarnings(do.call(caret::train, arg))

fitControl <- trainControl(method = methodML[l_best], number = cv[l_best], repeats = rep[l_best],
                           allowParallel = FALSE, verboseIter=FALSE, search="random",
                           selectionFunction=select[l_best])

arg <- c(list(form=form_score, data = score_df[-ind_u,], method = best_method,
              tuneGrid = tune_param[[l_best]], trControl = fitControl,
              preProcess=proces[l_best], tuneLength=tune[l_best]), args[[best_method]])
if(is.na(arg$tuneLength)){ arg$tuneLength <- NULL }
fit_score0 <- suppressWarnings(do.call(caret::train, arg))

B_score <- predict(fit_score0, newdata=score_df, type="raw")
S_score <- predict(fit_score1, newdata=score_df, type="raw") - B_score

ind_score <- (md_score > 0.01 & md_score < 0.99)
score_df  <- score_df[ind_score, ]
B_score   <- B_score[ind_score]
S_score   <- S_score[ind_score]
md_score  <- md_score[ind_score]

score_df$S.f   <- S_score
score_df$G.f   <- B_score + md_score*score_df$S.f
score_df$B.f   <- B_score
score_df$d.til <- score_df[, d_score] - md_score - mean(score_df[, d_score] - md_score)

for(ii in 1:3){
  
  score_df$label.S <- (score_df[, y_score] - score_df$G.f)/score_df$d.til - score_df$S.f
  
  form_label <- as.formula(paste("label.S", "~", x_score, sep=""))
  tuneGrid   <- expand.grid(size = c(1,2,3,4,5), decay = c(.1))
  fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 2,
                             allowParallel = FALSE, verboseIter=FALSE, search="random",
                             selectionFunction="best")
  arg <- c(list(form=form_label, data = score_df, method = "nnet", tuneGrid = tuneGrid,
                trControl = fitControl, linout=TRUE, preProcess="range",
                tunelength=100, maxit=100, trace=FALSE, MaxNWts=100000))
  fit_label <- suppressWarnings(do.call(caret::train, arg))
  
  score_df$S.f <- score_df$S.f + predict(fit_label, newdata=score_df, type="raw")
  score_df$G.f <- score_df$B.f + md_score*score_df$S.f
}

score_df$het_score <- score_df$S.f

data_scored <- data_original %>%
  dplyr::left_join(score_df[, c("row_id", "het_score")], by = "row_id")

save(data_scored, file = "ML/RData/data_with_het_score.RData")


table.who_all2   <- matrix(0,length(clan_vars)*12*length(Y), length(methods))
results_all2     <- matrix(NA,4*length(Y), length(methods))
results_het_all2 <- matrix(NA,4*length(Y), length(methods))
test_all2        <- matrix(NA,12*length(Y), length(methods))
best_ML          <- matrix(NA,4, length(methods))

for(i in 1:length(methods)){
  best_ML[1,i] <- round(bestML_all[1,i],3)
  best_ML[2,i] <- paste("[", round(bestML_all_inter[1,(i-1)*2+1],3),", ", round(bestML_all_inter[1,(i-1)*2+2], 3), "]", sep="")
  
  best_ML[3,i] <- round(bestML_all[2,i],3)
  best_ML[4,i] <- paste("[", round(bestML_all_inter[2,(i-1)*2+1],3),", ", round(bestML_all_inter[2,(i-1)*2+2], 3), "]", sep="")
}


l <- 1

for(i in seq(1, nrow(table.who_all), 5)){
  
  table.who_all2[l:(l+2),]  <- table.who_all[i:(i+2),]
  table.who_all2[l+3,]      <- sapply(seq(1:length(methods)), function(x) min(1,4*min(table.who_all[i+3,x], table.who_all[i+4,x])))
  
  if(l<nrow(results_all2)){
    results_all2[l:(l+2),]       <- results_all[i:(i+2),]
    results_het_all2[l:(l+2),]   <- results_het_all[i:(i+2),]
    
    results_all2[l+3,]     <- sapply(seq(1:length(methods)), function(x) min(1,4*min(results_all[i+3,x], results_all[i+4,x])))
    results_het_all2[l+3,] <- sapply(seq(1:length(methods)), function(x) min(1,4*min(results_het_all[i+3,x], results_het_all[i+4,x])))
  }
  if(l<nrow(test_all2)){
    
    test_all2[l:(l+2),]          <- test_all[i:(i+2),]
    test_all2[l+3,]              <- sapply(seq(1:length(methods)), function(x) min(1,4*min(test_all[i+3,x], test_all[i+4,x])))
  }
  
  l <- l+4
}


results_all     <- round(results_all2, digits = 3)
results_het_all <- round(results_het_all2, digits = 3)
test_all        <- round(test_all2, digits = 3)
bestML_all      <- format(round(bestML_all, pmax(0,4-nchar(floor(abs(bestML_all))))), nsmall= pmax(0,4-nchar(floor(abs(bestML_all)))))
table.who_all   <- round(table.who_all2, digits = 3)

results_test2   <- matrix(0,9*length(Y), length(methods))
result2         <- matrix(0,3*length(Y), length(methods))
result_het2     <- matrix(0,3*length(Y), length(methods))
table.who_all2  <- matrix(0,9*length(Y)*length(clan_vars), length(methods))

seq3 <- seq(1, nrow(table.who_all), 4)
l    <- 1

for(i in seq(1, nrow(table.who_all2), 3)){
  
  k <- seq3[l]
  
  if(i<nrow(result2)){
    result2[i,]       <- format(round(results_all[k,], pmax(0,4-nchar(floor(abs(results_all[k,]))))), nsmall= pmax(0,4-nchar(floor(abs(results_all[k,])))))
    result2[i+1,]     <- sapply(seq(1:ncol(results_all)), function(x) paste("(", format(round(results_all[k+1,x],pmax(0,4-nchar(floor(abs(results_all[k+1,x]))))), nsmall=pmax(0,4-nchar(floor(abs(results_all[k+1,x]))))), ",", format(round(results_all[k+2,x],pmax(0,4-nchar(floor(abs(results_all[k+2,x]))))) , nsmall=pmax(0,4-nchar(floor(abs(results_all[k+2,x]))))), ")", sep=""))
    result2[i+2,]     <- paste("[", format(results_all[k+3,], nsmall = pmax(0,4-nchar(floor(abs(results_all[k+3,]))))), "]", sep="")
    
    result_het2[i,]   <- format(round(results_het_all[k,],max(0,4-nchar(floor(abs(results_het_all[k,]))))) , nsmall=pmax(0,4-nchar(floor(results_het_all[k,]))))
    result_het2[i+1,] <- sapply(seq(1:ncol(results_het_all)), function(x) paste("(", format(round(results_het_all[k+1,x], pmax(0,4-nchar(floor(abs(results_het_all[k+1,x]))))) , nsmall=pmax(0,4-nchar(floor(abs(results_het_all[k+1,x]))))), ",", format(round(results_het_all[k+2,x],pmax(0,4-nchar(floor(abs(results_het_all[k+2,x]))))) , nsmall=pmax(0,4-nchar(floor(abs(results_het_all[k+2,x]))))), ")", sep=""))
    result_het2[i+2,] <- paste("[", format(results_het_all[k+3,], nsmall=max(0,4-nchar(floor(abs(results_het_all[k+3,]))))), "]", sep="")
  }
  
  if(i<nrow(results_test2)){
    results_test2[i,]    <- format(round(test_all[k,],pmax(0,4-nchar(floor(abs(test_all[k,]))))) , nsmall=pmax(0,4-nchar(floor(test_all[k,]))))
    results_test2[i+1,]  <- sapply(seq(1:ncol(test_all)), function(x) paste("(", format(round(test_all[k+1,x], pmax(0,4-nchar(floor(abs(test_all[k+1,x]))))),nsmall=pmax(0,4-nchar(floor(abs(test_all[k+1,x]))))), ",", format(round(test_all[k+2,x],pmax(0,4-nchar(abs(floor(test_all[k+2,x]))))),  nsmall=pmax(0,4-nchar(floor(abs(test_all[k+2,x]))))), ")", sep=""))
    results_test2[i+2,]  <- paste("[", format(test_all[k+3,], nsmall=pmax(0,4-nchar(floor(abs(test_all[k+3,]))))), "]", sep="")
  }
  
  table.who_all2[i,]       <- format(round(table.who_all[k,],pmax(0,4-nchar(floor(abs(table.who_all[k,]))))) ,nsmall=pmax(0,4-nchar(floor(abs(table.who_all[k,])))))
  table.who_all2[i+1,]     <- sapply(seq(1:ncol(table.who_all)), function(x) paste("(", format(round(table.who_all[k+1,x], pmax(0,4-nchar(floor(abs(table.who_all[k+1,x]))))), nsmall=pmax(0,4-nchar(floor(abs(table.who_all[k+1,x]))))), ",", format(round(table.who_all[k+2,x],pmax(0,4-nchar(floor(abs(table.who_all[k+2,x]))))) , nsmall=pmax(0,4-nchar(floor(abs(table.who_all[k+2,x]))))), ")", sep=""))
  if(i%%9==7){  table.who_all2[i+2,]     <- paste("[", format(table.who_all[k+3,], nsmall=pmax(0,4-nchar(floor(abs(table.who_all[k+3,]))))), "]", sep="") }
  if(i%%9!=7){  table.who_all2[i+2,]     <- "-" }
  
  
  l <- l+1
}

CLAN_final         <- matrix(NA, length(Y)*(length(clan_vars)*3+1), length(best)*3)
GATES_final        <- matrix(NA, length(Y)*3, length(best)*3)
BLP_final          <- matrix(NA, length(Y)*3, length(best)*2)
BEST_final         <- bestML_all

rownames_CLAN      <- matrix(NA, nrow(CLAN_final),1)
rownames_GATES     <- matrix(NA, nrow(GATES_final),1)
rownames_BEST      <- matrix(NA, nrow(bestML_all),1)

a  <- 1
b  <- 1
c  <- 1
c2 <- 1

for(l in 1:length(Y)){
  
  rownames_CLAN[a] <- names[l]
  
  a <- a+1
  
  for(i in 1:length(clan_vars)){
    for(j in 1:length(best)){
      
      k <- best[j]
      
      CLAN_final[(a):(a+2),((j-1)*3+1):(j*3)] <- matrix(table.who_all2[(b):(b+8),k], 3, 3)
      
      if(i==1){
        GATES_final[(c):(c+2),((j-1)*3+1):(j*3)] <- matrix(results_test2[(c2):(c2+8),k], 3, 3)
        rownames_GATES[c]   <- names[l]
        BLP_final[(c):(c+2),((j-1)*2+1):(j*2)] <- cbind(result2[(c):(c+2),k], result_het2[(c):(c+2),k])
      }
      
      rownames_CLAN[a]   <- names_clan_vars[i]
    }
    a <- a+3
    b <- b+9
  }
  c  <- c+3
  c2 <- c2+9
  
  rownames_BEST[((l-1)*2+1):((l-1)*2+2)] <- c(names[l], names[l])
  
}

rownames(CLAN_final)   <- rownames_CLAN
rownames(GATES_final)  <- rownames_GATES
rownames(BLP_final)    <- rownames_GATES

colnames(CLAN_final)   <- rep(c("Most Affected", 	"Least Affected",	"Difference"), length(best))
colnames(GATES_final)  <- rep(c("Most Affected", 	"Least Affected",	"Difference"), length(best))
colnames(BLP_final)    <- rep(c("ATE", 	"HET"), length(best))
colnames(best_ML)      <- method_names

print(xtable(cbind(rownames(best_ML),best_ML)), include.rownames=FALSE,file="ML/Tables/Table2.txt", digits=3)
print(xtable(cbind(rownames(BLP_final),BLP_final)), include.rownames=FALSE,file="ML/Tables/Table3.txt", digits=3)
print(xtable(cbind(rownames(GATES_final),GATES_final)), include.rownames=FALSE,file="ML/Tables/Table4-1.txt", digits=3)
print(xtable(cbind(rownames(CLAN_final),CLAN_final)), include.rownames=FALSE,file="ML/Tables/Table5.txt", digits=3)

for(i in 1:length(Y)){
  if(length(methods)>1){
    par(mfrow=c(2,2))
  }
  
  y_range     <- 1*range(group_all[(15*(i-1)+6):(15*(i-1)+10),],group_all[(15*(i-1)+11):(15*(i-1)+15),], na.rm=TRUE)
  y_range2    <- y_range
  y_range2[1] <- y_range[1]- (y_range[2] -  y_range[1])*0.1
  y_range2[2] <- y_range[2]+ (y_range[2] -  y_range[1])*0.1
  
  result=list(0)
  
  for(j in 1:length(methods)){
    
    ATE <- data.frame( x = c(-Inf, Inf), y = as.numeric(results_all[(4*(i-1)+1),j]) , cutoff = factor(50))
    U   <- data.frame( x = c(-Inf, Inf), y = as.numeric(results_all[(4*(i-1)+3),j]) , cutoff = factor(50))
    L   <- data.frame( x = c(-Inf, Inf), y = as.numeric(results_all[(4*(i-1)+2),j]) , cutoff = factor(50))
    
    df <- data.frame(x =1:5,
                     F =as.numeric(group_all[(15*(i-1)+1):(15*(i-1)+5),j]),
                     L =as.numeric(group_all[(15*(i-1)+6):(15*(i-1)+10),j]),
                     U =as.numeric(group_all[(15*(i-1)+11):(15*(i-1)+15),j]),
                     group = factor(c(2, 2, 2, 2,2)))
    
    result[[j]] <- ggplot() +
      theme_gray(base_size = 14) +
      geom_point(data=df,aes(y = F, x = x, colour='GATES'), size = 2) +
      geom_errorbar(data=df, aes(ymax = U, ymin = L ,x=x, y=F, height = .2, width=0.7, colour="Conf. Band (GATES)"), show.legend = TRUE, size = 0.35) +
      geom_line(aes( x, y, linetype = cutoff, colour='ATE' ),ATE, linetype = 2, size = 0.35) +
      geom_line(aes( x, y, linetype = cutoff, colour='Conf. Band (ATE)' ), U, linetype = 2, size =  0.35) +
      geom_line(aes( x, y, linetype = cutoff, colour='Conf. Band (ATE)' ), L, linetype = 2, size =  0.35) +
      scale_colour_manual(values = c("red", "black", "blue", "black"),
                          breaks=c('ATE','Conf. Band (ATE)',"GATES",'Conf. Band (GATES)'),
                          guide = guide_legend(override.aes = list(
                            linetype = c("dashed", "dashed" ,"blank", "solid"),
                            shape = c(NA,NA, 16, NA)), ncol =2,byrow=TRUE)) +  theme_minimal()  +
      theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold"), axis.title=element_text(size=13), legend.text=element_text(size=5),
            legend.key = element_rect(colour = NA, fill = NA), legend.key.size = unit(1, 'lines'),
            legend.title=element_blank(),legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=0.7),legend.box.background = element_blank(), legend.spacing.y = unit(-0.05, 'cm'))  +
      ylim(y_range)  +
      labs(title=method_names[j], y = "Treatment Effect", x = "Group by Het Score")
    
  }
  print(Y[i])
  result[[best[1]]] <- result[[best[1]]] + labs(title=method_names[best[1]], y = "Treatment Effect", x = "Group by Het Score")
  result[[best[2]]] <- result[[best[2]]] + labs(title=method_names[best[2]], y = "", x = "Group by Het Score")
  
  p_best <- plot_grid(result[[best[1]]], result[[best[2]]], ncol=2)
  ggsave("ML/Figures/Figure4.pdf", p_best, height=3.5, width=7)
}

save.image(file = "ML/RData/datas.RData")
